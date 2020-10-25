# coding: utf8

import argparse
import time
import numpy as np
from scipy.integrate import solve_ivp
from scipy.interpolate import InterpolatedUnivariateSpline as Spline


maxiter = 20000
maxiterSCF = 50
nsteps = 10000
rmin = 1e-5
rmax = 20
z = 2  # helium
tol = 1e-5  # does not converge for lower values
alpha = 0.8  # mixing parameter


def flags():
  parser = argparse.ArgumentParser()
  parser.add_argument("-v", "--verbose",
                      help="extended output", action="store_true")
  parser.add_argument(
      "-r", "--random", help="use random density", action="store_true")
  args = parser.parse_args()
  return args


def secant(f, x1=-12345, x2=6789, maxIter=10000, tolVal=1e-10):
  """secant method; x1 and x2 are crucial for finding the desired root"""
  for i in range(maxIter):
    xnew = x2 - (x2 - x1) / (f(x2) - f(x1)) * f(x2)
    if abs(xnew - x2) < tolVal:
      break
    x1 = x2
    x2 = xnew

  else:
    print("Calculation exceeded maximum number of iterations!")
    exit()

  return xnew, i


def trapezoidal(f, a, b, n=10000):
  """trapez method for numerical integration; same result for n = 100000"""
  s = 0.0
  h = (b - a) / n
  for i in range(0, n):
    s += f(a + i * h)

  return h * (s + 0.5 * (f(a) + f(b)))


def rad_seq(t, y, energy, veff):
  """returns radial SEQ as system of two 1st order differential equations"""
  # input: y = [y1, y2]; return y = [y1', y2']
  # y1' = y2; y2' = (...)*y1
  return [y[1], (- 2 * (energy - veff(t))) * y[0]]


def initValues(r):
  """getting initial values for numeric intergration from correct solution"""
  u = 2 * r * np.exp(-r)
  uPrime = (1 - r) * 2 * np.exp(-r)
  return [u, uPrime]


def solve_rad_seq(energy, veff):
  """wrapper for ODE integration; energy and veff as parameter, integration from
  rmax to rmin (inwards)"""
  sol = solve_ivp(
      lambda t, y: rad_seq(t, y, energy, veff),
      t_span=[rmax, rmin],
      t_eval=np.linspace(rmax, rmin, nsteps),
      y0=initValues(rmax))

  u = sol.y[0]
  r = sol.t
  return u[::-1], r[::-1]


def u0(energy, veff):
  """get first value of integrated Schrödinger equation; since the array is
  reversed, u[0] corresponds to the u-value at r = 0 (y-interscetion);
  different energies are passed in by secant method"""
  u, r = solve_rad_seq(energy, veff)
  return u[0]


def normalize(energy, veff):
  """integrating with calculated energy eigenvalue and normalization"""
  u, r = solve_rad_seq(energy, veff)
  u_spline = Spline(r, u * u)
  norm = trapezoidal(u_spline, r[0], r[-1])
  u2 = u * u / norm
  return u2, r, norm


def poisson(t, y, density):
  """returns poisson equation w''(t) = - u²(t) / t = - density(t) / t as system
  of two 1st order differential equations"""
  # input: y = [y1, y2]; return y = [y1', y2']
  # y1' = y2; y2' = - u²(t) / t
  return [y[1], -density(t) / t]


def solve_poisson(density):
  """solve poisson equation; input is u²(t) = density(t) from interpolation"""
  sol = solve_ivp(
      lambda t, y: poisson(t, y, density),
      t_span=[rmin, rmax],
      t_eval=np.linspace(rmin, rmax, nsteps),
      y0=[0, 1])

  return sol.y[0], sol.t


def sic_hartree_energy(density, vh, r):
  """calculate SIC hartree energy"""
  spline = Spline(r, vh(r) * density(r))
  ehartree = 0.5 * trapezoidal(spline, r[0], r[-1], maxiter)
  return ehartree


def iterStepVerbose(density):
  """one iteration in SCF cylce"""

  # STEP 1: build effective KS potential
  print("Building KS potential..", end="", flush=True)
  step1_start_time = time.time()

  # solve poisson equation for initial density
  w, r = solve_poisson(density)

  # adding homogeneous solution, so that boundary conditions are fulfilled
  beta = (z - w[-1]) / r[-1]
  w += beta * r

  # build SIC Hartree potential (factor 0.5)
  vh = Spline(r, 0.5 * w / r)

  step1_exec_time = round((time.time() - step1_start_time), 2)
  print("\t\t\tbeta = {:.5f}\t\t\t({:.2f}s)".format(beta, step1_exec_time))

  #########################################################################
  # STEP 2: solve implicit Schrödinger-like equation
  print("Solving SP-SEQ..", end="", flush=True)
  step2_start_time = time.time()

  # vext = Spline(r, - z / r ) results in convergence and accuracy problems
  # vh(r) returns scalar, unlike 0.5 * w / r which returns array -> needed
  def veff(r):
    vext = - z / r
    return vh(r) + vext

  # find lowest energy eigenvalue; corresponds to energy of hydrogen atom
  energy, nIter = secant(lambda e: solve_rad_seq(e, veff)[0][0], -1.8, -1.7)

  step2_exec_time = round((time.time() - step2_start_time), 2)
  print("\t\t\tE_sp = {: .5f} Eh\t\t({:.2f}s)".format(energy, step2_exec_time))

  #########################################################################
  # STEP 3: generate electronic density (and normalization)
  print("Generating new density..", end="", flush=True)
  step3_start_time = time.time()

  # normalization and generation of electronic density
  density, r, norm = normalize(energy, veff)
  new_density = Spline(r, z * density)

  step3_exec_time = round((time.time() - step3_start_time), 2)
  print("\t\t\t\t\t\t({:.2f}s)".format(step3_exec_time))

  #########################################################################
  # STEP4: accumulate total energy
  print("Calculating total energy..", end="", flush=True)
  step4_start_time = time.time()

  ehartree = sic_hartree_energy(new_density, vh, r)
  etot = z * energy - ehartree

  step4_exec_time = round((time.time() - step4_start_time), 2)
  print("\t\tE_tot = {: .5f} Eh\t\t({:.2f}s)".format(etot, step4_exec_time))

  return new_density, etot


def iterStep(density):
  """one iteration in SCF cylce, with minimal print output"""
  iter_start_time = time.time()
  w, r = solve_poisson(density)
  beta = (z - w[-1]) / r[-1]
  w += beta * r
  vh = Spline(r, 0.5 * w / r)

  def veff(r):
    vext = - z / r
    return vh(r) + vext

  energy, nIter = secant(lambda e: solve_rad_seq(e, veff)[0][0], -1.8, -1.7)

  density, r, norm = normalize(energy, veff)
  new_density = Spline(r, z * density)

  ehartree = sic_hartree_energy(new_density, vh, r)
  etot = z * energy - ehartree

  iter_exec_time = round((time.time() - iter_start_time), 2)
  print("\tE_tot = {: .10f} Eh\t\t({:.2f}s)".format(etot, iter_exec_time))

  return new_density, etot


def main():
  total_start_time = time.time()

  # SCF cylce
  # initializations
  r = np.linspace(rmin, rmax, nsteps)

  def hydrogen_density(r):
    return z ** 3 / np.pi * np.exp(-2 * z * r)

  def random_density(r):
    # range(10) # constant function
    return np.random.random()

  if flags().random:
    init_density = random_density
  else:
    init_density = hydrogen_density

  density = old_density = init_density
  etot_old = 0

  print("Starting SCF cylce for Helium.")
  print("------------------------------")

  if flags().verbose:
    for itr in range(maxiterSCF):
      print("Cycle {} of {}".format(itr+1, maxiterSCF))
      print("--------------")
      new_density, etot = iterStepVerbose(density)

      ediff = etot - etot_old
      print("Change in energy..\t\t\tE_diff = {:.5f} Eh".format(ediff))
      print("")

      if abs(ediff) < tol:
        total_exec_time = round((time.time() - total_start_time), 2)
        print("\nConvergence is achieved! After {} cycles and {:.2f}s.".format(
            itr+1, total_exec_time))
        break

      # n_mix(r) = a * n_new(r) + (1 - a) * n_old(r)
      density_mixed = Spline(r, alpha * new_density(r) +
                             (1 - alpha) * old_density(r))

      # density/energy of this step will be old one for next step
      old_density = new_density
      etot_old = etot

      # new density for next iteration
      density = density_mixed

    else:
      print("Calculation exceeded maximum number of iterations!")
      exit()

  else:
    for itr in range(maxiterSCF):
      print("Iteration {}:".format(itr+1), end="", flush=True)
      new_density, etot = iterStep(density)

      if abs(etot - etot_old) < tol:
        total_exec_time = round((time.time() - total_start_time), 2)
        print("\nConvergence is achieved! After {} cycles and {:.2f}s.".format(
            itr+1, total_exec_time))
        break

      # n_mix(r) = a * n_new(r) + (1 - a) * n_old(r)
      density_mixed = Spline(r, alpha * new_density(r) +
                             (1 - alpha) * old_density(r))

      # density/energy of this step will be old one for next step
      old_density = new_density
      etot_old = etot

      # new density for next iteration
      density = density_mixed

    else:
      print("Calculation exceeded maximum number of iterations!")
      exit()

  print("\n---------------------------------")
  print("Final energy: E(He) = {:.6f} Eh".format(etot))


if __name__ == "__main__":
  main()
