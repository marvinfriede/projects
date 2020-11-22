# coding: utf8

import time
import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp, trapz
from scipy.interpolate import interp1d


nsteps = 10000
rmin = 0.000001
rmax = 20


def secant(f, x1=-12345, x2=6789, maxiter=10000, tol=1e-10):
  """secant method; x1 and x2 are crucial for finding the desired root"""
  for itr in range(maxiter):
    xnew = x2 - (x2 - x1) / (f(x2) - f(x1)) * f(x2)
    if abs(xnew - x2) < tol:
      break

    x1 = x2
    x2 = xnew

  else:
    print("Calculation exceeded maximum number of iterations!")
    exit()

  return xnew, itr


def trapezoidal(f, a, b, n=10000):
  """trapez method for numerical integration"""
  s = 0.0
  h = (b - a) / n
  for i in range(0, n):
    s += f(a + i * h)

  return h * (s + 0.5 * (f(a) + f(b)))


def rad_seq(t, y, energy):
  """returns radial SEQ as system of two 1st order differential equations"""
  # input: y = [y1, y2]; return y = [y1', y2']
  # y1' = y2; y2' = (...)*y1
  return [y[1], (- 2 * (1 / t + energy)) * y[0]]


def initValues(r):
  """getting initial values for numeric intergration from correct solution"""
  u = 2 * r * np.exp(-r)
  uPrime = (1 - r) * 2 * np.exp(-r)
  return [u, uPrime]


def solve_rad_seq(energy):
  """wrapper for ODE integration; energy and l as parameter, integration from
  rmax to rmin (inwards)"""
  sol = solve_ivp(
      lambda t, y: rad_seq(t, y, energy),
      t_span=[rmax, rmin],
      t_eval=np.linspace(rmax, rmin, nsteps),
      y0=initValues(rmax))

  u = sol.y[0]
  r = sol.t
  return u[::-1], r[::-1]


def u0(energy):
  """get first value of integrated Schrödinger equation; since the array is
  reversed, u[0] corresponds to the u-value at r = 0 (y-interscetion); different
  energies are passed in by secant method"""
  u, r = solve_rad_seq(energy)
  return u[0]


def normalize(energy):
  """integrating with calculated energy eigenvalue and normalization"""
  u, r = solve_rad_seq(energy)
  norm = trapz(u * u, r)
  u_norm = u / np.sqrt(norm)
  return u_norm, r, norm


def poisson(t, y, u):
  """returns poisson equation w''(t) = - u²(t) / t as system of two 1st order
  differential equations"""
  # input: y = [y1, y2]; return y = [y1', y2']
  # y1' = y2; y2' = - u²(t) / t
  return [y[1], -u(t) ** 2 / t]


def solve_poisson(f_int):
  """solve radial poisson equation; input is u(r) from interpolation"""
  sol = solve_ivp(
      lambda t, y: poisson(t, y, f_int),
      t_span=[rmin, rmax],
      t_eval=np.linspace(rmin, rmax, nsteps),
      y0=[0, 1])

  return sol.y[0], sol.t


def main():
  # find lowest energy eigenvalue; corresponds to energy of hydrogen atom
  root_start_time = time.time()
  root, numIter = secant(u0, -0.6, -0.55)
  root_exec_time = round((time.time() - root_start_time), 2)
  print("Energy of hydrogen atom: {:.5f} Hartree\t\t ({:.2f}s)".format(
      root, root_exec_time))

  # normalization
  norm_start_time = time.time()
  u_norm, r, norm = normalize(root)
  norm_exec_time = round((time.time() - norm_start_time), 2)
  print(
      "Normalization done ({:.5f} -> 1)\t\t\t ({:.2f}s)".format(norm, norm_exec_time))

  # interpolation of radial SEQ
  u_norm_spline = interp1d(r, u_norm)

  # solving radial poisson equation result is w(r), the single orbital density
  w, r = solve_poisson(u_norm_spline)

  # adding homogeneous solution, so that boundary conditions are fulfilled
  addhom_start_time = time.time()
  qtot = 1
  beta = (qtot - w[-1]) / r[-1]
  w += beta * r
  addhom_exec_time = round((time.time() - addhom_start_time), 2)
  print("Correction for boundary condition: beta = {:.5f}\t ({:.2f}s)".format(
      beta, addhom_exec_time))

  # compute hartree energy: Z/2 * Int(dr v_h(r) u²(r)) with v_h(r) = w(r) / r
  hartree_start_time = time.time()
  e_hartree = 0.5 * trapz(w / r * u_norm * u_norm, r)
  hartree_exec_time = round((time.time() - hartree_start_time), 2)
  print("Hartree energy for hydrogen: E_h = {:.5f}\t\t ({:.2f}s)".format(
      e_hartree, hartree_exec_time))

  # compute hartree energy with own trapezoidal method
  hartree2_start_time = time.time()
  spline = interp1d(r, w / r * u_norm * u_norm)
  eh = 0.5 * trapezoidal(spline, r[0], r[-1])
  hartree2_exec_time = round((time.time() - hartree2_start_time), 2)
  print("Hartree energy for hydrogen: E_h = {:.5f}\t\t ({:.2f}s)".format(
      eh, hartree2_exec_time))

  # plotting numerical solutions
  plt.plot(r, w, "g", lw=2, ls="--", label=r"$w_{\mathrm{num}}(r)$")
  plt.plot(r, w / r, "r", lw=2, ls="--", label=r"$v_{\mathrm{num}}(r)$")
  plt.plot(r, w / r * u_norm ** 2, "b", lw=2, ls="--",
           alpha=0.5, label=r"$v_{\mathrm{num}}(r) \, |u(r)|^2$")

  # plotting exact solutions
  def w_exact(r):
    return - (r + 1) * np.exp(- 2 * r) + 1

  def v_exact(r):
    return w_exact(r) / r

  plt.plot(r, w_exact(r), "g", lw=4, alpha=0.5,
           ls="-", label=r"$w_{\mathrm{exact}}(r)$")
  plt.plot(r, v_exact(r), "r", lw=4, alpha=0.5,
           ls="-", label=r"$v_{\mathrm{exact}}(r)$")

  # plot styling
  plt.xlabel(r"$r$ in Bohr")
  plt.xlim(-0.3, 10)
  plt.axhline(y=0, color="k", ls="--", lw="0.5")
  plt.axvline(x=0, color="k", ls="--", lw="0.5")
  plt.legend(loc="best", fancybox=True, shadow=True)
  # plt.show()


if __name__ == "__main__":
  main()
