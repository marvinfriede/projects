# coding: utf8

import time
import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp, trapz


def secant(f, x1=-12345, x2=6789, maxiter=10000, tol=1e-10):
  '''secant method; x1 and x2 are crucial for finding the desired root'''
  for i in range(maxiter):
    xnew = x2 - (x2 - x1) / (f(x2) - f(x1)) * f(x2)
    if abs(xnew - x2) < tol:
      break

    x1 = x2
    x2 = xnew

  else:
    print("Calculation exceeded maximum number of iterations!")
    exit()

  return xnew


def radSchrodinger(t, y, energy, l):
  '''returns radial SEQ as system of two 1st order differential equations'''
  # input: y = [y1, y2]; return y = [y1', y2']
  # y1' = y2; y2' = (...)*y1
  return [y[1], ((l * (l + 1)) / t ** 2 - 2 * (1 / t + energy)) * y[0]]


def initValues(r):
  '''getting initial values for numeric intergration from correct solution'''
  u = 2 * r * np.exp(-r)
  uPrime = (1 - r) * 2 * np.exp(-r)
  return [u, uPrime]


def wrapper(energy, l, rmax, rmin):
  '''wrapper for ODE integration; energy and l as parameter'''
  sol = solve_ivp(
      lambda t, y: radSchrodinger(t, y, energy, l),
      t_span=[rmax, rmin],
      t_eval=np.linspace(rmax, rmin, 1000),
      y0=initValues(rmax))

  u = sol.y[0]
  r = sol.t
  return u[::-1], r[::-1]


def u0(energy, l=0, rmax=20.0, rmin=0.001):
  '''get first value of integrated Schrödinger equation; since the array is
  reversed, u[0] corresponds to the u-value at r = 0 (y-interscetion);
  different energies are passed in by secant method'''
  u, r = wrapper(energy, l, rmax, rmin)
  return u[0]


def normalize(energy, l=0, rmax=20.0, rmin=0.001):
  '''integrating with calculated energy eigenvalue and normalization'''
  u, r = wrapper(energy, l, rmax, rmin)

  uSquared = u * u
  norm = trapz(uSquared, r)
  uSquared /= norm

  return uSquared, r


def exactSolution(r):
  '''exact solution for radial Schrödinger equation; returns square'''
  u = 2 * r * np.exp(-r)
  return u * u


def main():
  # find lowest energy eigenvalue; corresponds to energy of hydrogen atom
  root_start_time = time.time()
  root = secant(u0, -1.001, -1.0)
  root_exec_time = round((time.time() - root_start_time), 2)
  print("Energy of hydrogen atom: {:.5f} Hartree\t ({:.2f}s)".format(
      root, root_exec_time))

  # calculate u(r=0) for energies in [-1, 0]; for visual solution
  u0_vals = []
  energies = np.linspace(-1, 0, 100)
  for energy in energies:
    u0_vals.append(u0(energy))

  # plot u(r=0) for different energies
  ax1 = plt.subplot(221)
  ax1.plot(energies, u0_vals)
  ax1.axhline(y=0, color="k", ls="--", lw="0.5")
  ax1.set_xlim(-1.01, 0)
  ax1.set_ylabel(r"$u(r=0,\,E)$")
  ax1.set_xlabel(r"$E~\mathrm{in~Hartree}$")

  # plot u(r=0) for different energies; zoom on root
  ax2 = plt.subplot(222)
  ax2.plot(energies, u0_vals)
  ax2.axhline(y=0, color="k", ls="--", lw="0.5")
  ax2.axvline(x=-0.5, color="k", ls="--", lw="0.5")
  ax2.set_ylim(-0.015, 0.02)
  ax2.set_xlim(-0.6, 0)
  ax2.set_ylabel(r"$u(r=0,\,E)$")
  ax2.set_xlabel(r"$E~\mathrm{in~Hartree}$")

  # plot true and numeric solution for l=0
  uSquared, r = normalize(root)
  ax3 = plt.subplot(212)
  ax3.plot(r, uSquared, ls="-", lw="3", label=r"$u^2_\mathrm{num}(r)$")
  ax3.plot(r, exactSolution(r), ls=(0, (5, 1)),
           lw="1.5", label=r"$u^2_\mathrm{exact}(r)$")
  ax3.set_xlim(-0.3, 7)
  ax3.set_ylim(-0.03, 0.6)
  ax3.axhline(y=0, color="k", ls="--", lw="0.5")
  ax3.axvline(x=0, color="k", ls="--", lw="0.5")
  ax3.legend(loc="best", fancybox=True, shadow=True)
  ax3.set_ylabel(r"$u^2(r)$")
  ax3.set_xlabel(r"$r$")

  plt.subplots_adjust(hspace=0.5, wspace=0.5)
  plt.show()

  # calculate energy eigenvalues for l in [0, 5]
  print("\nSpectrum:")
  for l in range(6):
    root_start_time = time.time()

    root = secant(
        lambda energy: u0(energy, l, max(20, 10 * l)),
        -0.7, -0.6)

    trueEnergy = - 1 / (2 * (l + 1) ** 2)
    root_exec_time = round((time.time() - root_start_time), 2)

    print("l = {} -> n = {}".format(l, l + 1), end="\t", flush=True)
    print("E(num) = {: .6f}".format(root), end="\t", flush=True)
    print("E(exact) = {: .6f}".format(trueEnergy), end="\t", flush=True)
    print("({: .2f}s)".format(root_exec_time))


if __name__ == "__main__":
  main()


"""
def trapezoidal(f, a, b, n = 10000):
	s = 0.0
	h = (b - a) / n
	for i in  range(1, n):
		s += f(a + i * h)

	return h * (s + 0.5 * (f(a) + f(b)))
"""
