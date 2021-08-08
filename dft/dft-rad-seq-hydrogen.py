#!/bin/env python3
# coding: utf8

'''
My implementation of DFT Assignment 4: Radial SEQ for the H-atom

Taught by René Wirnata in 2019/2020. 

Links:
https://tu-freiberg.de/fakultaet2/thph/lehre/density-functional-theory
https://github.com/PandaScience/teaching-resources



This script implements the full radial Schrödinger equation for the H-atom
according to Problem Sheet 4, Eq. (29) + atomic units. First, the 2nd order ODE
is converted to a system of 1st order equations in rseq(). This function is
then integrated for a prescribed range, energy and orbital quantum number using
the integrate() routine. The latter is coupled to the Secant root finding
algorithm from scipy, that should eventually return the lowest energy for the
given setup, provided the numerical parameters are set properly.
For the hydrogen ground state, i.e. 1 electron in the 1s-orbital, this script
yields almost exactly -0.5 Hartree.
In addition, we can find the beginning of the H-atom spectrum using a neat
trick (see assignment sheet):
l  -->  n : En/Hartree  ~  exact/Hartree
----------------------------------------
0  -->  1 : -0.499981   ~ -0.5
1  -->  2 : -0.124997   ~ -0.125
2  -->  3 : -0.0544761  ~ -0.0555556
3  -->  4 : -0.0302866  ~ -0.03125
4  -->  5 : -0.0189096  ~ -0.02
5  -->  6 : -0.0126417  ~ -0.0138889
6  -->  7 : -0.0340679  ~ -0.0102041
7  -->  8 : -0.24337    ~ -0.0078125

Unfortunately, the numerical values are far off for n>6. For small principal
quantum numbers, however, we find quite accurate energies when comparing to the
exact ones, En = -Z^2 / (2 * n^2).
'''


import time
import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp, trapz


''' secant method; x1 and x2 are crucial for finding the desired root '''
def secant(f, x1 = -12345, x2 = 6789, maxiter = 10000, tol = 1e-10):
  for iter in range(maxiter):
    xnew = x2 - (x2 - x1) / (f(x2) - f(x1)) * f(x2)
    if abs(xnew - x2) < tol:
      break
    else:
      x1 = x2
      x2 = xnew

  else:
    print("Calculation exceeded maximum number of iterations!")
    exit()

  return xnew


''' returns radial SEQ as system of two 1st order differential equations '''
def radSchrodinger(t, y, energy, l):
  # input: y = [y1, y2]; return y = [y1', y2']
  # y1' = y2; y2' = (...)*y1
  return [y[1], ((l * (l + 1)) / t ** 2 - 2 * (1 / t + energy)) * y[0]]


''' getting initial values for numeric intergration from correct solution '''
def initValues(r):
  u = 2 * r * np.exp(-r)
  uPrime = (1 - r) * 2 * np.exp(-r)
  return [u, uPrime]


''' wrapper for ODE integration; energy and l as parameter'''
def wrapper(energy, l, rmax, rmin):
  sol = solve_ivp(
    lambda t, y: radSchrodinger(t, y, energy, l),
    t_span = [rmax, rmin],
    t_eval = np.linspace(rmax, rmin, 1000),
    y0 = initValues(rmax))

  u = sol.y[0]
  r = sol.t
  return u[::-1], r[::-1]


''' get first value of integrated Schrödinger equation; since the array is
reversed, u[0] corresponds to the u-value at r = 0 (y-interscetion); different
energies are passed in by secant method '''
def u0(energy, l = 0, rmax = 20.0, rmin = 0.001):
  u, r = wrapper(energy, l, rmax, rmin)
  return u[0]


''' integrating with calculated energy eigenvalue and normalization'''
def normalize(energy, l = 0, rmax = 20.0, rmin = 0.001):
  u, r = wrapper(energy, l, rmax, rmin)

  uSquared = u * u
  norm = trapz(uSquared, r)
  uSquared /= norm

  return uSquared, r


''' exact solution for radial Schrödinger equation; returns square '''
def exactSolution(r):
  u = 2 * r * np.exp(-r)
  return u * u


def main():
  # find lowest energy eigenvalue; corresponds to energy of hydrogen atom
  root_start_time = time.time()
  root = secant(u0, -1.001, -1.0)
  root_exec_time = round((time.time() - root_start_time), 2)
  print("Energy of hydrogen atom: {:.5f} Hartree\t ({:.2f}s)".format(root, root_exec_time))

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
  ax3.plot(r, exactSolution(r), ls=(0, (5, 1)), lw="1.5", label=r"$u^2_\mathrm{exact}(r)$")
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

    trueEnergy = - 1 / (2 * (l + 1) **2)
    root_exec_time = round((time.time() - root_start_time), 2)

    print("l = {} -> n = {} \t\t E(num) = {: .6f} \t E(exact) = {: .6f} \t ({:.2f}s)".format(l, l + 1, root, trueEnergy, root_exec_time))
  

if __name__ == "__main__":
    main()



'''
def trapezoidal(f, a, b, n = 10000):
  s = 0.0
  h = (b - a) / n
  for i in  range(1, n):
    s += f(a + i * h)

  return h * (s + 0.5 * (f(a) + f(b)))
'''