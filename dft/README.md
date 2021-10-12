# Density Functional Theory Assignments #

Rudimentary DFT program. Extra course taught by René Wirnata in 2019/2020.

**Sources**  
[https://tu-freiberg.de/fakultaet2/thph/lehre/density-functional-theory](https://tu-freiberg.de/fakultaet2/thph/lehre/density-functional-theory)  
[https://github.com/PandaScience/teaching-resources](https://github.com/PandaScience/teaching-resources)

&nbsp;
### Assignment 1 ###
This script implements the full radial Schrödinger equation for the H-atom.
First, the 2nd order ODE is converted to a system of 1st order equations.
This function is then integrated for a prescribed range, energy and orbital
quantum number. The latter is coupled to the Secant root finding algorithm,
that should eventually return the lowest energy for the given setup,
provided the numerical parameters are set properly. For the hydrogen
ground state, i.e. 1 electron in the 1s-orbital, this script yields almost
exactly -0.5 Hartree.


### Assignment 2 ###
This script uses the last assignment's code to determine a solution of the
radial Schrödinger equation for the hydrogen ground state (n=1, l=0). After
normalizing, the Hartree potential energy is computed in a second "integration"
step and numerically integrated to the Hartree energy (~0.3125 Ha). For
hydrogen, the homogeneous solution is not required in order to match the
boundary condition.


### Assignment 3 ###
This is a self-consistent version of Assignment 2 with support for the
He-atom. Instead of the real electron-electron interaction term, we use the
classical Hartree potential and include a Hartree-Fock-like self-interaction
correction (SIC), which amounts to half of the proper Hartree potential. This
hack will be replaced by the LDA exchange-correlation (XC) potential in the
next assignment. Nevertheless, the code structure already follows the general
Kohn-Sham self-consconsistency cycle (see below).

The Kohn-Sham basically contains 6 steps:
<ol start="0">
  <li>initialize density; here we use the exact density of the H-atom,</li>
  <li>build the effective Kohn-Sham potential; here no XC but SIC-Hartree,</li>
  <li>solve the single-particle Schrödinger-like equation,</li>
  <li>build the new density from single-particle solutions,</li>
  <li>evaluate the total energy density functional E[n] for the new density,</li>
  <li>check for self-consistency,</li>
  <li>mix new and old density for faster convergence and increased stability.</li>
</ol>
