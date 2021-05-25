!> This is your module to ! writeyour very own SCF program.
module uhf
  !> Include standard Fortran environment for IO
  use iso_fortran_env, only: output_unit, error_unit

  ! ------------------------------------------------------------------------
  !> library functions provided by your lab assistents:

  !> interface to LAPACK's double precision symmetric eigenvalue solver (dspev)
  !  examples:
  !  call solve_spev(mat, eigval, eigvec)
  use linear_algebra, only: solve_spev

  !> expansion of slater-functions into contracted gaussians,
  !  coeffs and primitive expnts are taken from R.F. Stewart, JCP, 1970
  !  example:
  !  call expand_slater(zeta, alpha, coeff)
  use slater, only: expand_slater

  !> calculates one-electron integrals and two-electron integrals over
  !  spherical gaussians (s-functions). One-electron quanities supported
  !  are overlap, kinetic energy and nuclear attraction integrals.
  !  Two-electron integrals are provided in chemist notation.
  !  examples:
  !  call oneint(xyz, chrg, r_a, r_b, alp, bet, ca, ca, s, t, v)
  !  call twoint(r_a, r_b, r_c, r_d, alp, bet, gam, del, ca, cb, cc, cd, g)
  use integrals, only: oneint, twoint

  !> prints a matrix quantity to screen
  !  examples:
  !  call write_vector(vec, name='vector')
  !  call write_matrix(mat, name='matrix')
  !  call write_matrix(mat, name='packed matrix')
  use print_matrix, only: write_vector, write_matrix

  !> other tools that may help you jump ahead with I/O-heavy tasks
  !  example: call read_line(input, line)
  use io_tools, only: read_line

  !> outsourced subroutines
  use array_funcs

  !> Always declare everything explicitly
  implicit none

  private
  public :: uhf_prog

  !> Selecting double precision real number
  integer, parameter :: wp = selected_real_kind(15)

contains

!> This is the entry point to your program, do not modify the dummy arguments
!  without adjusting the call in lib/prog.f90
  subroutine uhf_prog(nat, nel, nbf, ng, xyz, chrg, zeta, bf_atom_map, &
                      ehf, MAX_SCF, TOL_SCF, print_level)
    implicit none

    !> System specific data
    !> Number of atoms
    integer, intent(in) :: nat
    !> Number of electrons
    integer, intent(in) :: nel
    !> Number of basis functions
    integer, intent(in) :: nbf
    !> number of primitive Gaussians in slater expansion (STO-nG)
    integer, intent(in) :: ng

    !> Atom coordinates of the system, all distances in bohr
    real(wp), intent(in), allocatable :: xyz(:, :)
    !> Nuclear charges
    real(wp), intent(in), allocatable :: chrg(:)
    !> Slater expnts of basis functions
    real(wp), intent(in), allocatable :: zeta(:)
    !> maps basis function to atom
    !> e.g.: (1, 1, 2) -> 1st bf from A1, 2nd bf from A1, 3rd bf from A3
    integer, intent(in), allocatable :: bf_atom_map(:)

    !> convergence criteria
    real(wp), intent(in) :: TOL_SCF
    integer, intent(in) :: MAX_SCF

    !> amount of info printed during program run (0, 1, 2)
    integer, intent(in), optional :: print_level

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !> nuclear repulsion energy
    real(wp) :: enn
    !> SCF energy
    real(wp) :: escf
    !> HF energy
    real(wp), intent(out) :: ehf

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> expnts of primitive Gaussian functions
    real(wp), dimension(:, :), allocatable :: expnts

    !> coeffs of primitive Gaussian functions
    real(wp), dimension(:, :), allocatable :: coeffs

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real(wp), dimension(:, :), allocatable :: S, T, V
    real(wp), dimension(:), allocatable :: S_packed, T_packed, V_packed

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real(wp), dimension(:, :), allocatable :: X ! orthonormalizer
    real(wp), dimension(:, :), allocatable :: P_a ! density matrix
    real(wp), dimension(:, :), allocatable :: P_b ! density matrix
    real(wp), dimension(:, :), allocatable :: C_a ! density matrix
    real(wp), dimension(:, :), allocatable :: C_b ! density matrix
    integer, dimension(:, :), allocatable :: n_occ_a ! occupation matrix
    integer, dimension(:, :), allocatable :: n_occ_b ! occupation matrix

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !> Number of alpha and beta electrons
    integer :: nel_a, nel_b
    real(wp) :: spin_uhf

    integer :: alloc_stat
    real(wp), dimension(:, :, :, :), allocatable :: two_ints ! 4D array of tei

101 format(//, "---------------------------------------------------------------")
102 format("---------------------------------------------------------------")

    !*********************************************************
    !********************** ALLOCATION ***********************
    !*********************************************************

    allocate (X(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (two_ints(nbf, nbf, nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (P_a(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (P_b(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (C_a(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (C_b(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (S(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (T(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (V(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

    !*********************************************************
    !*************** NUCLEAR REPULSION ENERGY ****************
    !*********************************************************

    if (present(print_level)) then
      if (print_level >= 1) then
        write (*, 101)
        write (*, "(A)") "Precalculations"
        write (*, 102)
        write (*, "(A28,7X)", advance="no") "Nuclear repulsion energy ..."
      end if
    end if

    !> nuclear repulsion energy
    call get_enn(xyz, chrg, nat, enn)
    if (present(print_level) .and. print_level >= 1) then
      write (*, "(A)") "done"
      write (*, "(A21,14X)", advance="no") "Gaussian orbitals ..."
    end if

    !*********************************************************
    !******************* BASIS SET SETUP  ********************
    !*********************************************************

    !> basis set setup
    call expand_slater_wrapper(nbf, ng, zeta, expnts, coeffs)
    if (present(print_level) .and. print_level >= 1) then
      write (*, "(A)") "done"
      write (*, "(A21,14X)", advance="no") "Occupation matrix ..."
    end if

    !*********************************************************
    !******************* OCCUPATION MATRIX *******************
    !*********************************************************

    nel_a = (nel + 1)/2
    nel_b = nel - nel_a

    !> build (diagonal) occupation matrix
    call set_n_occ_uhf(nbf, nel_a, n_occ_a)
    call set_n_occ_uhf(nbf, nel_b, n_occ_b)
    if (present(print_level) .and. print_level >= 1) then
      write (*, "(A)") "done"
      write (*, "(A26,9X)", advance="no") "One-electron integrals ..."
    end if

    !*********************************************************
    !***************** ONE-ELECTRON INTEGRALS ****************
    !*********************************************************

    call get_oneint(nbf, xyz, chrg, bf_atom_map, expnts, coeffs, S, V, T)
    if (present(print_level) .and. print_level >= 1) then
      write (*, "(A)") "done"
      write (*, "(A33,2X)", advance="no") "Packing one-electron matrices ..."
    end if

    !> pack matrices if symmetric
    call pack_matrix(S, S_packed)
    call pack_matrix(V, V_packed)
    call pack_matrix(T, T_packed)
    if (present(print_level) .and. print_level >= 1) then
      write (*, "(A)") "done"
      write (*, "(A26,9X)", advance="no") "Two-electron integrals ..."
    end if

    !*********************************************************
    !*************** TWO ELECTRON INTEGRALS ******************
    !*********************************************************

    call get_twoint(nbf, xyz, expnts, coeffs, bf_atom_map, two_ints)
    if (present(print_level) .and. print_level >= 1) then
      write (*, "(A)") "done"
      write (*, "(A29,6X)", advance="no") "Symmetric orthonormalizer ..."
    end if

    !*********************************************************
    !************** SYMMETRIC ORTHONORMALIZER ****************
    !*********************************************************

    !> last argument optional, pass to check if X^T * S * X = 1
    call sym_orthonormalizer(S_packed, nbf, X, S)
    if (present(print_level) .and. print_level >= 1) then
      write (*, "(A)") "done"
    end if

    !*********************************************************
    !******************* EXTENDED OUTPUT *********************
    !*********************************************************

    if (present(print_level)) then
      if (print_level >= 3) then
        write (*, 101)
        write (*, "(A)") "Precalculations: extended output"
        write (*, 102)

        call write_matrix(coeffs, name="Gaussian coeffs")
        call write_matrix(expnts, name="Gaussian expnts")
        call write_matrix(S, name="Overlap Matrix S")
        call write_matrix(T, name="Kinetic Energy Matrix T")
        call write_matrix(V, name="Nuclear Attraction Matrix V")
        call write_matrix(X, name="Symmetric orthonormalizer X")
      end if
    end if

    !*********************************************************
    !******************** INITIAL GUESS **********************
    !*********************************************************

    if (present(print_level) .and. print_level >= 1) then
      write (*, 101)
      write (*, "(A)") "Initial Guess"
      write (*, 102)
    end if

    !> get initial coefficient matrix, UHF needs different guesses for P
    P_a = matmul(matmul(X, n_occ_a), X)
    P_b = 0.0_wp
    call iter_step(nbf, T, V, two_ints, X, n_occ_a, n_occ_b, P_a, P_b, &
                   C_a, C_b, escf, print_level)

    !*********************************************************
    !******************* SCF PROCEDURE ***********************
    !*********************************************************

    if (present(print_level) .and. print_level >= 1) then
      write (*, 101)
      write (*, "(A)") "SCF Iterations"
      write (*, 102)
    end if

    !> scf loop
    call scf_loop(nbf, T, V, two_ints, X, n_occ_a, n_occ_b, P_a, P_b, &
                  C_a, C_b, escf, MAX_SCF, TOL_SCF, print_level)
    ehf = escf + enn

    !*********************************************************
    !***************** SPIN CONTAMINATION ********************
    !*********************************************************

    call get_spin_contam(nel_a, nel_b, C_a, C_b, S, spin_uhf, print_level)

    !*********************************************************
    !******************** PRINT RESULTS **********************
    !*********************************************************

    if (present(print_level) .and. print_level /= 0) then
      write (*, 101)
      write (*, "(A)") "Energies (in Hartree)"
      write (*, 102)

      write (*, "(A28,7X,F15.10)") "Nuclear repulsion energy ...", enn
      write (*, "(A14,21X,F15.10)") "SCF energy ...", escf
      write (*, "(A23,12X,F15.10)") "Hartree-Fock energy ... ", ehf
      write (*, "(A16,19X,F15.10)") "Total energy ...", ehf
    end if
  end subroutine uhf_prog

!*****************************************************************************
!**************************** END MAIN FUNCTION ******************************
!*****************************************************************************

  subroutine get_spin_contam(nel_a, nel_b, C_a, C_b, S, spin_uhf, print_level)
    implicit none

    integer, intent(in) :: nel_a
    integer, intent(in) :: nel_b
    real(wp), dimension(:, :), allocatable, intent(in) :: C_a
    real(wp), dimension(:, :), allocatable, intent(in) :: C_b
    real(wp), dimension(:, :), allocatable, intent(in) :: S
    integer, intent(in), optional :: print_level
    real(wp), intent(out) ::  spin_uhf

    ! variables for routine
    real(wp) :: s_exact, delta
    integer :: i, j, dim, alloc_stat
    real(wp), allocatable, dimension(:, :) :: temp

    if (present(print_level) .and. print_level >= 1) then
      write (*, *) ""
      write (*, "(A34,6X)", advance="no") "Calculating spin contamination ..."
    end if

    ! allocate temp variable
    dim = get_dim_mat(S)
    allocate (temp(dim, dim), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

    ! transform AO to MO basis
    temp = matmul(matmul(transpose(C_a), S), C_b)

    !> calculate exact spin contamination
    s_exact = (nel_a - nel_b)*0.5_wp*((nel_a - nel_b)*0.5_wp + 1)

    delta = nel_b
    do i = 1, nel_a
      do j = 1, nel_b
        delta = delta - temp(i, j)**2
      end do
    end do
    spin_uhf = s_exact + delta

    if (present(print_level) .and. print_level >= 1) then
      write (*, "(A)") "done"
      write (*, "(A25,6X,F9.6)") " -> Ideal value of S**2: ", s_exact
      write (*, "(A31,F9.6)") " -> Expectation value of S**2: ", spin_uhf
      write (*, "(A24,7X,F9.6)") " -> Spin contamination: ", delta
    end if
  end subroutine get_spin_contam

  subroutine scf_loop(nbf, T, V, two_ints, X, n_occ_a, n_occ_b, P_a, P_b, &
                      C_a, C_b, escf, MAX_SCF, TOL_SCF, print_level)
    implicit none

    integer, intent(in) :: nbf
    integer, intent(in) :: MAX_SCF
    real(wp), intent(in) :: TOL_SCF
    real(wp), dimension(:, :), allocatable, intent(in) :: T
    real(wp), dimension(:, :), allocatable, intent(in) :: V
    real(wp), dimension(:, :), allocatable, intent(in) :: X
    real(wp), dimension(:, :, :, :), allocatable, intent(in) :: two_ints
    integer, dimension(:, :), allocatable, intent(in) :: n_occ_a
    integer, dimension(:, :), allocatable, intent(in) :: n_occ_b
    real(wp), dimension(:, :), allocatable, intent(inout) :: P_a
    real(wp), dimension(:, :), allocatable, intent(inout) :: P_b
    real(wp), dimension(:, :), allocatable, intent(inout) :: C_a
    real(wp), dimension(:, :), allocatable, intent(inout) :: C_b
    integer, optional, intent(in) :: print_level
    real(wp), intent(inout) :: escf

    ! variables for routine
    integer :: i
    real(wp) :: escf_old, ediff

    do i = 1, MAX_SCF
      if (present(print_level) .and. print_level >= 1) then
        write (*, "(A10,I2,A1,2X)", advance="no") "Iteration ", i, ":"
      end if

      escf_old = escf
      call iter_step(nbf, T, V, two_ints, X, n_occ_a, n_occ_b, P_a, P_b, &
                     C_a, C_b, escf, print_level)

      ! calc diff between iterations, print it and exit if below threshold
      ediff = abs(escf - escf_old)

      if (present(print_level) .and. print_level >= 1) then
        write (*, "(2X,A9,F15.10)") "change =", ediff
      end if

      ! check convergence
      if (ediff < TOL_SCF) exit
    end do

    !> exit if number of iteration exceeds maximum
    if (i == MAX_SCF) then
      write (*, "(A)") "Maximum number of iterations reached."
      write (*, "(A)") "Energy not converged!"
      error stop 1
    end if

    if (present(print_level) .and. print_level >= 1) then
      write (*, *) ""
      write (*, "(A20,I2,A8)") "SCF converged after ", i, " cycles!"
    end if
  end subroutine scf_loop

  subroutine iter_step(nbf, T, V, two_ints, X, n_occ_a, n_occ_b, P_a, P_b, &
                       C_a, C_b, escf, print_level)
    implicit none
    integer, intent(in) :: nbf
    real(wp), intent(in), dimension(:, :), allocatable :: T
    real(wp), intent(in), dimension(:, :), allocatable :: V
    real(wp), intent(in), dimension(:, :, :, :), allocatable :: two_ints
    real(wp), intent(in), dimension(:, :), allocatable :: X
    integer, intent(in), dimension(:, :), allocatable :: n_occ_a
    integer, intent(in), dimension(:, :), allocatable :: n_occ_b
    integer, intent(in), optional :: print_level
    real(wp), intent(inout), dimension(:, :), allocatable :: P_a
    real(wp), intent(inout), dimension(:, :), allocatable :: P_b
    real(wp), intent(inout), dimension(:, :), allocatable :: C_a
    real(wp), intent(inout), dimension(:, :), allocatable :: C_b
    real(wp), intent(out) :: escf

    ! variables for routine
    ! Fock matrix: standard, transformed and packed
    real(wp), dimension(:, :), allocatable :: F_a
    real(wp), dimension(:, :), allocatable :: F_a_prime
    real(wp), dimension(:), allocatable :: F_a_prime_packed
    real(wp), dimension(:, :), allocatable :: F_b
    real(wp), dimension(:, :), allocatable :: F_b_prime
    real(wp), dimension(:), allocatable :: F_b_prime_packed

    ! eigenvalues Fock matrix
    real(wp), dimension(:), allocatable :: eps_a
    real(wp), dimension(:), allocatable :: eps_b

    ! H_core matrix (H = T + V)
    real(wp), dimension(:, :), allocatable :: H

    integer :: alloc_stat, stat_lapack, i, j, k, l

    allocate (F_a(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (F_a_prime(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (F_a_prime_packed(nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (F_b(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (F_b_prime(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (F_b_prime_packed(nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (eps_a(nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (eps_b(nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (H(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

    !print
    if (present(print_level)) then
      if (print_level >= 2) then
        write (*, *) ""
        write (*, "(A24,16X)", advance="no") "Building Fock matrix ..."
      end if
    end if

    !> build initial Fock matrix: F = H0 = T + V
    H = T + V
    F_a = H
    F_b = H
    do i = 1, nbf
      do j = 1, nbf
        do k = 1, nbf
          do l = 1, nbf
            F_a(i, j) = F_a(i, j) + &
                        (P_a(k, l) + P_b(k, l))*two_ints(i, j, l, k) - &
                        P_a(k, l)*two_ints(i, l, j, k)
            F_b(i, j) = F_b(i, j) + &
                        (P_a(k, l) + P_b(k, l))*two_ints(i, j, l, k) - &
                        P_b(k, l)*two_ints(i, l, j, k)
          end do
        end do
      end do
    end do

    ! print
    if (present(print_level)) then
      if (print_level >= 2) then
        write (*, *) "done"
        if (print_level >= 3) then
          call write_matrix(F_a, name="Fock matrix (alpha)")
          call write_matrix(F_b, name="Fock matrix (beta)")
          write (*, *) ""
        end if
        write (*, "(A28,12X)", advance="no") "Transforming Fock matrix ..."
      end if
    end if

    !> transform Fock matrix: F' = X**T * F * X (X symmetric: X**T = X)
    F_a_prime = matmul(matmul(transpose(X), F_a), X)
    F_b_prime = matmul(matmul(transpose(X), F_b), X)

    ! print
    if (present(print_level)) then
      if (print_level >= 2) then
        write (*, *) "done"
        if (print_level >= 3) then
          call write_matrix(F_a_prime, name="Transformed Fock matrix (alpha)")
          call write_matrix(F_b_prime, name="Transformed Fock matrix (beta)")
          write (*, *) ""
        end if
        write (*, "(A29,11X)", advance="no") "Diagonalizing Fock matrix ..."
      end if
    end if

    !> pack Fock matrix for solve_spev
    call pack_matrix(F_a_prime, F_a_prime_packed)
    call pack_matrix(F_b_prime, F_b_prime_packed)

    !> diagonalize Fock matrix, i.e. solve F'C' = C'e
    call solve_spev(F_a_prime_packed, eps_a, C_a, stat_lapack)
    if (stat_lapack /= 0) then
      write (*, *) "Lapack error: ", stat_lapack
      error stop 1
    end if
    call solve_spev(F_b_prime_packed, eps_b, C_b, stat_lapack)
    if (stat_lapack /= 0) then
      write (*, *) "Lapack error: ", stat_lapack
      error stop 1
    end if

    ! print
    if (present(print_level)) then
      if (print_level >= 2) then
        write (*, *) "done"
        if (print_level >= 3) then
          call write_vector(eps_a, name="eigenvalues Fock matrix (alpha)")
          call write_vector(eps_b, name="eigenvalues Fock matrix (beta)")
          write (*, *) ""
        end if
        write (*, "(A30,10X)", advance="no") "Calculating density matrix ..."
      end if

    end if

    !> backtransformation: C = X * C'
    C_a = matmul(X, C_a)
    C_b = matmul(X, C_b)

    !> P = C * n0 * C**T
    P_a = matmul(matmul(C_a, n_occ_a), transpose(C_a))
    P_b = matmul(matmul(C_b, n_occ_b), transpose(C_b))

    ! print
    if (present(print_level)) then
      if (print_level >= 2) then
        write (*, *) "done"
      end if
      if (print_level >= 3) then
        call write_matrix(P_a, name="density matrix P (alpha)")
        call write_matrix(P_b, name="density matrix P (beta)")
        write (*, *) ""
      end if
    end if

    !> calculate SCF energy
    call get_escf(F_a, F_b, H, P_a, P_b, escf)
    if (present(print_level) .and. print_level >= 1) then
      write (*, "(A7,F15.10)", advance="no") "E_SCF =", escf
    end if
  end subroutine iter_step

  subroutine set_n_occ_uhf(nbf, nel, n_occ)
    implicit none
    integer, intent(in) :: nbf
    integer, intent(in) :: nel
    integer, intent(out), allocatable, dimension(:, :) :: n_occ

    ! variables for routine
    integer :: alloc_stat, i, temp

    allocate (n_occ(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

    n_occ = 0
    temp = nel
    do i = 1, nbf
      if (temp == 0) exit
      n_occ(i, i) = 1
      temp = temp - 1
    end do
  end subroutine set_n_occ_uhf

  subroutine get_escf(F_a, F_b, H, P_a, P_b, escf)
    implicit none
    real(wp), intent(in), dimension(:, :), allocatable :: F_a
    real(wp), intent(in), dimension(:, :), allocatable :: F_b
    real(wp), intent(in), dimension(:, :), allocatable :: P_a
    real(wp), intent(in), dimension(:, :), allocatable :: P_b
    real(wp), intent(in), dimension(:, :), allocatable :: H ! H_core
    real(wp), intent(out) :: escf

    ! variables for subroutine
    integer :: dim
    integer :: i, j

    !> get shape of matrix
    dim = get_dim_mat(F_a)

    !> calculate energy
    escf = 0.0_wp
    do i = 1, dim
      do j = 1, dim
        escf = escf + P_a(i, j)*(H(i, j) + F_a(i, j)) + &
               P_b(i, j)*(H(i, j) + F_b(i, j))
      end do
    end do

    !> E_HF = 0.5 * Tr{ (F + H0) * P }
    escf = 0.5_wp*escf
  end subroutine get_escf

  subroutine sym_orthonormalizer(S_packed, nbf, X, S)
    implicit none

    intrinsic :: matmul, transpose
    integer, intent(in) :: nbf
    real(wp), intent(in), dimension(:, :), allocatable, optional :: S
    real(wp), intent(in), dimension(:), allocatable :: S_packed
    real(wp), intent(out), dimension(:, :), allocatable :: X

    ! variables for routine
    integer :: i, stat_lapack, alloc_stat
    real(wp), dimension(:), allocatable :: eigval
    real(wp), dimension(:, :), allocatable :: eigvec
    real(wp), dimension(:, :), allocatable :: s_small
    real(wp), dimension(:), allocatable :: mat_temp
    real(wp), dimension(:, :), allocatable :: testX

    !> allocation
    allocate (eigval(nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (eigvec(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (s_small(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (mat_temp(nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

    !> call routine on packed matrix, diagonalizes S
    mat_temp = S_packed
    call solve_spev(mat_temp, eigval, eigvec, stat_lapack)
    if (stat_lapack /= 0) then
      write (*, *) "Lapack error: ", stat_lapack
      error stop 1
    end if

    ! form s matrix
    s_small = 0.0_wp
    do i = 1, nbf
      s_small(i, i) = sqrt(1/eigval(i))
    end do

    ! calculate X = U s**(-1/2) U**T
    X = matmul(matmul(eigvec, s_small), transpose(eigvec))

    ! do the check if the unpacked matrix is passed
    if (present(S)) then
      allocate (testX(nbf, nbf), stat=alloc_stat)
      if (alloc_stat /= 0) error stop 1

      ! calculate X**T * S * X
      testX = matmul(matmul(transpose(X), S), X)
      if (is_mat_identity(testX) .eqv. .false.) then
        write (*, *) "Symmetric orthonormalizer not obtained! Aborting..."
        call write_matrix(testX, name="X^T * S * X")
        error stop 1
      end if
    end if
  end subroutine sym_orthonormalizer

  subroutine pack_matrix(mat, mat_packed)
    implicit none
    real(wp), intent(in), dimension(:, :), allocatable :: mat
    real(wp), intent(out), dimension(:), allocatable :: mat_packed

    ! variables for calculation
    integer :: i, j, counter, alloc_stat
    integer :: dim_packed, dim
    integer, dimension(2) :: dims

    !> get dim of matrix
    dims = shape(mat)
    dim = dims(1)

    if (is_mat_symmetric(mat) .eqv. .false.) then
      write (*, *) "failed"
      write (*, "(A,A)") "", "-> Could not pack matrix. Not symmetric!"
      call write_matrix(mat, name="mat")
      error stop 1
    end if

    !> formula for dimension of packed matrix
    dim_packed = dim*(1 + dim)/2

    !> allocate with calculated dimension
    allocate (mat_packed(dim_packed), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

    !> packing
    counter = 1
    do i = 1, dim
      do j = 1, i
        mat_packed(counter) = mat(i, j)
        counter = counter + 1
      end do
    end do

  end subroutine pack_matrix

  subroutine get_oneint(nbf, xyz, chrg, bf_atom_map, expnts, &
                        coeffs, S, V, T)
    implicit none

    integer, intent(in) :: nbf
    real(wp), intent(in), dimension(:, :), allocatable :: xyz
    real(wp), intent(in), dimension(:), allocatable :: chrg
    integer, intent(in), dimension(:), allocatable :: bf_atom_map
    real(wp), intent(in), dimension(:, :), allocatable :: expnts
    real(wp), intent(in), dimension(:, :), allocatable :: coeffs
    real(wp), intent(inout), dimension(:, :), allocatable :: S
    real(wp), intent(inout), dimension(:, :), allocatable :: T
    real(wp), intent(inout), dimension(:, :), allocatable :: V

    ! variables for calculation
    real(wp) :: sab, vab, tab
    integer :: i, j

    do i = 1, nbf
      do j = 1, nbf
        ! xyz(:, i) is wrong
        call oneint(xyz, chrg, &
                    xyz(:, bf_atom_map(i)), xyz(:, bf_atom_map(j)), &
                    expnts(:, i), expnts(:, j), &
                    coeffs(:, i), coeffs(:, j), &
                    sab, tab, vab)
        S(i, j) = sab
        T(i, j) = tab
        V(i, j) = vab
      end do
    end do
  end subroutine get_oneint

  subroutine get_twoint(nbf, xyz, expnts, coeffs, bf_atom_map, two_ints)
    implicit none
    integer, intent(in) :: nbf
    real(wp), intent(in), dimension(:, :) :: xyz
    real(wp), intent(in), dimension(:, :) :: expnts
    real(wp), intent(in), dimension(:, :) :: coeffs
    integer, intent(in), dimension(:) :: bf_atom_map
    real(wp), intent(out), dimension(:, :, :, :) :: two_ints

    ! variables for routine
    integer :: i, j, k, l
    real(wp) :: tei

    do i = 1, nbf
      do j = 1, i
        do k = 1, i
          do l = 1, merge(j, k, i == k)
            call twoint( &
              xyz(:, bf_atom_map(i)), xyz(:, bf_atom_map(j)), &
              xyz(:, bf_atom_map(k)), xyz(:, bf_atom_map(l)), &
              expnts(:, i), expnts(:, j), &
              expnts(:, k), expnts(:, l), &
              coeffs(:, i), coeffs(:, j), &
              coeffs(:, k), coeffs(:, l), &
              tei)

            two_ints(i, j, k, l) = tei
            two_ints(j, i, k, l) = tei
            two_ints(i, j, l, k) = tei
            two_ints(j, i, l, k) = tei

            two_ints(k, l, i, j) = tei
            two_ints(k, l, j, i) = tei
            two_ints(l, k, i, j) = tei
            two_ints(l, k, j, i) = tei
          end do
        end do
      end do
    end do
  end subroutine get_twoint

  subroutine expand_slater_wrapper(nbf, ng, zeta, expnts, coeffs)
    implicit none
    integer, intent(in) :: nbf
    integer, intent(in) :: ng
    real(wp), intent(in), dimension(:), allocatable ::  zeta
    real(wp), intent(out), dimension(:, :), allocatable :: expnts
    real(wp), intent(out), dimension(:, :), allocatable :: coeffs
    integer :: i, alloc_stat

    allocate (expnts(ng, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop "Allocation failed."
    allocate (coeffs(ng, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop "Allocation failed."

    do i = 1, nbf
      call expand_slater(ng, zeta(i), expnts(:, i), coeffs(:, i))
    end do
  end subroutine expand_slater_wrapper

  subroutine get_enn(xyz, chrg, nat, enn)
    implicit none
    intrinsic :: norm2

    ! in and out variables
    integer, intent(in) :: nat
    real(wp), intent(in), dimension(:), allocatable :: chrg
    real(wp), intent(in), dimension(:, :), allocatable :: xyz
    real(wp), intent(out) :: enn

    ! variables for calculation
    integer :: i, j
    real(wp) :: Z_A, Z_B
    real(wp) :: dist

    enn = 0.0_wp
    do i = 1, nat - 1
      Z_A = chrg(i)
      do j = i + 1, nat
        Z_B = chrg(j)

        ! d = | R1 - R2 |
        dist = norm2(xyz(:, i) - xyz(:, j))

        ! enn = ZA * ZB / d
        enn = enn + Z_A*Z_B/dist
      end do
    end do
  end subroutine get_enn
end module uhf

