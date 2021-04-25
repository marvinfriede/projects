!> This is your module to write your very own SCF program.
module scf_main
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
  !  example:
  !  call read_line(input, line)
  use io_tools, only: read_line

  !> small helper functions for matrices
  use array_funcs, only: is_mat_symmetric, is_mat_identity, get_dim_mat

  !> Always declare everything explicitly
  implicit none

  !> All subroutines within this module are not exported, except for scf_prog
  !  which is the entry point to your program
  private
  public :: scf_prog

  !> Selecting double precision real number
  integer, parameter :: wp = selected_real_kind(15)

contains

!> This is the entry point to your program, do not modify the dummy arguments
!  without adjusting the call in lib/prog.f90
  subroutine scf_prog(input, print_level)

    !> Always declare everything explicitly
    implicit none

    !> IO unit bound to the input file
    integer, intent(in) :: input

    !> amount of info printed during program run (0, 1, 2)
    integer, intent(in), optional :: print_level

    !> System specific data
    !> Number of atoms
    integer :: nat
    !> Number of electrons
    integer :: nel
    !> Number of basis functions
    integer :: nbf ! -> nbf

    !> Atom coordinates of the system, all distances in bohr
    real(wp), allocatable :: xyz(:, :)

    !> Nuclear charges
    real(wp), allocatable :: chrg(:)

    !> Slater expnts of basis functions
    real(wp), allocatable :: zeta(:)

    !> maps basis function to atom
    !> e.g.: (1, 1, 2) -> 1st bf from A1, 2nd bf from A1, 3rd bf from A3
    integer, allocatable :: bf_atom_map(:)

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !> energies
    !> nuclear repulsion energy
    real(wp) :: enn
    real(wp) :: ehf
    real(wp) :: escf

    real(wp), parameter :: TOL_SCF = 1.0e-10_wp
    integer, parameter :: MAX_SCF = 100

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !> number of primitive Gaussians in slater expansion (STO-nG)
    integer, parameter :: ng = 6

    !> expnts of primitive Gaussian functions
    real(wp), dimension(:, :), allocatable :: expnts

    !> coeffs of primitive Gaussian functions
    real(wp), dimension(:, :), allocatable :: coeffs

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real(wp), dimension(:, :), allocatable :: S, T, V
    real(wp), dimension(:), allocatable :: S_packed, T_packed, V_packed

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real(wp), dimension(:, :), allocatable :: X ! orthonormalizer
    real(wp), dimension(:, :), allocatable :: P ! density matrix
    integer, dimension(:, :), allocatable :: n_occ ! occupation matrix

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    integer :: alloc_stat
    real(wp), dimension(:, :, :, :), allocatable :: two_ints ! 4D array of tei

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

101 format(//, "---------------------------------------------------------------")
102 format("---------------------------------------------------------------")

    !*********************************************************
    !********************* READING INPUT *********************
    !*********************************************************

    !> init all variables from input file
    call read_file(input, xyz, chrg, zeta, bf_atom_map, nat, nel, nbf)

    write (*, 101)
    write (*, "(A)") "Settings"
    write (*, 102)

    write (*, "(A)") "Molecule"
    write (*, "(A20,15X,I1)") " Number of atoms ...", nat
    write (*, "(A24,11X,I1)") " Number of electrons ...", nel

    write (*, "(A)") "Basis set"
    write (*, "(A31,4X,I1)") " Number of Slater functions ...", nbf
    write (*, "(A18,17X,A4,I1,A1)") " Gaussian Type ...", "STO-", ng, "G"

    write (*, "(A)") "SCF"
    write (*, "(A26,9X,A)") " Convergence criterion ...", "Energy"
    write (*, "(A26,9X,ES7.1)") " Convergence threshold ...", TOL_SCF
    write (*, "(A33,2X,I3)") " Maximal number of iterations ...", MAX_SCF

    if (present(print_level)) then
      if (print_level >= 1) then
        write (*, 101)
        write (*, "(A)") "Settings: extended output"
        write (*, 102)
        call write_matrix(xyz, name='Coordinates')
        call write_vector(chrg, name="Charges")
        call write_vector(real(bf_atom_map, wp), name="Mapping: basis function -> atom")
      end if
    end if

    !*********************************************************
    !********************** ALLOCATION ***********************
    !*********************************************************

    allocate (X(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (two_ints(nbf, nbf, nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (P(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

    !*********************************************************
    !*************** NUCLEAR REPULSION ENERGY ****************
    !*********************************************************

    write (*, 101)
    write (*, "(A)") "Precalculations"
    write (*, 102)

    !> nuclear repulsion energy
    write (*, "(A28,7X)", advance="no") "Nuclear repulsion energy ..."
    call get_enn(xyz, chrg, nat, enn)
    write (*, "(A)") "done"

    !*********************************************************
    !******************* BASIS SET SETUP  ********************
    !*********************************************************

    !> basis set setup
    write (*, "(A21,14X)", advance="no") "Gaussian orbitals ..."
    call expand_slater_wrapper(nbf, ng, zeta, expnts, coeffs)
    write (*, "(A)") "done"

    !*********************************************************
    !******************* OCCUPATION MATRIX *******************
    !*********************************************************

    !> build (diagonal) occupation matrix
    write (*, "(A21,14X)", advance="no") "Occupation matrix ..."
    call set_n_occ(nbf, nel, n_occ)
    write (*, "(A)") "done"

    !*********************************************************
    !***************** ONE-ELECTRON INTEGRALS ****************
    !*********************************************************

    write (*, "(A26,9X)", advance="no") "One-electron integrals ..."
    call get_oneint(nbf, xyz, chrg, bf_atom_map, expnts, coeffs, S, V, T)
    write (*, "(A)") "done"

    !> pack matrices if symmetric
    write (*, "(A33,2X)", advance="no") "Packing one-electron matrices ..."
    call pack_matrix(S, S_packed)
    call pack_matrix(V, V_packed)
    call pack_matrix(T, T_packed)
    write (*, "(A)") "done"

    !*********************************************************
    !*************** TWO ELECTRON INTEGRALS ******************
    !*********************************************************

    write (*, "(A26,9X)", advance="no") "Two-electron integrals ..."
    call get_twoint(nbf, xyz, expnts, coeffs, bf_atom_map, two_ints)
    write (*, "(A)") "done"

    !*********************************************************
    !************** SYMMETRIC ORTHONORMALIZER ****************
    !*********************************************************

    !> last argument optional, pass to check if X^T * S * X = 1
    write (*, "(A29,6X)", advance="no") "Symmetric orthonormalizer ..."
    call sym_orthonormalizer(S_packed, nbf, X, S)
    write (*, "(A)") "done"

    !*********************************************************
    !******************* EXTENDED OUTPUT *********************
    !*********************************************************

    if (present(print_level)) then
      if (print_level >= 1) then
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

    write (*, 101)
    write (*, "(A)") "Initial Guess"
    write (*, 102)

    !> get initial density matrix
    call iter_step(nbf, T, V, two_ints, X, n_occ, P, escf, print_level, &
                   is_initial=.true.)

    !*********************************************************
    !******************* SCF PROCEDURE ***********************
    !*********************************************************

    write (*, 101)
    write (*, "(A)") "SCF Iterations"
    write (*, 102)

    !> scf loop
    call scf_loop(nbf, T, V, two_ints, X, n_occ, P, escf, &
                  MAX_SCF, TOL_SCF, print_level)
    ehf = escf + enn

    !*********************************************************
    !******************** PRINT RESULTS **********************
    !*********************************************************

    write (*, 101)
    write (*, "(A)") "Energies (in Hartree)"
    write (*, 102)

    write (*, "(A28,7X,F15.10)") "Nuclear repulsion energy ...", enn
    write (*, "(A14,21X,F15.10)") "SCF energy ...", escf
    write (*, "(A23,12X,F15.10)") "Hartree-Fock energy ... ", ehf

    !*********************************************************
    !********************** PROPERTIES ***********************
    !*********************************************************

    write (*, 101)
    write (*, "(A)") "Properties"
    write (*, 102)

    call check_normalization(S, P, nel)
    call mulliken_analysis(S, P, chrg, bf_atom_map, nat)

    ! call scan_charge_density(P, expnts, xyz, bf_atom_map, ng)

    !*********************************************************
    !********************* DEALLOCATION **********************
    !*********************************************************
    deallocate (zeta)
    deallocate (chrg)
    deallocate (xyz)
    deallocate (expnts)
    deallocate (coeffs)
  end subroutine scf_prog

  !*********************************************************
  !****************** END MAIN FUNCTION ********************
  !*********************************************************

  subroutine scan_charge_density(P, expnts, xyz, bf_atom_map, ng)
    implicit none

    real(wp), dimension(:, :), allocatable, intent(in) :: P
    real(wp), dimension(:, :), allocatable, intent(in) :: expnts
    real(wp), dimension(:, :), allocatable, intent(in) :: xyz
    integer, dimension(:), allocatable, intent(in) :: bf_atom_map
    integer, intent(in) :: ng

    !> charge density at point r
    real(wp) :: rho

    !> position vector
    real(wp), dimension(3) :: r

    integer :: i
    integer, parameter :: n_steps = 100
    real(wp) :: start, end, step_size

    r = 0.0_wp
    start = -0.7_wp
    end = 0.7_wp
    step_size = abs(end - start)/real(n_steps, wp)

    do i = 0, n_steps
      r(3) = start + i*step_size
      call charge_density(r, P, expnts, xyz, bf_atom_map, ng, rho)
      write (*, *) r(3), rho
    end do

  end subroutine scan_charge_density

  subroutine charge_density(r, P, expnts, xyz, bf_atom_map, ng, rho)
    implicit none
    intrinsic :: sum, sqrt, exp

    real(wp), dimension(3), intent(in) :: r
    real(wp), dimension(:, :), allocatable, intent(in) :: P
    real(wp), dimension(:, :), allocatable, intent(in) :: expnts
    real(wp), dimension(:, :), allocatable, intent(in) :: xyz
    integer, dimension(:), allocatable, intent(in) :: bf_atom_map
    integer, intent(in) :: ng
    real(wp), intent(out) :: rho

    ! variables for routine
    real(wp), parameter :: pi = 4.0_wp*atan(1.0_wp)
    integer :: i, j, k, l, dim, alloc_stat
    real(wp) :: kab
    real(wp), dimension(:), allocatable :: alpha
    real(wp), dimension(:), allocatable :: beta
    real(wp), dimension(3) :: ra, rb, rp
    real(wp):: a_plus_b, a_times_b, rab

    dim = get_dim_mat(P)

    allocate (alpha(dim), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (beta(dim), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

    rho = 0.0_wp
    do i = 1, dim
      alpha = expnts(:, i)
      ra = xyz(:, bf_atom_map(i))
      do j = 1, dim
        beta = expnts(:, j)
        rb = xyz(:, bf_atom_map(j))

        !> | R_a - R_b | ** 2
        rab = sum((ra - rb)**2)
        do k = 1, ng
          do l = 1, ng
            a_plus_b = alpha(k) + beta(l)
            a_times_b = alpha(k)*beta(l)
            rp = (alpha(k)*ra + beta(l)*rb)/a_plus_b

            !> prefactor of new Gaussian
            kab = (2.0_wp*a_times_b/(a_plus_b*pi))**0.75_wp* &
                  exp(-a_times_b/a_plus_b*rab)

            !> calculate charge density with new Gaussian
            rho = rho + P(i, j)*kab*exp(-a_plus_b*sum((r - rp)**2))
          end do
        end do
      end do
    end do

  end subroutine charge_density

  subroutine mulliken_analysis(S, P, chrg, bf_atom_map, nat)
    implicit none

    real(wp), dimension(:, :), allocatable, intent(in) :: S
    real(wp), dimension(:, :), allocatable, intent(in) :: P
    real(wp), dimension(:), allocatable, intent(in) :: chrg
    integer, dimension(:), allocatable, intent(in) :: bf_atom_map
    integer, intent(in) :: nat

    ! variables for subroutine
    real(wp), dimension(:), allocatable :: Q ! Mulliken charges
    real(wp), dimension(:, :), allocatable :: temp
    integer :: i, j, dim, alloc_stat

    write (*, "(A20,15X)", advance="no") "Mulliken charges ..."

    dim = get_dim_mat(S)

    allocate (Q(nat), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (temp(dim, dim), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

    temp = matmul(S, P)
    do i = 1, nat
      q(i) = chrg(i)
      do j = 1, dim
        if (i == bf_atom_map(j)) then
          q(i) = q(i) - temp(j, j)
        end if
      end do
    end do

    write (*, "(A4)") "done"
    do i = 1, nat
      write (*, "(A6,I1,28X,F8.5)") " Atom ", i, q(i)
    end do

  end subroutine mulliken_analysis

  subroutine check_normalization(S, P, nel)
    implicit none
    intrinsic :: abs

    real(wp), dimension(:, :), allocatable, intent(in) :: S
    real(wp), dimension(:, :), allocatable, intent(in) :: P
    integer :: nel

    ! variables for routine
    integer :: i, j, dim
    real(wp) :: sum

    !> thresholds for two reals being equal
    real(wp), parameter :: TOL_EQ = 1.0e-8_wp

    write (*, "(A23,12X)", advance="no") "Check normalization ..."

    dim = get_dim_mat(S)

    sum = 0.0_wp
    do i = 1, dim
      do j = 1, dim
        sum = sum + P(i, j)*S(i, j)
      end do
    end do

    if (abs(sum - real(nel, wp)) < TOL_EQ) then
      write (*, "(A)") "done"
    else
      write (*, "(A)") "failed"
    end if

    write (*, "(A24,11X,I1)") " Expected # of electrons", nel
    write (*, "(A26,9X,F3.1)") " Calculated # of electrons", sum
  end subroutine check_normalization

  subroutine scf_loop(nbf, T, V, two_ints, X, n_occ, P, escf, &
                      MAX_SCF, TOL_SCF, print_level)
    implicit none

    integer, intent(in) :: nbf
    integer, intent(in) :: MAX_SCF
    real(wp), intent(in) :: TOL_SCF
    real(wp), dimension(:, :), allocatable, intent(in) :: T
    real(wp), dimension(:, :), allocatable, intent(in) :: V
    real(wp), dimension(:, :), allocatable, intent(in) :: X
    real(wp), dimension(:, :, :, :), allocatable, intent(in) :: two_ints
    integer, dimension(:, :), allocatable, intent(in) :: n_occ
    integer, optional, intent(in) :: print_level
    real(wp), dimension(:, :), allocatable, intent(inout) :: P
    real(wp), intent(inout) :: escf

    ! variables for routine
    integer :: i
    real(wp) :: escf_old, ediff

    do i = 1, MAX_SCF
      write (*, "(A10,I2,A1,2X)", advance="no") "Iteration ", i, ":"
      escf_old = escf
      call iter_step(nbf, T, V, two_ints, X, n_occ, P, escf, &
                     print_level, is_initial=.false.)

      !> calc diff between iterations, print it and exit if below threshold
      ediff = abs(escf - escf_old)
      write (*, "(2X,A9,F15.10)") "change =", ediff
      if (ediff < TOL_SCF) exit
    end do

    !> exit if number of iteration exceeds maximum
    if (i == MAX_SCF) then
      write (*, "(A)") "Maximum number of iterations reached."
      write (*, "(A)") "Energy not converged!"
      error stop 1
    end if

		write(*, *) ""
    write (*, "(A20,I2,A8)") "SCF converged after ", i, " cycles!"
  end subroutine scf_loop

  subroutine iter_step(nbf, T, V, two_ints, X, n_occ, P, escf, print_level, is_initial)
    implicit none
    integer, intent(in) :: nbf
    real(wp), intent(in), dimension(:, :), allocatable :: T
    real(wp), intent(in), dimension(:, :), allocatable :: V
    real(wp), intent(in), dimension(:, :, :, :), allocatable :: two_ints
    real(wp), intent(in), dimension(:, :), allocatable :: X
    integer, intent(in), dimension(:, :), allocatable :: n_occ
    integer, intent(in), optional :: print_level
    logical, intent(in), optional :: is_initial
    real(wp), intent(inout), dimension(:, :), allocatable :: P
    real(wp), intent(out) :: escf

    ! variables for routine
    real(wp), dimension(:, :), allocatable :: F ! Fock matrix
    real(wp), dimension(:, :), allocatable :: F_prime ! transformed Fock matrix
    real(wp), dimension(:), allocatable :: F_prime_packed ! packed F'
    real(wp), dimension(:), allocatable :: eps ! eigenvalues Fock matrix
    real(wp), dimension(:, :), allocatable :: C ! coefficient matrix
    integer :: alloc_stat, stat_lapack, i, j, k, l

    allocate (F(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (F_prime(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (F_prime_packed(nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (C(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (eps(nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

    !print
    if (present(print_level)) then
      if (print_level >= 1) then
        write (*, "(A24,16X)", advance="no") "Building Fock matrix ..."
      end if
    end if

    !> build initial Fock matrix: F = H0 = T + V
    F = T + V
    if (present(is_initial)) then
      if (is_initial .eqv. .false.) then
        do i = 1, nbf
          do j = 1, nbf
            do k = 1, nbf
              do l = 1, nbf
                F(i, j) = F(i, j) + P(k, l)*( &
                          two_ints(i, j, l, k) - 0.5_wp*two_ints(i, l, j, k))
              end do
            end do
          end do
        end do
      end if
    end if

    ! print
    if (present(print_level)) then
      if (print_level >= 1) then
        write (*, *) "done"
        if (print_level >= 2) then
          call write_matrix(F, name="Fock matrix")
          write (*, *) ""
        end if
        write (*, "(A28,12X)", advance="no") "Transforming Fock matrix ..."
      end if
    end if

    !> transform Fock matrix: F' = X**T * F * X
    F_prime = matmul(matmul(transpose(X), F), X)

    ! print
    if (present(print_level)) then
      if (print_level >= 1) then
        write (*, *) "done"
        write (*, "(A29,11X)", advance="no") "Diagonalizing Fock matrix ..."
      end if
    end if

    !> pack Fock matrix for solve_spev
    call pack_matrix(F_prime, F_prime_packed)

    !> diagonalize Fock matrix, i.e. solve F'C' = C'e
    call solve_spev(F_prime_packed, eps, C, stat_lapack)
    if (stat_lapack /= 0) then
      write (*, *) "Lapack error: ", stat_lapack
      error stop 1
    end if

    ! print
    if (present(print_level)) then
      if (print_level >= 1) then
        write (*, *) "done"
        if (print_level >= 2) then
          call write_vector(eps, name="eigenvalues Fock matrix")
          write (*, *) ""
        end if
        write (*, "(A30,10X)", advance="no") "Calculating density matrix ..."
      end if

    end if

    !> backtransformation: C = X * C'
    C = matmul(X, C)

    !> P = C * n0 * C**T
    P = matmul(matmul(C, n_occ), transpose(C))

    ! print
    if (present(print_level)) then
      if (print_level >= 1) then
        write (*, *) "done"
      end if
      if (print_level >= 2) then
        call write_matrix(P, name="density matrix P")
        write (*, *) ""
      end if
    end if

    !> calculate SCF energy
    call get_escf(F, T, V, P, escf)
    write (*, "(A7,F15.10)", advance="no") "E_SCF =", escf
  end subroutine iter_step

  subroutine set_n_occ(nbf, nel, n_occ)
    implicit none
    integer, intent(in) :: nbf
    integer, intent(in) :: nel
    integer, intent(out), allocatable, dimension(:, :) :: n_occ

    ! variables for routine
    integer :: alloc_stat, i, temp

    allocate (n_occ(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

    n_occ = 0.0_wp
    temp = nel
    do i = 1, nbf
      n_occ(i, i) = 2
      temp = temp - 2
      if (temp == 0) then
        exit
      end if
    end do
  end subroutine set_n_occ

  subroutine get_escf(F, T, V, P, escf)
    implicit none
    real(wp), intent(in), dimension(:, :), allocatable :: F ! Fock matrix
    real(wp), intent(in), dimension(:, :), allocatable :: P ! density matrix
    real(wp), intent(in), dimension(:, :), allocatable :: T ! kinetic energy
    real(wp), intent(in), dimension(:, :), allocatable :: V ! V_eN
    real(wp), intent(out) :: escf

    ! variables for subroutine
    integer :: dim
    integer :: i, alloc_stat
    real(wp) :: trace
    real(wp), dimension(:, :), allocatable :: temp !

    !> get shape of matrix
    dim = get_dim_mat(F)

    !> matrix of which we need trace: (F + H0) * P
    allocate (temp(dim, dim), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    temp = matmul(F + T + V, P)

    !> calculate trace
    trace = 0.0_wp
    do i = 1, dim
      trace = trace + temp(i, i)
    end do

    !> E_HF = 0.5 * Tr{ (F + H0) * P }
    escf = 0.5_wp*trace
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
    real(wp), intent(out), dimension(:, :), allocatable :: S
    real(wp), intent(out), dimension(:, :), allocatable :: T
    real(wp), intent(out), dimension(:, :), allocatable :: V

    ! variables for calculation
    real(wp) :: sab, vab, tab
    integer :: i, j, alloc_stat

    allocate (S(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (T(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (V(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

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
      do j = 1, nbf
        do k = 1, nbf
          do l = 1, nbf
            if (i > j) then
              ! write(*, *) "i > j", i, j, k, l, two_ints(j, i, k, l)
              two_ints(i, j, k, l) = two_ints(j, i, k, l)
            else if (k > l) then
              ! write(*, *) "k > l", i, j, k, l, two_ints(i, j, l, k)
              two_ints(i, j, k, l) = two_ints(i, j, l, k)
              ! else if (i*(i + 1)*0.5 + j > k*(k + 1)*0.5 + l) then
              !    write(*, *) "ij>kl", i, j, k, l, two_ints(k, l, i, j)
              !   two_ints(i, j, k, l) = two_ints(k, l, i, j)
            else
              call twoint( &
                xyz(:, bf_atom_map(i)), xyz(:, bf_atom_map(j)), &
                xyz(:, bf_atom_map(k)), xyz(:, bf_atom_map(l)), &
                expnts(:, i), expnts(:, j), &
                expnts(:, k), expnts(:, l), &
                coeffs(:, i), coeffs(:, j), &
                coeffs(:, k), coeffs(:, l), &
                tei)
              two_ints(i, j, k, l) = tei
              ! write (*, *) "call ", i, j, k, l, tei
            end if
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

  subroutine read_file(input, xyz, chrg, zeta, bf_atom_map, nat, nel, nbf)
    implicit none

    ! in and out variables
    integer, intent(in) :: input
    real(wp), allocatable, intent(out) :: xyz(:, :)
    real(wp), allocatable, intent(out) :: chrg(:)
    real(wp), allocatable, intent(out) :: zeta(:)
    integer, allocatable, intent(out) :: bf_atom_map(:)
    integer, intent(out) :: nat
    integer, intent(out) :: nel
    integer, intent(out) :: nbf

    ! variables for calculation
    integer :: io_stat, alloc_stat, i, j
    integer :: nbf_of_atom
    integer :: counter
    counter = 1

    ! read first line containing info vor allocation
    read (input, *, iostat=io_stat) nat, nel, nbf
    if (io_stat /= 0) error stop 1

    ! memory allocation of arrays
    allocate (zeta(nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (chrg(nat), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (xyz(3, nat), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (bf_atom_map(nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

    ! read rest of file
    ! iterate over number of atoms
    do i = 1, nat
      read (input, *, iostat=io_stat) xyz(:, i), chrg(i), nbf_of_atom
      if (io_stat /= 0) exit

      ! iterate over number of basis functions of this atom
      do j = 1, nbf_of_atom
        read (input, *, iostat=io_stat) zeta(counter)
        if (io_stat /= 0) exit
        bf_atom_map(counter) = i
        counter = counter + 1
      end do
    end do
  end subroutine
end module scf_main

