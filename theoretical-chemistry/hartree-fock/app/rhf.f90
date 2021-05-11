!> This is your module to ! writeyour very own SCF program.
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
  !  example: call read_line(input, line)
  use io_tools, only: read_line

  !> outsourced subroutines
  use array_funcs
  use mp2
  use properties

  !> Always declare everything explicitly
  implicit none

  !> All subroutines within this module are not exported, except for scf_prog
  !  which is the entry point to your program
  private
  public :: scf_prog, opt_coords, opt_expnts, wp

  !> Selecting double precision real number
  integer, parameter :: wp = selected_real_kind(15)

contains

!> This is the entry point to your program, do not modify the dummy arguments
!  without adjusting the call in lib/prog.f90
  subroutine scf_prog(nat, nel, nbf, ng, xyz, chrg, zeta, bf_atom_map, &
                      ehf, MAX_SCF, TOL_SCF, do_mp2, print_level)
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

    !> do MP2 energy calculation at end
    integer, intent(in), optional :: do_mp2

    !> HF energy
    real(wp), intent(out) :: ehf
    real(wp) :: emp2

    real(wp), allocatable, dimension(:) :: eps

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !> energies
    !> nuclear repulsion energy
    real(wp) :: enn
    real(wp) :: escf

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
    real(wp), dimension(:, :), allocatable :: P ! density matrix
    real(wp), dimension(:, :), allocatable :: C ! coeffs matrix
    integer, dimension(:, :), allocatable :: n_occ ! occupation matrix

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    allocate (P(nbf, nbf), stat=alloc_stat)
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

    !> build (diagonal) occupation matrix
    call set_n_occ(nbf, nel, n_occ)
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

    !> get initial density matrix
    call iter_step(nbf, T, V, two_ints, X, n_occ, P, C, eps, escf, &
                   print_level, 1)

    !*********************************************************
    !******************* SCF PROCEDURE ***********************
    !*********************************************************

    if (present(print_level) .and. print_level >= 1) then
      write (*, 101)
      write (*, "(A)") "SCF Iterations"
      write (*, 102)
    end if

    !> scf loop
    call scf_loop(nbf, T, V, two_ints, X, n_occ, P, C, eps, escf, &
                  MAX_SCF, TOL_SCF, print_level)
    ehf = escf + enn

    !*********************************************************
    !********************** PROPERTIES ***********************
    !*********************************************************

    if (present(print_level) .and. print_level >= 1) then
      write (*, 101)
      write (*, "(A)") "Properties"
      write (*, 102)
      call check_normalization(S, P, nel)
      call mulliken_analysis(S, P, chrg, bf_atom_map, nat)
    end if

    !*********************************************************
    !********************** MP2 ENERGY ***********************
    !*********************************************************

    if (present(do_mp2) .and. do_mp2 == 1) then
      if (present(print_level) .and. print_level >= 1) then
        write (*, 101)
        write (*, "(A)") "MP2"
        write (*, 102)
      end if

      call mp2_5(two_ints, C, nel, eps, emp2, print_level)
      ! call mp2_8(two_ints, C, nel, eps, emp2, print_level)
    end if

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
      if (present(do_mp2) .and. do_mp2 == 1) then
        write (*, "(A14,21X,F15.10)") "MP2 energy ...", emp2
        write (*, "(A16,19X,F15.10)") "Total energy ...", ehf + emp2
      else
        write (*, "(A16,19X,F15.10)") "Total energy ...", ehf
      end if
    end if

    ! call scan_charge_density(P, expnts, xyz, bf_atom_map, ng)
  end subroutine scf_prog

!*****************************************************************************
!**************************** END MAIN FUNCTION ******************************
!*****************************************************************************

  subroutine scf_loop(nbf, T, V, two_ints, X, n_occ, P, C, eps, escf, &
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
    real(wp), dimension(:, :), allocatable, intent(out) :: C
    real(wp), intent(out), dimension(:), allocatable :: eps
    real(wp), intent(inout) :: escf

    ! variables for routine
    integer :: i
    real(wp) :: escf_old, ediff

    do i = 1, MAX_SCF
      if (present(print_level) .and. print_level >= 1) then
        write (*, "(A10,I2,A1,2X)", advance="no") "Iteration ", i, ":"
      end if

      escf_old = escf
      call iter_step(nbf, T, V, two_ints, X, n_occ, P, C, eps, escf, &
                     print_level, 0)

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

  subroutine iter_step(nbf, T, V, two_ints, X, n_occ, P, C, eps, escf, print_level, is_initial)
    implicit none
    integer, intent(in) :: nbf
    real(wp), intent(in), dimension(:, :), allocatable :: T
    real(wp), intent(in), dimension(:, :), allocatable :: V
    real(wp), intent(in), dimension(:, :, :, :), allocatable :: two_ints
    real(wp), intent(in), dimension(:, :), allocatable :: X
    integer, intent(in), dimension(:, :), allocatable :: n_occ
    integer, intent(in), optional :: print_level
    integer, intent(in), optional :: is_initial
    real(wp), intent(inout), dimension(:, :), allocatable :: P
    real(wp), intent(out), dimension(:, :), allocatable :: C
    real(wp), intent(out), dimension(:), allocatable :: eps ! eigvals Fock matrix
    real(wp), intent(out) :: escf

    ! variables for routine
    real(wp), dimension(:, :), allocatable :: F ! Fock matrix
    real(wp), dimension(:, :), allocatable :: F_prime ! transformed Fock matrix
    real(wp), dimension(:), allocatable :: F_prime_packed ! packed F'
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

    !> print
    if (present(print_level)) then
      if (print_level >= 2) then
        write (*, *) ""
        write (*, "(A24,16X)", advance="no") "Building Fock matrix ..."
      end if
    end if

    !> build initial Fock matrix: F = H0 = T + V
    F = T + V
    if (present(is_initial) .and. is_initial == 0) then
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

    !> print
    if (present(print_level)) then
      if (print_level >= 2) then
        write (*, *) "done"
        if (print_level >= 3) then
          call write_matrix(F, name="Fock matrix")
          write (*, *) ""
        end if
        write (*, "(A28,12X)", advance="no") "Transforming Fock matrix ..."
      end if
    end if

    !> transform Fock matrix: F' = X**T * F * X (X symmetric: X**T = X)
    F_prime = matmul(matmul(transpose(X), F), X)

    !> print
    if (present(print_level)) then
      if (print_level >= 2) then
        write (*, *) "done"
        if (print_level >= 3) then
          call write_matrix(F_prime, name="transformed Fock matrix")
          write (*, *) ""
        end if
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

    !> print
    if (present(print_level)) then
      if (print_level >= 2) then
        write (*, *) "done"
        if (print_level >= 3) then
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

    !> print
    if (present(print_level)) then
      if (print_level >= 2) then
        write (*, *) "done"
      end if
      if (print_level >= 3) then
        call write_matrix(P, name="density matrix P")
        write (*, *) ""
      end if
    end if

    !> calculate SCF energy
    call get_escf(F, T, V, P, escf)
    if (present(print_level) .and. print_level >= 1) then
      write (*, "(A7,F15.10)", advance="no") "E_SCF =", escf
    end if
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
          do l = 1, merge(j, k, i == k) ! l to j, except if i=k, then l to k
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

!*****************************************************************************
!******************************** OPTIMIZERS *********************************
!*****************************************************************************

  subroutine opt_coords(nat, nel, nbf, ng, xyz, chrg, zeta, bf_atom_map, &
                        MAX_SCF, TOL_SCF, MAX_OPT_GEOM, TOL_OPT, ETA, &
                        COORD_STEP_SIZE, print_level)
    implicit none

    integer, intent(in) :: nat
    integer, intent(in) :: nel
    integer, intent(in) :: nbf
    integer, intent(in) :: ng
    real(wp), intent(inout), dimension(:, :), allocatable :: xyz
    real(wp), intent(in), dimension(:), allocatable :: chrg
    real(wp), intent(in), dimension(:), allocatable :: zeta
    integer, intent(in), dimension(:), allocatable :: bf_atom_map
    integer, intent(in) :: MAX_SCF
    integer, intent(in) :: MAX_OPT_GEOM
    real(wp), intent(in) :: TOL_SCF
    real(wp), intent(in) :: TOL_OPT
    real(wp), intent(in) :: COORD_STEP_SIZE
    real(wp), intent(in) :: ETA
    integer, optional, intent(in) :: print_level

    ! variables for routine
    real(wp), allocatable, dimension(:, :) :: temp
    real(wp), allocatable, dimension(:, :) :: grad_coords
    integer :: alloc_stat, i, j, k
    real(wp) :: ehf1, ehf2, eta_new

    allocate (temp(3, nat), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (grad_coords(3, nat), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

    write (*, "(A)") "Starting iterations..."
    eta_new = ETA
    steep: do k = 1, MAX_OPT_GEOM
      grad_outer: do i = 1, 3
        grad_inner: do j = 1, nat
          ! reset coordinates because only one value changes
          temp = xyz
          temp(i, j) = xyz(i, j) + COORD_STEP_SIZE
          call scf_prog(nat, nel, nbf, ng, temp, chrg, zeta, &
                        bf_atom_map, ehf1, MAX_SCF, TOL_SCF, 0, 0)

          temp(i, j) = xyz(i, j) - COORD_STEP_SIZE
          call scf_prog(nat, nel, nbf, ng, temp, chrg, zeta, &
                        bf_atom_map, ehf2, MAX_SCF, TOL_SCF, 0, 0)

          grad_coords(i, j) = (ehf2 - ehf1)/(2*COORD_STEP_SIZE)
        end do grad_inner
      end do grad_outer

      ! calculate new coordinates
      xyz = xyz + eta_new*grad_coords

      if (present(print_level) .and. print_level >= 1) then
        write (*, "(A10,I3,A1,2X)", advance="no") "Iteration ", k, ":"
        write (*, "(A15,F15.10)") "RMS(gradient) =", rms(grad_coords)
      end if

      ! check if converged
      if (rms(grad_coords) < TOL_OPT) exit steep

      !> exit if number of iteration exceeds maximum
      if (k >= MAX_OPT_GEOM) then
        write (*, "(A)") "Maximum number of iterations reached."
        write (*, "(A)") "Geometry not converged!"
        error stop 1
      end if

      ! "dynamic" learning rate for better convergence
      if (k == 10) then
        eta_new = 1.0_wp
      else if (k == 20) then
        eta_new = 5.0_wp
      else if (k == 50) then
        eta_new = 10.0_wp
      else if (k == 100) then
        eta_new = 20.0_wp
      end if

    end do steep

    write (*, "(A)") ""
    write (*, "(A)") "Stationary point found!"
    write (*, "(A36,I3,A7)") "Geometry optimization finished after ", &
      k, " steps."
    write (*, "(A)") "Performing final energy evaluation on optimized coordinates."
    if (present(print_level) .and. print_level >= 2) then
      call write_matrix(xyz, name="Optimized coordinates")
    end if

  end subroutine opt_coords

  subroutine opt_expnts(nat, nel, nbf, ng, xyz, chrg, zeta, bf_atom_map, &
                        MAX_SCF, TOL_SCF, MAX_OPT_EXP, TOL_OPT, ETA, &
                        EXPNTS_STEP_SIZE, print_level)
    implicit none

    integer, intent(in) :: nat
    integer, intent(in) :: nel
    integer, intent(in) :: nbf
    integer, intent(in) :: ng
    real(wp), intent(in), dimension(:, :), allocatable :: xyz
    real(wp), intent(in), dimension(:), allocatable :: chrg
    real(wp), intent(inout), dimension(:), allocatable :: zeta
    integer, intent(in), dimension(:), allocatable :: bf_atom_map
    integer, intent(in) :: MAX_SCF
    integer, intent(in) :: MAX_OPT_EXP
    real(wp), intent(in) :: TOL_SCF
    real(wp), intent(in) :: TOL_OPT
    real(wp), intent(in) :: EXPNTS_STEP_SIZE
    real(wp), intent(in) :: ETA
    integer, optional, intent(in) :: print_level

    ! variables for routine
    real(wp), allocatable, dimension(:) :: temp_expnts
    real(wp), allocatable, dimension(:) :: grad_expnts
    integer :: alloc_stat, i, k
    real(wp) :: ehf1, ehf2, eta_new

    allocate (temp_expnts(nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (grad_expnts(nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

    write (*, "(A)") "Starting iterations..."
    eta_new = ETA
    steep_expnts: do k = 1, MAX_OPT_EXP
      expnts: do i = 1, nbf
        temp_expnts = zeta
        temp_expnts(i) = zeta(i) + EXPNTS_STEP_SIZE
        call scf_prog(nat, nel, nbf, ng, xyz, chrg, temp_expnts, &
                      bf_atom_map, ehf1, MAX_SCF, TOL_SCF, 0, 0)

        temp_expnts(i) = zeta(i) - EXPNTS_STEP_SIZE
        call scf_prog(nat, nel, nbf, ng, xyz, chrg, temp_expnts, &
                      bf_atom_map, ehf2, MAX_SCF, TOL_SCF, 0, 0)

        grad_expnts(i) = (ehf2 - ehf1)/(2*EXPNTS_STEP_SIZE)
      end do expnts

      ! calculate new exponents
      zeta = zeta + eta*grad_expnts

      if (present(print_level) .and. print_level >= 1) then
        write (*, "(A10,I3,A1,2X)", advance="no") "Iteration ", k, ":"
        write (*, "(A15,F15.10)") "RMS(exponents) =", rms_vec(grad_expnts)
      end if

      ! check if converged
      if (rms_vec(grad_expnts) < TOL_OPT) exit steep_expnts

      !> exit if number of iteration exceeds maximum
      if (k >= MAX_OPT_EXP) then
        write (*, "(A)") "Maximum number of iterations reached."
        write (*, "(A)") "Geometry not converged!"
        error stop 1
      end if

      ! "dynamic" learning rate for better convergence
      if (k == 10) then
        eta_new = 1.0_wp
      else if (k == 20) then
        eta_new = 5.0_wp
      else if (k == 30) then
        eta_new = 10.0_wp
      else if (k == 50) then
        eta_new = 20.0_wp
      else if (k == 100) then
        eta_new = 30.0_wp
      else if (k == 150) then
        eta_new = 50.0_wp
      else if (k == 200) then
        eta_new = 100.0_wp
      end if
    end do steep_expnts

    write (*, "(A)") ""
    write (*, "(A)") "Stationary point found!"
    write (*, "(A36,I3,A7)") "Exponent optimization finished after ", &
      k, " steps."
    write (*, "(A)") "Performing final energy evaluation on optimized coordinates."
    if (present(print_level) .and. print_level >= 2) then
      call write_matrix(xyz, name="Optimized exponents")
    end if
  end subroutine opt_expnts

!*****************************************************************************
!****************************** CHARGE DENSITY *******************************
!*****************************************************************************

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
    ! psi(r) ... molecular orbital (occupied)
    ! phi(r) ... basis functions
    ! C      ... expansion coefficients
    !
    !              N/2                     N/2
    ! rho(r) = 2 * sum |psi_a(r)|**2 = 2 * sum (psi_a(r) * psi_a(r))
    !                                             a                       a
    !
    !              N/2
    ! rho(r) = 2 * sum [ sum (C_mu,a * phi_mu(r)) * sum (C_nu,a * phi_nu(r)) ]
    !                                             a    mu                         nu
    !
    !                       N/2
    ! rho(r) = sum sum [2 * sum (C_mu,a * C_nu,a) ] phi_mu(r) * phi_nu(r))
    !          mu  nu        a
    !
    !
    ! rho(r) = sum sum (P_nu,mu * phi_mu(r) * phi_nu(r))
    !          mu  nu
    !
    ! then use Gaussian product theorem

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

end module scf_main

