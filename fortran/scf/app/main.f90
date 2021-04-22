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
  !  coefficients and primitive exponents are taken from R.F. Stewart, JCP, 1970
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
  subroutine scf_prog(input)

    !> Always declare everything explicitly
    implicit none

    !> IO unit bound to the input file
    integer, intent(in) :: input

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

    !> Slater exponents of basis functions
    real(wp), allocatable :: zeta(:)

    !> maps basis function to atom
    !> e.g.: (1, 1, 2) -> 1st bf from A1, 2nd bf from A1, 3rd bf from A3
    integer, allocatable :: bf_atom_map(:)

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !> energies
    !> scf energy
    !real(wp) :: escf

    !> nuclear repulsion energy
    real(wp) :: enn

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !> number of primitive Gaussians in slater expansion (STO-nG)
    integer, parameter :: ng = 6

    !> exponents of primitive Gaussian functions
    real(wp), dimension(:, :), allocatable :: exponents

    !> coefficients of primitive Gaussian functions
    real(wp), dimension(:, :), allocatable :: coefficients

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real(wp), dimension(:, :), allocatable :: sab_mat, tab_mat, vab_mat
    real(wp), dimension(:), allocatable :: sab_mat_packed, tab_mat_packed, vab_mat_packed

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real(wp), dimension(:, :), allocatable :: X

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !> init all variables from input file
    write (*, *) ""
    call read_file(input, xyz, chrg, zeta, bf_atom_map, nat, nel, nbf)

    !> nuclear repulsion energy
    call get_enn(xyz, chrg, nat, enn)
    write (*, 100) "Nuclear repulsion energy:", enn, " Hartree"
100 format(A25, F15.10, A8)

    !> basis set setup
    call expand_slater_wrapper(nbf, ng, zeta, exponents, coefficients)

    !> one electron integrals
    call get_oneint(nbf, xyz, chrg, bf_atom_map, exponents, coefficients, &
                    sab_mat, vab_mat, tab_mat)

    !> pack matrices if symmetric
    call pack_matrix(sab_mat, sab_mat_packed)
    call pack_matrix(vab_mat, vab_mat_packed)
    call pack_matrix(tab_mat, tab_mat_packed)

    ! symmetric orthonormalizer
    ! ---> check matrix being overwritten <---
    call sym_orthonormalizer(sab_mat_packed, nbf, X)
    call write_matrix(X, name="X")
    call write_matrix(matmul(matmul(transpose(X), sab_mat), X), &
                      name="X^T * S * X = 1")

    ! some printing
    ! call write_matrix(sab_mat, name="sab")
    ! call write_matrix(sab_mat_packed, name="packed mat")
    ! call write_vector(sab_mat_packed, name="packed mat")

    ! call write_matrix(tab_mat, name="tab")
    ! call write_matrix(vab_mat, name="vab")
    ! call write_matrix(xyz, name='Coordinates')
    ! call write_vector(chrg, name="Charges")
    ! call write_vector(real(nbf, wp), name="Number of Basis functions each atom")
    ! call write_vector(zeta, name="Slater exponents")
    ! call write_vector(real(bf_atom_map, wp), name="bf")
    ! call write_matrix(coefficients, name="Gaussian coefficients")
    ! call write_matrix(exponents, name="Gaussian exponents")

    !  Write the self-consistent field procedure in a subroutine.
    write (output_unit, '(a)') 'Here could start a Hartree-Fock calculation'

    deallocate (zeta)
    deallocate (chrg)
    deallocate (xyz)
    deallocate (exponents)
    deallocate (coefficients)
  end subroutine scf_prog

  subroutine sym_orthonormalizer(sab_mat_packed, nbf, X)
    implicit none

    intrinsic :: matmul, transpose
    integer, intent(in) :: nbf
    real(wp), intent(inout), dimension(:), allocatable :: sab_mat_packed
    real(wp), intent(out), dimension(:, :), allocatable :: X

    ! variables for routine
    integer :: i, stat_lapack, alloc_stat
    real(wp), dimension(:), allocatable :: eigval
    real(wp), dimension(:, :), allocatable :: eigvec
    real(wp), dimension(:, :), allocatable :: s

    !> allocation
    allocate (eigval(nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (eigvec(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (s(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (X(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

    !> call routine on packed matrix
    call solve_spev(sab_mat_packed, eigval, eigvec, stat_lapack)
    if (stat_lapack /= 0) then
      write (*, *) "Lapack error: ", stat_lapack
      error stop 1
    end if

    ! form s matrix
    s = 0.0_wp
    do i = 1, nbf
      s(i, i) = sqrt(1/eigval(i))
    end do

    ! calculate X = U s**(-1/2) U**T
    X = matmul(matmul(eigvec, s), transpose(eigvec))
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

    if (is_mat_symmetric(mat, dim) .eqv. .true.) then
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
    end if
  end subroutine pack_matrix

  function is_mat_symmetric(mat, dim) result(is_symmetric)
    intrinsic :: abs

    real(wp), intent(in), dimension(:, :), allocatable :: mat
    integer, intent(in) :: dim
    logical :: is_symmetric
    integer :: i, j

    !> thresholds for two reals being equal
    real(wp), parameter :: TOL_EQ = 1.0e-8_wp

    outer: do i = 1, dim
      inner: do j = 1, i
        if (i /= j) then
          if (abs(mat(i, j) - mat(j, i)) > TOL_EQ) then
            is_symmetric = .false.
            exit outer
          end if
        end if
      end do inner
    end do outer
  end function is_mat_symmetric

  subroutine get_oneint(nbf, xyz, chrg, bf_atom_map, exponents, &
                        coefficients, sab_mat, vab_mat, tab_mat)
    implicit none

    integer, intent(in) :: nbf
    real(wp), intent(in), dimension(:, :), allocatable :: xyz
    real(wp), intent(in), dimension(:), allocatable :: chrg
    integer, intent(in), dimension(:), allocatable :: bf_atom_map
    real(wp), intent(in), dimension(:, :), allocatable :: exponents
    real(wp), intent(in), dimension(:, :), allocatable :: coefficients
    real(wp), intent(out), dimension(:, :), allocatable :: sab_mat
    real(wp), intent(out), dimension(:, :), allocatable ::tab_mat
    real(wp), intent(out), dimension(:, :), allocatable :: vab_mat

    ! variables for calculation
    real(wp) :: sab, vab, tab
    integer :: i, j, alloc_stat

    allocate (sab_mat(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (tab_mat(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (vab_mat(nbf, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

    do i = 1, nbf
      do j = 1, nbf
        ! xyz(:, i) is wrong
        call oneint(xyz, chrg, &
                    xyz(:, bf_atom_map(i)), xyz(:, bf_atom_map(j)), &
                    exponents(:, i), exponents(:, j), &
                    coefficients(:, i), coefficients(:, j), &
                    sab, tab, vab)
        sab_mat(i, j) = sab
        tab_mat(i, j) = tab
        vab_mat(i, j) = vab
      end do
    end do

  end subroutine get_oneint

  subroutine expand_slater_wrapper(nbf, ng, zeta, exponents, coefficients)
    implicit none
    integer, intent(in) :: nbf
    integer, intent(in) :: ng
    real(wp), intent(in), dimension(:), allocatable ::  zeta
    real(wp), intent(out), dimension(:, :), allocatable :: exponents
    real(wp), intent(out), dimension(:, :), allocatable :: coefficients
    integer :: i, alloc_stat

    allocate (exponents(ng, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop "Allocation failed."
    allocate (coefficients(ng, nbf), stat=alloc_stat)
    if (alloc_stat /= 0) error stop "Allocation failed."

    do i = 1, nbf
      call expand_slater(ng, zeta(i), exponents(:, i), coefficients(:, i))
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

