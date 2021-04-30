module properties
  !> small helper functions for matrices
  use array_funcs, only: get_dim_mat

  implicit none

  private
  public :: mulliken_analysis, check_normalization

  integer, parameter :: wp = selected_real_kind(15)

contains

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

end module properties
