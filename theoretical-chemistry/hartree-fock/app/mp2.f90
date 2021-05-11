module mp2
  !> small helper functions for matrices
  use array_funcs, only: get_dim_mat

  implicit none

  private
  public :: mp2_5, mp2_8

  integer, parameter :: wp = selected_real_kind(15)

contains

  subroutine mp2_5(two_ints, C, nel, eps, emp2, print_level)
    implicit none
    real(wp), intent(in), allocatable, dimension(:, :, :, :) :: two_ints
    real(wp), intent(in), allocatable, dimension(:, :) :: C
    real(wp), intent(in), allocatable, dimension(:) :: eps
    integer, intent(in) :: nel
    integer, intent(in), optional :: print_level
    real(wp), intent(out) :: emp2

    !> variables for routine
    integer :: alloc_stat, i, j, k, l, r, s, t, u
    integer :: dim, nel_half
    real(wp), allocatable, dimension(:, :, :, :):: mo, temp

    if (present(print_level) .and. print_level >= 1) then
      write (*, "(A27,8X)", advance="no") "Transforming AOs to MOs ..."
    end if

    ! variables for convenience
    dim = get_dim_mat(C)
    nel_half = int(nel*0.5)

    allocate (mo(dim, dim, dim, dim), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1
    allocate (temp(dim, dim, dim, dim), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

    ! AO to MO transformation (scaling with M⁵)
    ! trafo 1
    mo = 0.0_wp
    do i = 1, dim
      do j = 1, dim
        do k = 1, dim
          do l = 1, dim
            do r = 1, dim
              mo(i, j, k, l) = mo(i, j, k, l) + C(r, l)*two_ints(i, j, k, r)
            end do
          end do
        end do
      end do
    end do

    ! trafo 2
    temp = mo
    mo = 0.0_wp
    do i = 1, dim
      do j = 1, dim
        do k = 1, dim
          do l = 1, dim
            do s = 1, dim
              mo(i, j, k, l) = mo(i, j, k, l) + C(s, k)*temp(i, j, s, l)
            end do
          end do
        end do
      end do
    end do

    ! trafo 3
    temp = mo
    mo = 0.0_wp
    do i = 1, dim
      do j = 1, dim
        do k = 1, dim
          do l = 1, dim
            do t = 1, dim
              mo(i, j, k, l) = mo(i, j, k, l) + C(t, j)*temp(i, t, k, l)
            end do
          end do
        end do
      end do
    end do

    ! trafo 4
    temp = mo
    mo = 0.0_wp
    do i = 1, dim
      do j = 1, dim
        do k = 1, dim
          do l = 1, dim
            do u = 1, dim
              mo(i, j, k, l) = mo(i, j, k, l) + C(u, i)*temp(u, j, k, l)
            end do
          end do
        end do
      end do
    end do

    if (present(print_level) .and. print_level >= 1) then
      write (*, "(A4)") "done"
      write (*, "(A26,9X)", advance="no") "Calculating MP2 energy ..."
    end if

    ! MP2 energy calculation
    emp2 = 0.0_wp
    do i = 1, nel_half
      do j = 1, nel_half
        do k = nel_half + 1, dim
          do l = nel_half + 1, dim
            emp2 = emp2 + mo(i, k, j, l)* &
                   (2.0_wp*mo(i, k, j, l) - mo(i, l, j, k))/ &
                   (eps(i) + eps(j) - eps(k) - eps(l))
          end do
        end do
      end do
    end do

    if (present(print_level) .and. print_level >= 1) then
      write (*, "(A4)") "done"
    end if
  end subroutine mp2_5

  subroutine mp2_8(two_ints, C, nel, eps, emp2, print_level)
    implicit none
    real(wp), intent(in), allocatable, dimension(:, :, :, :) :: two_ints
    real(wp), intent(in), allocatable, dimension(:, :) :: C
    real(wp), intent(in), allocatable, dimension(:) :: eps
    integer, intent(in) :: nel
    integer, intent(in), optional :: print_level
    real(wp), intent(out) :: emp2

    !> variables for routine
    integer :: alloc_stat, i, j, k, l, r, s, t, u
    integer :: dim, nel_half
    real(wp), allocatable, dimension(:, :, :, :):: mo

    if (present(print_level) .and. print_level >= 1) then
      write (*, "(A27,8X)", advance="no") "Transforming AOs to MOs ..."
    end if

    ! variables for convenience
    dim = get_dim_mat(C)
    nel_half = int(nel*0.5)

    allocate (mo(dim, dim, dim, dim), stat=alloc_stat)
    if (alloc_stat /= 0) error stop 1

    ! slow AO to MO transformation (scaling with M⁸)
    mo = 0.0_wp
    do i = 1, dim
      do j = 1, dim
        do k = 1, dim
          do l = 1, dim
            do r = 1, dim
              do s = 1, dim
                do t = 1, dim
                  do u = 1, dim
                    mo(i, j, k, l) = mo(i, j, k, l) + &
                                     C(r, i)*C(s, j)*two_ints(r, s, t, u)* &
                                     C(t, k)*C(u, l)
                  end do
                end do
              end do
            end do
          end do
        end do
      end do
    end do

    if (present(print_level) .and. print_level >= 1) then
      write (*, "(A4)") "done"
      write (*, "(A26,9X)", advance="no") "Calculating MP2 energy ..."
    end if

    ! MP2 energy calculation
    emp2 = 0.0_wp
    do i = 1, nel_half
      do j = 1, nel_half
        do k = nel_half + 1, dim
          do l = nel_half + 1, dim
            emp2 = emp2 + mo(i, k, j, l)*(2.0_wp*mo(i, k, j, l) - &
                                          mo(i, l, j, k))/(eps(i) + eps(j) - eps(k) - eps(l))
          end do
        end do
      end do
    end do

    if (present(print_level) .and. print_level >= 1) then
      write (*, "(A4)") "done"
    end if
  end subroutine mp2_8

end module mp2
