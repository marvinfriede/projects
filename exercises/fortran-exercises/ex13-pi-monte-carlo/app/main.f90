program main
  implicit none
  intrinsic :: selected_real_kind

  integer, parameter :: wp = selected_real_kind(15)
  real(wp), parameter :: PI = 4.0_wp * DATAN(1.0_wp) ! exact value 
  real(wp), dimension(2) :: x
  integer :: n_in_circle = 0, n_attemps = 100000
  integer :: i

  do i = 1, n_attemps
    call random_number(x) ! generates a number in [0, 1)
    if ( x(1)*x(1) + x(2)*x(2) < 1) then
      n_in_circle = n_in_circle + 1
    end if
  end do

  write(*, *) "Pi (appro) = ", 4 * real(n_in_circle, wp) / real(n_attemps, wp)
  write(*, *) "Pi (exact) = ", PI

end program main


! probability of point being in circle:
! area of circle / area of square = pi*r² / 4*r² = pi / 4
!
! point is in circle if x² + y² < r² (r = 1)