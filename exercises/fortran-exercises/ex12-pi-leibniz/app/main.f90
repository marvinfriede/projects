program main
  implicit none
  intrinsic :: selected_real_kind, abs
  integer :: i, N
  integer, parameter :: wp = selected_real_kind(15)
  real(wp), parameter :: PI = 4.0_wp*DATAN(1.0_wp) ! exact value 
  real(wp) :: sum1 = 0.0_wp, sum2 = 0.0_wp, temp
  real(wp) :: tol = 1e-6
  
  N = 1000

  do i = 0, N
    temp = sum1 + (-1.0_wp)**i / (2.0_wp*i + 1.0_wp) 
    if ( abs(sum1 - temp) < tol ) then
      exit
    else 
      sum1 = temp 
    end if
  end do

  do i = 0, N, 2
    sum2 = sum2 + 1.0_wp / (2.0_wp*i + 1.0_wp)
  end do
  do i = 1, N, 2
    sum2 = sum2 - 1.0_wp / (2.0_wp*i + 1.0_wp)
  end do

  write(*, *) "Pi (appro) = ", 4*sum1
  write(*, *) "Pi (appro) = ", 4*sum2
  write(*, *) "Pi (exact) = ", PI
  
end program main
