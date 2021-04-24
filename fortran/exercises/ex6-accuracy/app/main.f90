program main
  implicit none
  intrinsic :: selected_real_kind ! could be left out

  real :: a, b, c
  integer :: single, double

  ! kind parameter for real variables
  integer, parameter :: wp = selected_real_kind(15)
  real(wp) :: u, v, w ! always requires parameter type

  single = selected_real_kind(6)
  double = selected_real_kind(15)
  write (*, *) "For 6 significant digits", single, "bytes are required"
  write (*, *) "For 15 significant digits", double, "bytes are required"
  write (*, *) ''

  ! only 6 significant digits
  a = 1.0
  b = 6.0
  c = a/b
  write (*, *) 'a is', a
  write (*, *) 'b is', b
  write (*, *) 'c is', c
  write (*, *) ''

  ! correct behaviour
  u = 1.0_wp
  v = 6.0_wp
  w = u/v
  write (*, *) 'u is', u
  write (*, *) 'v is', v
  write (*, *) 'w is', w
  write (*, *) ''

  ! but...
  u = 1.0_wp/6.0_wp
  v = 1.0/6.0
  w = 1/6

  write (*, *) 'u is', u ! expected
  write (*, *) 'v is', v ! calculation with 32bit real -> cast to 64bit real
  write (*, *) 'w is', w ! calculation with integers -> cast to 64bit real

end program main
