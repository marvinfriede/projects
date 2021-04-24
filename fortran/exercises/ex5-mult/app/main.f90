program main
  implicit none

  integer :: a, b, res

  ! create integer overflow
  a = 10000000
  b = 10000000

  write(*, *) "First number is ", a
  write(*, *) "Second number is ", b

  res = a * b

  print *, "Result is ", res

end program main
