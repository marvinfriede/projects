program main
  implicit none

  integer :: a, b, res

  write(*, *) "Enter integer."
  read(*, *) a, b

  write(*, *) "First number is ", a
  write(*, *) "Second number is ", b

  res = a + b

  print *, "Result is ", res
end program main
