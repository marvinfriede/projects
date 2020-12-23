program test

! forces declaration of all variables
implicit none 

! strings are arrays of chars like in C
character (len =20) :: name

! constant float
real, parameter :: PI = 3.14

! float; only six digits of precision
real :: num1 = 0.0 , num2 = 0.1 

! float; 15 digits of precision; d+0 necessary
double precision :: numDP = 1.11111111111d+0

! integer
integer :: numInt = 1

! booleans
logical :: isReady = .true.

! complex numbers
complex :: numComplex = (2.0, 4.0)

print *, "biggest real = ", huge(num1), " ; smallest real = ", tiny(num2)
print *, "biggest int = ", huge(numInt)

print *, "Integer", kind(numInt)
print *, "Double", kind(numDP)
print *, "Complex", kind(numComplex)
print *, "Boolean", kind(isReady)
print *, "String", kind(name)


	
end program test