module array_functions
  implicit none

contains
  function sumVector(vector) result(vecSum)
    intrinsic :: size
    integer, intent(in) :: vector(:)
    integer vecSum, i

    vecSum = 0
    do i = 1, size(vector)
      vecSum = vecSum + vector(i)
    end do
  end function sumVector

  subroutine sumVectorSub(vector, vecSum)
    integer, intent(in) :: vector(:)
    integer, intent(out) :: vecSum
    integer :: i

    vecSum = 0
    do i = 1, size(vector)
      vecSum = vecSum + vector(i)
    end do
  end subroutine sumVectorSub

  function prodVector(vector) result(vecprod)
    intrinsic :: size
    integer, intent(in) :: vector(:)
    integer vecprod, i

    vecprod = 1
    do i = 1, size(vector)
      vecprod = vecprod*vector(i)
    end do
  end function prodVector

  function minVector(vector) result(vecMin)
    intrinsic :: size
    integer, intent(in) :: vector(:)
    integer vecMin, i

    vecMin = vector(1)
    do i = 2, size(vector)
      if (vecMin > vector(i)) then
        vecMin = vector(i)
      end if
    end do
  end function minVector

  function maxVector(vector) result(vecMax)
    intrinsic :: size
    integer, intent(in) :: vector(:)
    integer vecMax, i

    vecMax = vector(1)
    do i = 2, size(vector)
      if (vecMax < vector(i)) then
        vecMax = vector(i)
      end if
    end do
  end function maxVector

  function dotProd(v, w) result(res)
    intrinsic :: size
    integer, intent(in) :: v(:), w(:)
    integer :: res, i

    res = 0
    do i = 1, size(v)
      res = res + v(i)*w(i)
    end do
  end function dotProd

end module array_functions

program main
  use array_functions
  implicit none
  intrinsic :: dot_product

  ! interface way to write functions
  ! interface
  !   function sumVector(vector) result(vecSum)
  !     integer, intent(in) :: vector(:)
  !     integer :: vecSum
  !   end function sumVector
  ! end interface

  integer, allocatable :: vec(:)
  integer :: status
  integer :: i, ndim, vecSum

  read (*, *) ndim
  allocate (vec(ndim), stat=status)
  write (*, *) "Status ", status

  do i = 1, ndim
    vec(i) = i
  end do

  write (*, *) "Sum of all elements (func)", sumVector(vec)
  call sumVectorSub(vec, vecSum)
  write (*, *) "Sum of all elements (sub)", vecSum
  write (*, *) "Product of all elements ", prodVector(vec)
  write (*, *) "Minimal: ", minVector(vec), "Maximal value: ", maxVector(vec)
  write (*, *) "Dot product ", dotProd(vec, vec), dot_product(vec, vec)

  deallocate(vec, stat=status)
end program main

! interface way to write functions
! function sumVector(vector) result (vecSum)
!   implicit none
!   intrinsic :: size
!   integer, intent(in) :: vector(:)
!   integer vecSum, i

!   vecSum = 0
!   do i = 1, size(vector)
!     vecSum = vecSum + vector(i)
!   end do
! end function sumVector