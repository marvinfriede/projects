program main
  implicit none
  intrinsic:: sum, product, maxval, minval
  integer, dimension(0:3) :: vec
  integer, dimension(0:2, 0:2) :: arr
  integer :: vecSum, vecMax, vecProd, vecMin, i

  vec(0) = 2
  vec(1) = 3
  vec(2) = 4
  vec(3) = 6

  vecSum = 0
  do i = 0, size(vec) - 1
    vecSum = vecSum + vec(i)
  end do

  vecProd = 1 ! do not start with 0
  do i = 0, size(vec) - 1
    vecProd = vecProd * vec(i)
  end do

  vecMax = vec(0)
  do i = 1, size(vec) - 1
    if (vecMax < vec(i)) then
      vecMax = vec(i)
    end if
  end do

  vecMin = vec(0)
  do i = 1, size(vec) - 1
    if (vecMin > vec(i)) then
      vecMin = vec(i)
    end if
  end do

  write (*, *) "Sum of all elements (intrinsic)", sum(vec)
  write (*, *) "Sum of all elements (own)", vecSum
  write (*, *) "Product of all elements (intrinsic)", product(vec)
  write (*, *) "Product of all elements (own)", vecProd
  write (*, *) "Minimal: ", minval(vec), "Maximal value: ", maxval(vec)
  write (*, *) "Minimal: ", vecMin, "Maximal value: ", vecMax
  write (*, *) ""

  ! 1 2 3
  ! 4 5 6
  ! 7 8 9
  arr(0, 0) = 1
  arr(0, 1) = 2
  arr(0, 2) = 3
  arr(1, 0) = 4
  arr(1, 1) = 5
  arr(1, 2) = 6
  arr(2, 0) = 7
  arr(2, 1) = 8
  arr(2, 2) = 9
  write (*, *) 'Full matrix ', arr ! goes down cols
  write (*, *) "Sum of first col", sum(arr(0:2, 0))
end program main
