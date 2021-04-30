program main
  implicit none
  intrinsic :: selected_real_kind, size
  integer, parameter :: wp = selected_real_kind(15)
  integer :: i, dim1, dim2
  real(wp), allocatable, dimension(:, :) :: arr

  read (*, *) dim1, dim2

  allocate (arr(dim1, dim2))

  ! 1.000000000000000 2.000000000000000 3.000000000000000
  ! 1.000000000000000 2.000000000000000 3.000000000000000
  ! 1.000000000000000 2.000000000000000 3.000000000000000
  do i = 1, size(arr, 2) ! size(array, dimension <- optional)
    arr(:, i) = i
  end do

  write (*, *) "Array: ", arr

  deallocate (arr)
end program main
