module array_funcs
  implicit none

  private
  public :: is_mat_identity, is_mat_symmetric, get_dim_mat, rms, rms_vec

  integer, parameter :: wp = selected_real_kind(15)

contains
  logical function is_mat_symmetric(mat) result(is_symmetric)
    implicit none
    intrinsic :: abs

    real(wp), intent(in), dimension(:, :):: mat
    integer :: num_cols, num_rows
    integer, dimension(2) :: shape_mat
    integer :: i, j

    !> thresholds for two reals being equal
    real(wp), parameter :: TOL_EQ = 1.0e-8_wp

    !> get shape of matrix
    shape_mat = shape(mat)
    num_rows = shape_mat(1)
    num_cols = shape_mat(2)

    if (num_cols /= num_rows) then
      is_symmetric = .false.
      return
    end if

    is_symmetric = .true.
    outer: do i = 1, num_rows
      inner: do j = 1, i
        if (i /= j) then
          if (abs(mat(i, j) - mat(j, i)) > TOL_EQ) then
            is_symmetric = .false.
            exit outer
          end if
        end if
      end do inner
    end do outer
  end function is_mat_symmetric

  logical function is_mat_identity(mat) result(is_identity)
    implicit none
    intrinsic :: abs

    real(wp), intent(in), dimension(:, :), allocatable :: mat
    integer :: num_cols, num_rows
    integer, dimension(2) :: shape_mat
    integer :: i, j

    !> thresholds for two reals being equal
    real(wp), parameter :: TOL_EQ = 1.0e-8_wp

    !> get shape of matrix
    shape_mat = shape(mat)
    num_rows = shape_mat(1)
    num_cols = shape_mat(2)

    if (num_cols /= num_rows) then
      is_identity = .false.
    else
      is_identity = .true.

      outer: do i = 1, num_rows
        inner: do j = 1, num_rows
          if (i == j) then
            if (abs(mat(i, i) - 1.0_wp) > TOL_EQ) then
              is_identity = .false.
              exit outer
            end if
          else
            if (abs(mat(i, j)) > TOL_EQ) then
              is_identity = .false.
              exit outer
            end if
          end if
        end do inner
      end do outer

    end if
  end function is_mat_identity

  integer function get_dim_mat(mat) result(dim)
    implicit none
    intrinsic :: abs

    real(wp), intent(in), dimension(:, :), allocatable :: mat
    integer :: num_cols, num_rows
    integer, dimension(2) :: shape_mat

    !> get shape of matrix
    shape_mat = shape(mat)
    num_rows = shape_mat(1)
    num_cols = shape_mat(2)

    !> only for symmetric matrices
    if (num_cols /= num_rows) then
      write (*, *) "Matrix not symmetric. Aborting..."
      error stop 1
    end if

    dim = num_cols
  end function get_dim_mat

  real(wp) function rms(mat) result(val)
    implicit none
    intrinsic :: sqrt, sum

    real(wp), intent(in), dimension(:, :), allocatable :: mat

    val = sqrt(sum(mat**2)/size(mat))
  end function rms

  real(wp) function rms_vec(vec) result(val)
    implicit none
    intrinsic :: sqrt, sum

    real(wp), intent(in), dimension(:), allocatable :: vec

    val = sqrt(sum(vec**2)/size(vec))
  end function rms_vec

end module array_funcs
