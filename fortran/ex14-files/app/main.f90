module strings
  implicit none
contains

  function str2int(str) result(int)
    character(len=*), intent(in) :: str
    integer :: int

    read (str, *) int
  end function str2int

end module

program main
  use strings
  use iso_fortran_env, only: input_unit, error_unit
  implicit none

  integer :: io, stat, n = 0
  integer :: num_atoms
  character(:), allocatable :: filename
  character(len=100) :: io_msg
  character(len=200) :: line
  logical :: is_file_existing

  filename = "/home/marvin/Dokumente/projects/fortran/ex14-files/files/h2.xyz"

  ! check if file is existing
  inquire (file=filename, exist=is_file_existing)
  if (.not. is_file_existing) then
    write (error_unit, '("ERROR:", 1x, a)') &
        & "The input file '"//filename//"' does not exist"
    error stop 1
  end if

  ! try opening the file
  open (file=filename, action="read", iostat=stat, iomsg=io_msg, &
        newunit=io, status="old")
  if (stat /= 0) then
    write (error_unit, '("ERROR:", 1x, a)') trim(io_msg)
    error stop 1
  end if

  ! reading the file
  do
    read (io, "(A)", iostat=stat) line
    if (stat /= 0) exit ! stat becomes neq 0 on EOF
    n = n + 1 ! count lines

    if (n == 1) then
      num_atoms = str2int(trim(line))
    else if (n == 2) then
      cycle
    else

    end if
    write (*, *) trim(line)
  end do

  write (*, *) n
  write (*, *) "Number of atoms: ", num_atoms

  ! close file again; clean up
  if (stat /= input_unit) then
    close (io)
  end if

end program main
