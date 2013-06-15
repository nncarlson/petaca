program parameter_list_input_example

  use,intrinsic :: iso_fortran_env
  use parameter_list_type
  use parameter_list_json
  implicit none
  
  character(len=64) :: prog, file
  character(:), allocatable :: errmsg
  type(parameter_list), pointer :: plist
  
  if (command_argument_count() == 1) then
    call get_command_argument (1, file)
  else
    call get_command (prog)
    write(error_unit,'(a)') 'usage: ' // trim(prog) // ' file'
    stop
  end if
  
  open(10,file=file,action='read',access='stream',form='unformatted')
  call parameter_list_from_json_stream(10, plist, errmsg)
  if (associated(plist)) then
    call plist%print (output_unit, ' ')
    deallocate(plist)
  else
    write(error_unit,'(a)') errmsg
  end if
  
end program

