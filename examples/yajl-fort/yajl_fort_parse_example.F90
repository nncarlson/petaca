!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2011, 2013, Neil N. Carlson
!!
!! Permission is hereby granted, free of charge, to any person obtaining a
!! copy of this software and associated documentation files (the "Software"),
!! to deal in the Software without restriction, including without limitation
!! the rights to use, copy, modify, merge, publish, distribute, sublicense,
!! and/or sell copies of the Software, and to permit persons to whom the
!! Software is furnished to do so, subject to the following conditions:
!!
!! The above copyright notice and this permission notice shall be included
!! in all copies or substantial portions of the Software.
!!
!! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
!! THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
!! FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
!! DEALINGS IN THE SOFTWARE.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module echo_callbacks_type

  use yajl_fort
  implicit none
  private
  
  type, extends(fyajl_callbacks), public :: echo_callbacks
    ! no context data is needed
  contains
    procedure :: start_map
    procedure :: end_map
    procedure :: map_key
    procedure :: null_value
    procedure :: logical_value
    procedure :: integer_value
    procedure :: double_value
    procedure :: string_value
    procedure :: start_array
    procedure :: end_array
  end type
  
contains

  integer function null_value (this) result (stat)
    class(echo_callbacks) :: this
    write(*,*) '(null)'
    stat = FYAJL_CONTINUE_PARSING
  end function
  
  integer function logical_value (this, value) result (stat)
    class(echo_callbacks) :: this
    logical, intent(in) :: value
    write(*,*) value
    stat = FYAJL_CONTINUE_PARSING
  end function
  
  integer function integer_value (this, value) result(stat)
    class(echo_callbacks) :: this
    integer(fyajl_integer_kind), intent(in) :: value
    write(*,*) value
    stat = FYAJL_CONTINUE_PARSING
  end function
  
  integer function double_value (this, value) result(stat)
    class(echo_callbacks) :: this
    real(fyajl_real_kind), intent(in) :: value
    write(*,*) value
    stat = FYAJL_CONTINUE_PARSING
  end function
  
  integer function string_value (this, value) result (stat)
    class(echo_callbacks) :: this
    character(*), intent(in) :: value
    write(*,*) '"', value, '"'
    stat = FYAJL_CONTINUE_PARSING
  end function
  
  integer function map_key (this, value) result (stat)
    class(echo_callbacks) :: this
    character(*), intent(in) :: value
    write(*,*) '"', value, '":'
    stat = FYAJL_CONTINUE_PARSING
  end function
  
  integer function start_map (this) result(stat)
    class(echo_callbacks) :: this
    write(*,*) '{'
    stat = FYAJL_CONTINUE_PARSING
  end function
  
  integer function end_map (this) result(stat)
    class(echo_callbacks) :: this
    write(*,*) '}'
    stat = FYAJL_CONTINUE_PARSING
  end function

  integer function start_array (this) result(stat)
    class(echo_callbacks) :: this
    write(*,*) '['
    stat = FYAJL_CONTINUE_PARSING
  end function
  
  integer function end_array (this) result(stat)
    class(echo_callbacks) :: this
    write(*,*) ']'
    stat = FYAJL_CONTINUE_PARSING
  end function
  
end module echo_callbacks_type

program yajl_fort_parse_example

  use,intrinsic :: iso_fortran_env
  use,intrinsic :: iso_c_binding
  use yajl_fort
  use echo_callbacks_type
  implicit none
  
  character(len=64) :: prog, file
  
  if (command_argument_count() == 1) then
    call get_command_argument (1, file)
  else
    call get_command (prog)
    write(error_unit,'(a)') 'usage: ' // trim(prog) // ' file'
    stop
  end if
  
  call echo_json_events (file)
  
contains

  subroutine echo_json_events (file)
  
    character(*), intent(in) :: file
    
    type(echo_callbacks), target :: callbacks
    type(fyajl_parser), target :: parser
    type(fyajl_status) :: stat
    integer :: ios, last_pos, curr_pos, buflen
    character :: buffer(128) ! intentionally small buffer
    
    call parser%init (callbacks)
    call parser%set_option (FYAJL_ALLOW_COMMENTS)
    
    open(10,file=trim(file),action='read',access='stream',form='unformatted')
    inquire(10,pos=last_pos)
    do
      read(10,iostat=ios) buffer
      if (ios /= 0 .and. ios /= iostat_end) then
        write(error_unit,'(a,i0)') 'read error: iostat=', ios
        exit
      end if

      inquire(10,pos=curr_pos)
      buflen = curr_pos - last_pos
      last_pos = curr_pos
      if (buflen > 0) then
        call parser%parse (buffer(:buflen), stat)
        if (stat /= FYAJL_STATUS_OK) then
          write(error_unit,'(a)') fyajl_get_error(parser, .true., buffer(:buflen))
          exit
        end if
      end if

      if (ios == iostat_end) then
        call parser%complete_parse (stat)
        if (stat /= FYAJL_STATUS_OK) then
          write(error_unit,'(a)') fyajl_get_error(parser, .false., buffer(:buflen))
        end if
        exit
      end if
    end do
    close(10)
    
  end subroutine
  
end program
