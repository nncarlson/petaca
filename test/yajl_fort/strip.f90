!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2018, Neil N. Carlson
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

module strip_cb_type

  use,intrinsic :: iso_fortran_env, only: output_unit
  use yajl_fort
  implicit none
  private

  type, extends(fyajl_callbacks), public :: strip_cb
    integer :: top = 1
    logical :: comma(99) = .false.
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

  subroutine push(this)
    class(strip_cb), intent(inout) :: this
    this%top = this%top + 1
    this%comma(this%top) = .false.
  end subroutine

  subroutine pop(this)
    class(strip_cb), intent(inout) :: this
    this%top = this%top - 1
  end subroutine

  subroutine write_comma(this, next)
    class(strip_cb), intent(inout) :: this
    logical, intent(in) :: next
    if (this%comma(this%top)) write(output_unit,'(",")',advance='no')
    this%comma(this%top) = next
  end subroutine

  integer function null_value(this) result(stat)
    class(strip_cb) :: this
    call write_comma(this, next=.true.)
    write(output_unit,'("null")',advance='no')
    stat = FYAJL_CONTINUE_PARSING
  end function

  integer function logical_value(this, value) result(stat)
    class(strip_cb) :: this
    logical, intent(in) :: value
    call write_comma(this, next=.true.)
    if (value) then
      write(output_unit,'("true")',advance='no')
    else
      write(output_unit,'("false")',advance='no')
    end if
    stat = FYAJL_CONTINUE_PARSING
  end function

  integer function integer_value(this, value) result(stat)
    class(strip_cb) :: this
    integer(fyajl_integer_kind), intent(in) :: value
    call write_comma(this, next=.true.)
    write(output_unit,'(i0)',advance='no') value
    stat = FYAJL_CONTINUE_PARSING
  end function

  integer function double_value(this, value) result(stat)
    class(strip_cb) :: this
    real(fyajl_real_kind), intent(in) :: value
    call write_comma(this, next=.true.)
    write(output_unit,'(es12.5)',advance='no') value
    stat = FYAJL_CONTINUE_PARSING
  end function

  integer function string_value(this, value) result(stat)
    class(strip_cb) :: this
    character(*), intent(in) :: value
    call write_comma(this, next=.true.)
    write(output_unit,'(3a)',advance='no') '"', value, '"'
    stat = FYAJL_CONTINUE_PARSING
  end function

  integer function map_key(this, value) result(stat)
    class(strip_cb) :: this
    character(*), intent(in) :: value
    call write_comma(this, next=.false.)
    write(output_unit,'(3a)',advance='no') '"', value, '":'
    stat = FYAJL_CONTINUE_PARSING
  end function

  integer function start_map(this) result(stat)
    class(strip_cb) :: this
    call write_comma(this, next=.true.)
    write(output_unit,'("{")',advance='no')
    call push(this)
    stat = FYAJL_CONTINUE_PARSING
  end function

  integer function end_map(this) result(stat)
    class(strip_cb) :: this
    write(output_unit,'("}")',advance='no')
    call pop(this)
    stat = FYAJL_CONTINUE_PARSING
  end function

  integer function start_array(this) result(stat)
    class(strip_cb) :: this
    call write_comma(this, next=.true.)
    write(output_unit,'("[")',advance='no')
    call push(this)
    stat = FYAJL_CONTINUE_PARSING
  end function

  integer function end_array(this) result(stat)
    class(strip_cb) :: this
    write(output_unit,'("]")',advance='no')
    call pop(this)
    stat = FYAJL_CONTINUE_PARSING
  end function

end module

program strip_json

  use,intrinsic :: iso_fortran_env
  use yajl_fort
  use strip_cb_type
  implicit none

  integer :: ios, lun, last_pos, curr_pos, buflen
  character(64) :: arg
  character(:), allocatable :: file
  character :: buffer(64) ! intentionally small buffer for testing
  type(strip_cb), target :: callbacks
  type(fyajl_parser), target :: parser
  type(fyajl_status) :: stat

  if (command_argument_count() == 1) then
    call get_command_argument(1, arg)
    file = trim(arg)
  else
    call get_command(arg)
    write(error_unit,'(a)') 'usage: ' // trim(arg) // ' file'
    stop
  end if

  call parser%init(callbacks)
  call parser%set_option(FYAJL_ALLOW_COMMENTS)

  open(newunit=lun,file=file,action='read',access='stream')
  inquire(lun,pos=last_pos)

  do
    read(lun,iostat=ios) buffer
    if (ios /= 0 .and. ios /= iostat_end) then
      write(error_unit,'(a,i0)') 'read error: iostat=', ios
      exit
    end if

    inquire(lun,pos=curr_pos)
    buflen = curr_pos - last_pos
    last_pos = curr_pos
    if (buflen > 0) then
      call parser%parse(buffer(:buflen), stat)
      if (stat /= FYAJL_STATUS_OK) then
        write(error_unit,'(a)') fyajl_get_error(parser, .true., buffer(:buflen))
        exit
      end if
    end if

    if (ios == iostat_end) then
      call parser%complete_parse(stat)
      if (stat /= FYAJL_STATUS_OK) then
        write(error_unit,'(a)') fyajl_get_error(parser, .false., buffer(:buflen))
      end if
      exit
    end if
  end do
  close(lun)

end program
