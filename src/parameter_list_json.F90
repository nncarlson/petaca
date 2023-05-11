!!
!! PARAMETER_LIST_JSON
!!
!!  This module provides a function for creating a PARAMETER_LIST that
!!  is initialized with data read from a JSON-format data stream.
!!
!!  Neil N. Carlson <neil.n.carlson@gmail.com>
!!  10 July 2011; extended June 2014
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2011, 2013, 2014 Neil N. Carlson
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
!!
!! PROGRAMMING INTERFACE
!!
!!  TYPE(PARAMETER_LIST), POINTER :: PLIST
!!  PLIST => PARAMETER_LIST_FROM_JSON_STREAM(UNIT)
!!
!!  UNIT must be connected for unformatted stream access.  The data stream
!!  must be valid JSON format data (http://www.json.org) that conforms to the
!!  semantics of the PARAMETER_LIST data structure.  Specifically,
!!
!!  - A parameter list is described by a JSON object, which begins with
!!    a "{" and ends with a "}".
!!  - A parameter name and associated value are given as a name/value pair
!!    of an object:
!!    - Names are strings (enclosed in double quotes).
!!    - Values may be integer, real, string (enclosed in double quotes), or
!!      logical (true or false -- not the Fortran constants).
!!    - Values may also be JSON arrays, which begin with a "[" and end with
!!      a "]".  The values in an array are constrained to be simple scalar
!!      values all of the same type.  JSON would allow array values of
!!      different types and even general JSON values (object and array).
!!      Update: values may now also be arrays of arrays (rank-2 arrays) as
!!      long as the shape is regular so that it maps onto a Fortran array.
!!      (rank-2 character arrays are currently disabled due to compiler bugs.)
!!    - Values may also be a JSON object which are interpreted as a parameter
!!      sublist.
!!    - Null values (null) are not allowed.
!!    - 0-sized arrays are not allowed.
!!  - The outer-most data element is a JSON object, which is the parameter
!!    list to be created.
!!  - Comments (starting from "//" to the end of the line) are allowed; this
!!    is an extention to the JSON standard provided by the YAJL library.
!!
!!  TYPE(PARAMETER_LIST), POINTER :: PLIST
!!  CALL PARAMETER_LIST_TO_JSON (PLIST, UNIT)
!!
!!  Writes the parameter list.  UNIT must be connected to a file for formatted
!!  write access.  The output is valid JSON output and, aside from issues
!!  related to ascii-binary conversion of floating point numbers, be an exact
!!  representation of the parameter list, in the sense that a parameter list
!!  created from the file should be equivalent to the original.
!!
!! NOTES
!!
!! * JSON integers are long long ints (64-bit typically) but these get stuffed
!!   into default Fortran integers (32-bit typically) so there is the (unchecked)
!!   possibility of overflow.
!! * JSON reals are doubles which typically match Fortran double precision, and
!!   so there is unlikely problems with overflow here, but it is unchecked so
!!   there is a potential problem here.
!! * Arrays of strings are allocated with a length parameter that equals the
!!   longest string read; the others are left justified.
!! * We don't check that the Fortran I/O unit is connected for unformatted
!!   stream access.
!! * As currently implented, only one array can be in the process of being
!!   built at a time (the ARRAY component of PLIST_BUILDER).  This precludes
!!   an array whose elements are parameter lists, which themselves could have
!!   parameters with arrays values.  This could be addressed by introducing
!!   an array stack similar to the parameter list stack.
!! * The procedure for writing a JSON file does so directly and does not use
!!   the YAJL library.  This is far simpler because of the more restrictive
!!   semantics of a parameter list.  Moreover, there is a bug in the library
!!   that results in a floating point value 1.0 being written as "1"; while
!!   numerically equivalent, the latter would rightly be interpreted as an
!!   integer when parsed by JSON.
!!

#include "f90_assert.fpp"

module parameter_list_json

  use,intrinsic :: iso_c_binding
  use,intrinsic :: iso_fortran_env
  use parameter_list_type
  use yajl_fort
  implicit none
  private

  public :: parameter_list_from_json_stream, parameters_from_json_stream
  public :: parameter_list_from_json_string, parameters_from_json_string
  public :: parameter_list_to_json

  interface parameter_list_from_json_stream
    procedure parameter_list_from_json_stream_default, parameter_list_from_json_stream_name
  end interface

  !! Private data structure to hold the hierarchy of parameter lists under construction.
  type parameter_list_stack
    type(stack_item), pointer :: top => null()
  contains
    procedure :: push => stack_push
    procedure :: pop  => stack_pop
    procedure :: peek => stack_peek
    procedure :: is_empty => stack_is_empty
  end type

  type stack_item
    type(parameter_list), pointer :: plist => null()
    type(stack_item), pointer :: next => null()
  end type stack_item

  !! Private data structure to collect the values of an array under construction.
  type value_queue
    integer :: n = 0
    type(queue_item), pointer :: head => null()
    type(queue_item), pointer :: tail => null()
  contains
    procedure :: push => queue_push
    procedure :: pop => queue_pop
    procedure :: peek => queue_peek
    procedure :: is_empty => queue_is_empty
    procedure :: size => queue_size
    procedure, private :: queue_to_array_integer
    procedure, private :: queue_to_array_logical
    procedure, private :: queue_to_array_real
    procedure, private :: queue_to_array_string
    generic :: to_array => queue_to_array_integer, queue_to_array_logical, &
                           queue_to_array_real, queue_to_array_string
  end type value_queue

  type queue_item
    class(*), allocatable :: value
    type(queue_item), pointer :: next => null()
  end type

  type :: array_data
    class(*), allocatable :: mold
    logical :: complete = .false.
    integer :: dim = 0
    integer, allocatable :: shape(:)
    integer, allocatable :: index(:)
    type(value_queue) :: values
    integer :: maxlen = 0 ! max character length for string values
  contains
    procedure :: push_scalar => array_push_scalar
    procedure :: push_array => array_push_array
    procedure :: pop_array => array_pop_array
    procedure :: to_array => array_to_array
  end type array_data

  !! Parsing state labels.
  integer, parameter :: STATE_INIT = 0 ! starting state
  integer, parameter :: STATE_NAME = 1 ! expecting a parameter name
  integer, parameter :: STATE_PVAL = 2 ! expecting the parameter value
  integer, parameter :: STATE_AVAL = 3 ! expecting an array value
  integer, parameter :: STATE_DONE = 4 ! ending state; parameter list complete

  !! The callbacks type required by the YAJL_FORT parser.
  type, extends(fyajl_callbacks) :: plist_builder
    integer :: state
    type(parameter_list_stack) :: pstack
    character(:), allocatable :: name
    type(array_data), allocatable :: array
    character(:), allocatable :: errmsg
  contains
    procedure :: init
    !! Callback functions from the abstract base type
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

  subroutine parameter_list_from_json_stream_default (unit, plist, errmsg)
    integer, intent(in) :: unit
    type(parameter_list), pointer, intent(out) :: plist
    character(:), allocatable :: errmsg
    integer :: stat
    allocate(plist)
    call parameters_from_json_stream (unit, plist, stat, errmsg)
    if (stat /= 0) deallocate(plist)
  end subroutine parameter_list_from_json_stream_default

  subroutine parameter_list_from_json_stream_name (unit, name, plist, errmsg)
    integer, intent(in) :: unit
    character(*), intent(in) :: name
    type(parameter_list), pointer, intent(out) :: plist
    character(:), allocatable :: errmsg
    integer :: stat
    allocate(plist)
    call plist%set_name (name)
    call parameters_from_json_stream (unit, plist, stat, errmsg)
    if (stat /= 0) deallocate(plist)
  end subroutine parameter_list_from_json_stream_name


  subroutine parameters_from_json_stream (unit, plist, stat, errmsg)

    use,intrinsic :: iso_fortran_env, only: error_unit

    integer, intent(in) :: unit
    type(parameter_list), target :: plist
    integer, intent(out) :: stat
    character(:), allocatable, intent(out) :: errmsg

    type(plist_builder), target :: builder
    type(fyajl_parser),  target :: parser
    type(fyajl_status) :: yajl_stat

    integer, parameter :: BUFFER_SIZE = 4096
    character(kind=c_char) :: buffer(BUFFER_SIZE)
    integer :: buflen, last_pos, curr_pos, ios

    !TODO: check unit is open with unformatted stream access

    stat = 0

    !! Initialize the parser
    call builder%init (plist)
    call parser%init (builder)
    call parser%set_option (FYAJL_ALLOW_COMMENTS)
    call parser%set_option (FYAJL_ALLOW_TRAILING_GARBAGE)

    inquire(unit,pos=last_pos)  ! starting position in stream
    do
      read(unit,iostat=ios) buffer
      if (ios /= 0 .and. ios /= iostat_end) then
        write(error_unit,'(a,i0)') 'read error: iostat=', ios
        exit
      end if

      inquire(unit,pos=curr_pos)
      buflen = curr_pos - last_pos
      last_pos = curr_pos
      if (buflen > 0) then
        call parser%parse (buffer(:buflen), yajl_stat)
        if (yajl_stat /= FYAJL_STATUS_OK) then
          errmsg = fyajl_get_error(parser, .true., buffer(:buflen))
          if (yajl_stat == FYAJL_STATUS_CLIENT_CANCELED) errmsg = errmsg // builder%errmsg
          stat = -1
          exit
        end if
      end if

      if (ios == iostat_end) then
        call parser%complete_parse (yajl_stat)
        !call plist%print(output_unit, ' ')
        if (yajl_stat /= FYAJL_STATUS_OK) then
          errmsg = fyajl_get_error(parser, .false., buffer(:buflen))
          if (yajl_stat == FYAJL_STATUS_CLIENT_CANCELED) errmsg = errmsg // builder%errmsg
          stat = -1
        end if
        exit
      end if
    end do

  end subroutine parameters_from_json_stream


  subroutine parameter_list_from_json_string(string, plist, errmsg)
    character(*), intent(in), target :: string
    type(parameter_list), pointer, intent(out) :: plist
    character(:), allocatable, intent(out) :: errmsg
    integer :: stat
    allocate(plist)
    call parameters_from_json_string(string, plist, stat, errmsg)
    if (stat /= 0) deallocate(plist)
  end subroutine parameter_list_from_json_string


  subroutine parameters_from_json_string(string, plist, stat, errmsg)

    use,intrinsic :: iso_c_binding, only: c_loc, c_f_pointer

    character(*), intent(in), target :: string
    type(parameter_list), target :: plist
    integer, intent(out) :: stat
    character(:), allocatable, intent(out) :: errmsg

    type(plist_builder), target :: builder
    type(fyajl_parser),  target :: parser
    type(fyajl_status) :: yajl_stat
    character, pointer :: buffer(:)

    stat = 0

    !! This may be a little dicey...
    call c_f_pointer(c_loc(string(1:1)), buffer, shape=[len(string)])

    call builder%init(plist)
    call parser%init(builder)
    call parser%parse(buffer, yajl_stat)
    if (yajl_stat /= FYAJL_STATUS_OK) then
      errmsg = fyajl_get_error(parser, .true., buffer)
      if (yajl_stat == FYAJL_STATUS_CLIENT_CANCELED) errmsg = errmsg // builder%errmsg
      stat = -1
      return
    end if
    call parser%complete_parse(yajl_stat)
    if (yajl_stat /= FYAJL_STATUS_OK) then
      errmsg = fyajl_get_error(parser, .false., buffer)
      if (yajl_stat == FYAJL_STATUS_CLIENT_CANCELED) errmsg = errmsg // builder%errmsg
      stat = -1
      return
    end if

  end subroutine parameters_from_json_string


  subroutine init (this, plist)
    class(plist_builder), intent(out) :: this
    type(parameter_list), intent(in), target :: plist
    this%state = STATE_INIT
    call this%pstack%push (plist)
  end subroutine init

 !!
 !! THE CALLBACK FUNCTIONS
 !!

  integer function start_map (this) result (status)
    class(plist_builder) :: this
    type(parameter_list), pointer :: plist
    select case (this%state)
    case (STATE_INIT)
      this%state = STATE_NAME           ! expecting a parameter name or map end
      status = FYAJL_CONTINUE_PARSING
    case (STATE_PVAL)
      plist => this%pstack%peek()       ! get current parameter list context
      plist => plist%sublist(this%name) ! create the named sublist
      call this%pstack%push (plist)     ! sublist becomes the new context
      this%state = STATE_NAME           ! expecting a parameter name or map end
      status = FYAJL_CONTINUE_PARSING
    case default
      call unexpected_event (this, this%errmsg)
      status = FYAJL_TERMINATE_PARSING
    end select
  end function start_map

  integer function end_map (this) result (status)
    class(plist_builder) :: this
    type(parameter_list), pointer :: plist
    select case (this%state)
    case (STATE_NAME)
      plist => this%pstack%pop()  ! restore the previous parameter list context
      if (this%pstack%is_empty()) then
        this%state = STATE_DONE ! the top-level parameter list is complete
      else
        this%state = STATE_NAME ! expecting a parameter name or map end
      end if
      status = FYAJL_CONTINUE_PARSING
    case default
      call unexpected_event (this, this%errmsg)
      status = FYAJL_TERMINATE_PARSING
    end select
  end function end_map

  integer function map_key (this, value) result (status)
    class(plist_builder) :: this
    character(*), intent(in) :: value
    type(parameter_list), pointer :: plist
    select case (this%state)
    case (STATE_NAME)
      plist => this%pstack%peek() ! get current parameter list context
      if (plist%is_parameter(value)) then
        this%errmsg = 'parameter with this name already exists'
        status = FYAJL_TERMINATE_PARSING
      else
        this%name = value       ! cache the name for later use
        this%state = STATE_PVAL ! expecting the parameter value
        status = FYAJL_CONTINUE_PARSING
      end if
    case default
      call unexpected_event (this, this%errmsg)
      status = FYAJL_TERMINATE_PARSING
    end select
  end function map_key

  integer function null_value (this) result (status)
    class(plist_builder) :: this
    this%errmsg = 'null values are not allowed'
    status = FYAJL_TERMINATE_PARSING
  end function null_value

  integer function integer_value (this, value) result (status)
    class(plist_builder) :: this
    integer(fyajl_integer_kind), intent(in) :: value
    type(parameter_list), pointer :: plist
    !TODO: check for overflow from mismatched integer types.
    select case (this%state)
    case (STATE_PVAL)
      plist => this%pstack%peek() ! get current parameter list context
      call plist%set (this%name, int(value))  ! create the parameter
      this%state = STATE_NAME ! expecting the next parameter name or map end
      status = FYAJL_CONTINUE_PARSING
    case (STATE_AVAL)
      ASSERT(allocated(this%array))
      call this%array%push_scalar (int(value), status, this%errmsg)
      this%state = STATE_AVAL ! expecting the next array value or array end
    case default
      call unexpected_event (this, this%errmsg)
      status = FYAJL_TERMINATE_PARSING
    end select
  end function integer_value

  integer function double_value (this, value) result (status)
    class(plist_builder) :: this
    real(fyajl_real_kind), intent(in) :: value
    type(parameter_list), pointer :: plist
    !TODO: check for overflow from mismatched real types.
    select case (this%state)
    case (STATE_PVAL)
      plist => this%pstack%peek() ! get current parameter list context
      call plist%set (this%name, real(value,kind(1.0d0))) ! create the parameter
      this%state = STATE_NAME ! expecting the next parameter name or map end
      status = FYAJL_CONTINUE_PARSING
    case (STATE_AVAL)
      ASSERT(allocated(this%array))
      call this%array%push_scalar (real(value,kind(1.0d0)), status, this%errmsg)
      this%state = STATE_AVAL ! expecting the next array value or array end
    case default
      call unexpected_event (this, this%errmsg)
      status = FYAJL_TERMINATE_PARSING
    end select
  end function double_value

  integer function logical_value (this, value) result (status)
    class(plist_builder) :: this
    logical, intent(in) :: value
    type(parameter_list), pointer :: plist
    select case (this%state)
    case (STATE_PVAL)
      plist => this%pstack%peek() ! get current parameter list context
      call plist%set (this%name, value) ! create the parameter
      this%state = STATE_NAME ! expecting the next parameter name or map end
      status = FYAJL_CONTINUE_PARSING
    case (STATE_AVAL)
      ASSERT(allocated(this%array))
      call this%array%push_scalar (value, status, this%errmsg)
      this%state = STATE_AVAL ! expecting the next array value or array end
    case default
      call unexpected_event (this, this%errmsg)
      status = FYAJL_TERMINATE_PARSING
    end select
  end function logical_value

  integer function string_value (this, value) result (status)
    class(plist_builder) :: this
    character(*), intent(in) :: value
    type(parameter_list), pointer :: plist
    select case (this%state)
    case (STATE_PVAL)
      plist => this%pstack%peek() ! get current parameter list context
      call plist%set (this%name, value) ! create the parameter
      this%state = STATE_NAME ! expecting the next parameter name or map end
      status = FYAJL_CONTINUE_PARSING
    case (STATE_AVAL)
      ASSERT(allocated(this%array))
      call this%array%push_scalar (value, status, this%errmsg)
      this%state = STATE_AVAL ! expecting the next array value or array end
    case default
      call unexpected_event (this, this%errmsg)
      status = FYAJL_TERMINATE_PARSING
    end select
  end function string_value

  integer function start_array (this) result (status)
    class(plist_builder) :: this
    select case (this%state)
    case (STATE_PVAL)
      ASSERT(.not.allocated(this%array))
      allocate(this%array)
      call this%array%push_array (status, this%errmsg)
      INSIST(status == FYAJL_CONTINUE_PARSING)
      this%state = STATE_AVAL ! looking for an array value
    case (STATE_AVAL)
      ASSERT(allocated(this%array))
      call this%array%push_array (status, this%errmsg)
      this%state = STATE_AVAL ! looking for an array value
    case default
      call unexpected_event (this, this%errmsg)
      status = FYAJL_TERMINATE_PARSING
    end select
  end function start_array

  integer function end_array (this) result (status)
    class(plist_builder) :: this
    type(parameter_list), pointer :: plist
    class(*), allocatable, target :: flat_array(:)
    class(*), pointer :: array2(:,:)
    select case (this%state)
    case (STATE_AVAL)
      INSIST(allocated(this%array))
      call this%array%pop_array (status, this%errmsg)
      if (status == FYAJL_TERMINATE_PARSING) return
      if (this%array%complete) then
        plist => this%pstack%peek() ! current parameter list
        call this%array%to_array (flat_array)
        select case (size(this%array%shape))
        case (1)
          call plist%set (this%name, flat_array) ! create the parameter
        case (2)
          array2(1:this%array%shape(1),1:this%array%shape(2)) => flat_array
          call plist%set (this%name, array2) ! create the parameter
        case default
          this%errmsg = 'arrays of rank greater than 2 are not supported'
          status = FYAJL_TERMINATE_PARSING
          return
        end select
        deallocate(this%array)
        this%state = STATE_NAME ! expecting the next parameter name or map end
        status = FYAJL_CONTINUE_PARSING
      else
        this%state = STATE_AVAL ! looking for an array value
        status = FYAJL_CONTINUE_PARSING
      end if
    case default
      call unexpected_event (this, this%errmsg)
      status = FYAJL_TERMINATE_PARSING
    end select
  end function end_array

  subroutine unexpected_event (this, errmsg)
    class(plist_builder), intent(in) :: this
    character(:), allocatable :: errmsg
    select case (this%state)
    case (STATE_INIT)
      errmsg = '"{" expected'
    case (STATE_NAME)
      errmsg = 'parameter name or "}" expected'
    case (STATE_PVAL)
      errmsg = 'scalar value, "[", or "{" expected'
    case (STATE_AVAL)
      errmsg = 'scalar value, "[", or "]" expected'
    case (STATE_DONE)
      errmsg = 'no further input expected'
    case default
      INSIST(.false.)
    end select
  end subroutine unexpected_event

 !!
 !! PARAMETER_LIST_STACK TYPE-BOUND PROCEDURES
 !!

  !! Push the parameter list onto the top of the stack.
  subroutine stack_push (this, plist)
    class(parameter_list_stack), intent(inout) :: this
    type(parameter_list), intent(in), target :: plist
    type(stack_item), pointer :: top
    top => this%top
    allocate(this%top)
    this%top%plist => plist
    this%top%next => top
  end subroutine stack_push

  !! Pop the parameter list off the top of the stack.
  function stack_pop (this) result (plist)
    class(parameter_list_stack), intent(inout) :: this
    type(parameter_list), pointer :: plist
    type(stack_item), pointer :: top
    if (associated(this%top)) then
      plist => this%top%plist
      top => this%top
      this%top => top%next
      deallocate(top)
    else
      plist => null()
    end if
  end function stack_pop

  !! Return a pointer to the parameter list on the top of the stack.
  function stack_peek (this) result (plist)
    class(parameter_list_stack), intent(in) :: this
    type(parameter_list), pointer :: plist
    if (associated(this%top)) then
      plist => this%top%plist
    else
      plist => null()
    end if
  end function stack_peek

  logical function stack_is_empty (this)
    class(parameter_list_stack), intent(in) :: this
    stack_is_empty = .not.associated(this%top)
  end function stack_is_empty

 !!
 !! ARRAY_DATA TYPE-BOUND PROCEDURES
 !!

  subroutine array_push_scalar (this, value, status, errmsg)

    class(array_data), intent(inout) :: this
    class(*), intent(in) :: value
    integer, intent(out) :: status
    character(:), allocatable :: errmsg

    if (allocated(this%shape)) then
      if (this%dim == 1) then
        if (this%index(1) < this%shape(1)) then
          this%index(1) = this%index(1) + 1
        else
          status = FYAJL_TERMINATE_PARSING
          errmsg = 'irregular array shape'
          return
        end if
      else
        status = FYAJL_TERMINATE_PARSING
        errmsg = 'irregular array shape'
        return
      end if
    else  ! discovered the array rank
      allocate(this%shape(this%dim), this%index(this%dim))
      this%shape = huge(1)  ! extents to be discovered
      this%index = 1        ! array index of this scalar
      this%dim = 1          ! current array dimension
    end if

    if (allocated(this%mold)) then
      if (.not.my_same_type_as(value, this%mold)) then
        status = FYAJL_TERMINATE_PARSING
        select type (uptr => this%mold)
        type is (integer)
          errmsg = 'expecting an integer value'
#ifdef GFORTRAN_10_1
        type is (double precision)
#else
        type is (real(kind(1.0d0)))
#endif
          errmsg = 'expecting a real value'
        type is (character(*))
          errmsg = 'expecting a string value'
        type is (logical)
          errmsg = 'expecting a logical value'
        class default
          INSIST(.false.)
        end select
        return
      end if
    else  ! discovered the type
      allocate(this%mold, mold=value)
    end if
    call this%values%push (value)

    select type (value)
    type is (character(*))
      this%maxlen = max(this%maxlen, len(value))
    end select

    status = FYAJL_CONTINUE_PARSING

  end subroutine array_push_scalar


  subroutine array_push_array (this, status, errmsg)

    class(array_data), intent(inout) :: this
    integer, intent(out) :: status
    character(:), allocatable :: errmsg

    status = FYAJL_CONTINUE_PARSING
    if (allocated(this%shape)) then
      if (this%dim > 1) then
        if (this%index(this%dim) < this%shape(this%dim)) then
          this%index(this%dim) = this%index(this%dim) + 1
          this%dim = this%dim - 1
          this%index(this%dim) = 0
        else
          status = FYAJL_TERMINATE_PARSING
          errmsg = 'irregular array shape'
        end if
      else
        status = FYAJL_TERMINATE_PARSING
        errmsg = 'irregular array shape'
      end if
    else  ! still discovering the rank
      this%dim = this%dim + 1 ! update the rank
    end if

  end subroutine array_push_array


  subroutine array_pop_array (this, status, errmsg)

    class(array_data), intent(inout) :: this
    integer, intent(out) :: status
    character(:), allocatable :: errmsg

    if (allocated(this%index)) then
      if (this%index(this%dim) == 0) then
        status = FYAJL_TERMINATE_PARSING
        errmsg = '0-sized arrays are not allowed'
        return
      end if
    else
      status = FYAJL_TERMINATE_PARSING
      errmsg = '0-sized arrays are not allowed'
      return
    end if

    if (this%shape(this%dim) == huge(1)) then ! discovered the extent in this dimensiono
      this%shape(this%dim) = this%index(this%dim)
    else if (this%index(this%dim) /= this%shape(this%dim)) then
      status = FYAJL_TERMINATE_PARSING
      errmsg = 'irregular array shape'
      return
    end if

    this%dim = this%dim + 1
    if (this%dim > size(this%shape)) this%complete = .true.

    select type (uptr => this%mold)
    type is (character(*))
      deallocate(this%mold)
      allocate(character(this%maxlen) :: this%mold)
    end select

    status = FYAJL_CONTINUE_PARSING

  end subroutine array_pop_array

  subroutine array_to_array (this, array)
    class(array_data), intent(inout) :: this
    class(*), allocatable, intent(out) :: array(:)
    INSIST(this%complete)
    INSIST(product(this%shape) == this%values%size())
    allocate(array(this%values%size()), mold=this%mold)
    select type (array)
    type is (integer)
      call this%values%to_array (array)
    type is (logical)
      call this%values%to_array (array)
#ifdef GFORTRAN_10_1
    type is (double precision)
#else
    type is (real(kind(1.0d0)))
#endif
      call this%values%to_array (array)
    type is (character(*))
      call this%values%to_array (array)
    class default
      INSIST(.false.)
    end select
  end subroutine array_to_array

 !!
 !! VALUE_QUEUE TYPE-BOUND PROCEDURES
 !!

  !! Push the value onto the tail of the list.
  subroutine queue_push (this, value)
    class(value_queue), intent(inout) :: this
    class(*), intent(in) :: value
    if (associated(this%head)) then
      allocate(this%tail%next)
      this%tail => this%tail%next
    else  ! empty queue -- first item
      allocate(this%head)
      this%tail => this%head
    end if
    allocate(this%tail%value, source=value)
    this%n = this%n + 1
  end subroutine queue_push

  !! Return a pointer to the value at the head of the list.
  function queue_peek (this) result (value)
    class(value_queue), intent(in) :: this
    class(*), pointer :: value
    if (associated(this%head)) then
      value => this%head%value
    else
      value => null()
    end if
  end function queue_peek

  !! Pop the value off the head of the list; use peek first to get it.
  subroutine queue_pop (this)
    class(value_queue), intent(inout) :: this
    type(queue_item), pointer :: head
    if (associated(this%head)) then
      head => this%head
      if (associated(this%head, this%tail)) then
        this%head => null()
        this%tail => null()
      else
        this%head => this%head%next
      end if
      deallocate(head)
      this%n = this%n - 1
    end if
  end subroutine queue_pop

  !! Move the values into an array, leaving an empty queue.
  !! Specific procedures for the different possible types.

  subroutine queue_to_array_integer (this, array)
    class(value_queue) :: this
    integer, intent(out) :: array(:)
    integer :: n
    ASSERT(size(array) == this%size())
    do n = 1, size(array)
      select type (uptr => this%peek())
      type is (integer)
        array(n) = uptr
      class default
        INSIST(.false.)
      end select
      call this%pop()
    end do
  end subroutine queue_to_array_integer

  subroutine queue_to_array_logical (this, array)
    class(value_queue) :: this
    logical, intent(out) :: array(:)
    integer :: n
    ASSERT(size(array) == this%size())
    do n = 1, size(array)
      select type (uptr => this%peek())
      type is (logical)
        array(n) = uptr
      class default
        INSIST(.false.)
      end select
      call this%pop()
    end do
  end subroutine queue_to_array_logical

  subroutine queue_to_array_real (this, array)
    class(value_queue) :: this
    real(kind(1.0d0)), intent(out) :: array(:)
    integer :: n
    ASSERT(size(array) == this%size())
    do n = 1, size(array)
      select type (uptr => this%peek())
      type is (real(fyajl_real_kind))
        array(n) = uptr
      class default
        INSIST(.false.)
      end select
      call this%pop()
    end do
  end subroutine queue_to_array_real

  subroutine queue_to_array_string (this, array)
    class(value_queue) :: this
    character(*), intent(out) :: array(:)
#if defined(INTEL_BUG20180115)
    class(*), pointer :: uptr
#endif
    integer :: n
    ASSERT(size(array) == this%size())
    do n = 1, size(array)
#if defined(INTEL_BUG20180115)
      uptr => this%peek()
      select type (uptr)
#else
      select type (uptr => this%peek())
#endif
      type is (character(*))
        array(n) = uptr
      class default
        INSIST(.false.)
      end select
      call this%pop()
    end do
  end subroutine queue_to_array_string

  logical function queue_is_empty (this)
    class(value_queue), intent(in) :: this
    queue_is_empty = .not.associated(this%head)
  end function queue_is_empty

  integer function queue_size (this)
    class(value_queue), intent(in) :: this
    queue_size = this%n
  end function queue_size

  subroutine parameter_list_to_json (plist, unit, real_fmt)
    type(parameter_list), intent(in) :: plist
    integer, intent(in) :: unit
    character(*), intent(in), optional :: real_fmt
    write(unit,'(a)') '{'
    call parameter_list_to_json_aux (plist, '  ', unit, real_fmt)
    write(unit,'(/,a)') '}'
  end subroutine parameter_list_to_json

  recursive subroutine parameter_list_to_json_aux (plist, indent, unit, real_fmt)

    use map_any_type
    use parameter_entry_class

    type(parameter_list), intent(in) :: plist
    character(*), intent(in) :: indent
    integer, intent(in) :: unit
    character(*), intent(in), optional :: real_fmt

    type(parameter_list_iterator) :: piter
    logical :: first_param

    piter = parameter_list_iterator(plist)
    first_param = .true.
    do while (.not.piter%at_end())
      if (first_param) then
        first_param = .false.
      else
        write(unit,'(a)') ','
      end if
      write(unit,'(a)',advance='no') indent // '"' // piter%name() // '"'
      select type (pentry => piter%entry())
      type is (parameter_list)
        write(unit,'(a)') ': {'
        call parameter_list_to_json_aux (pentry, indent//'  ', unit, real_fmt)
        write(unit,'(/,a)',advance='no') indent // '}'
      type is (any_scalar)
        write(unit,'(a)',advance='no') ': '
        call pentry%write (unit, real_fmt)
      type is (any_vector)
        write(unit,'(a)',advance='no') ': '
        call pentry%write (unit, real_fmt)
      type is (any_matrix)
        write(unit,'(a)',advance='no') ': '
        call pentry%write (unit, real_fmt)
      end select
      call piter%next
    end do

  end subroutine parameter_list_to_json_aux

  !! The behavior of the intrinsic SAME_TYPE_AS function is processor-dependent
  !! when applied to CLASS(*) variables with intrinsic dynamic type, making it
  !! useless for the present need.  Hence this custom version.  The variables
  !! will have one of the types returned for values by the YAJL_FORT parser,
  !! namely an integer and real of one specific kind, default logical and
  !! character.

  logical function my_same_type_as(a, b)
    class(*), intent(in) :: a, b
    my_same_type_as = (type_num(a) == type_num(b))
  contains
    integer function type_num(a)
      class(*), intent(in) :: a
      select type (a)
      type is (integer)
        type_num = 1
#ifdef GFORTRAN_10_1
      type is (double precision)
#else
      type is (real(kind(1.0d0)))
#endif
        type_num = 2
      type is (logical)
        type_num = 3
      type is (character(*))
        type_num = 4
      class default
        INSIST(.false.)
      end select
    end function type_num
  end function my_same_type_as

end module parameter_list_json
