!!
!! PARAMETER_LIST_JSON
!!
!!  This module provides a function for creating a PARAMETER_LIST that
!!  is initialized with data read from a JSON-format data stream.
!!
!!  Neil N. Carlson <neil.n.carlson@gmail.com>
!!  10 July 2011
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
!!   built at a time (the VALUES component of PLIST_BUILDER).  This precludes
!!   multidimensional arrays (arrays of arrays).  Multidimensional parameter
!!   values are not supported by PARAMETER_LIST currently anyway.
!! * The procedure for writing a JSON file does so directly and does not use
!!   the YAJL library.  This is far simpler because of the more restrictive
!!   semantics of a parameter list.  Moreover, there is a bug in the library
!!   that results in a floating point value 1.0 being written as "1"; while
!!   numerically equivalent, the latter would rightly be interpreted as an
!!   integer when parsed by JSON.
!!

#ifdef __INTEL_COMPILER
#define INTEL_WORKAROUND
#define INTEL_WORKAROUND3
#endif

#include "f90_assert.fpp"

module parameter_list_json

  use,intrinsic :: iso_c_binding
  use,intrinsic :: iso_fortran_env
  use parameter_list_type
  use yajl_fort
  implicit none
  private

  public :: parameter_list_from_json_stream
  public :: parameter_list_to_json

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
  end type value_queue

  type queue_item
    class(*), allocatable :: value
    type(queue_item), pointer :: next => null()
  end type

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
    type(value_queue) :: values
    integer :: maxlen ! max string length while building a string array
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

  subroutine parameter_list_from_json_stream (unit, plist, errmsg)

    use,intrinsic :: iso_fortran_env, only: error_unit

    integer, intent(in) :: unit
    type(parameter_list), pointer, intent(out) :: plist
    character(:), allocatable :: errmsg

    type(plist_builder), target :: builder
    type(fyajl_parser),  target :: parser
    type(fyajl_status) :: stat

    integer, parameter :: BUFFER_SIZE = 4096
    character(kind=c_char) :: buffer(BUFFER_SIZE)
    integer :: buflen, last_pos, curr_pos, ios
#ifdef INTEL_WORKAROUND3
    integer :: offset, bufsize
#endif

    !TODO: check unit is open with unformatted stream access

    !! Initialize the parser
    allocate(plist)
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
      
#ifdef INTEL_WORKAROUND3
      !! The intel compiler doesn't advance the file position at all when the
      !! EOF is encountered.  This means we don't know how many characters
      !! were read before the EOF.  So we keep re-reading, halving the buffer
      !! size each time until the buffer size is 0.  For successful reads we
      !! keep the characters read (we know how many).  This way we fill up the
      !! buffer with all the remaining characters and know how many there are.
      if (ios == iostat_end) then ! file position not advanced
        offset = 0
        bufsize = size(buffer)/2
        do
          read(unit,iostat=ios) buffer(offset+1:offset+bufsize)
          if (ios /= 0 .and. ios /= iostat_end) then
            write(error_unit,'(a,i0)') 'read error: iostat=', ios
            exit
          end if
          if (ios == iostat_end) then
            if (bufsize == 1) exit
          else
            offset = offset + bufsize
          end if
          bufsize = max(1, bufsize/2)
        end do
        if (ios /= iostat_end) exit
      end if

#endif
      inquire(unit,pos=curr_pos)
      buflen = curr_pos - last_pos
      last_pos = curr_pos
      if (buflen > 0) then
        call parser%parse (buffer(:buflen), stat)
        if (stat /= FYAJL_STATUS_OK) then
          errmsg = fyajl_get_error(parser, .true., buffer(:buflen))
          if (stat == FYAJL_STATUS_CLIENT_CANCELED) errmsg = errmsg // builder%errmsg
          deallocate(plist)
          INSIST(.not.associated(plist))
          exit
        end if
      end if

      if (ios == iostat_end) then
        call parser%complete_parse (stat)
        !call plist%print(output_unit, ' ')
        if (stat /= FYAJL_STATUS_OK) then
          errmsg = fyajl_get_error(parser, .false., buffer(:buflen))
          if (stat == FYAJL_STATUS_CLIENT_CANCELED) errmsg = errmsg // builder%errmsg
          deallocate(plist)
        end if
        exit
      end if
    end do

  end subroutine parameter_list_from_json_stream

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
#ifdef INTEL_WORKAROUND
    class(*), pointer :: uptr
    integer :: pval
#endif
    !TODO: check for overflow from mismatched integer types.
    select case (this%state)
    case (STATE_PVAL)
      plist => this%pstack%peek() ! get current parameter list context
#ifdef INTEL_WORKAROUND
      pval = int(value)
      call plist%set (this%name, pval)  ! create the parameter
#else
      call plist%set (this%name, int(value))  ! create the parameter
#endif
      this%state = STATE_NAME ! expecting the next parameter name or map end
      status = FYAJL_CONTINUE_PARSING
    case (STATE_AVAL)
      if (.not.this%values%is_empty()) then
        !! Verify that the other values are integers too.
#ifdef INTEL_WORKAROUND
        uptr => this%values%peek()
        select type (uptr)
#else
        select type (uptr => this%values%peek())
#endif
        type is (integer)
        class default
          call unexpected_array_type (this, this%errmsg)
          status = FYAJL_TERMINATE_PARSING
          return
        end select
      end if
#ifdef INTEL_WORKAROUND
      pval = int(value)
      call this%values%push (pval) ! save the value for later use
#else
      call this%values%push (int(value)) ! save the value for later use
#endif
      this%state = STATE_AVAL ! expecting the next array value or array end
      status = FYAJL_CONTINUE_PARSING
    case default
      call unexpected_event (this, this%errmsg)
      status = FYAJL_TERMINATE_PARSING
    end select
  end function integer_value

  integer function double_value (this, value) result (status)
    class(plist_builder) :: this
    real(fyajl_real_kind), intent(in) :: value
    type(parameter_list), pointer :: plist
#ifdef INTEL_WORKAROUND
    class(*), pointer :: uptr
#endif
    !TODO: check for overflow from mismatched real types.
    select case (this%state)
    case (STATE_PVAL)
      plist => this%pstack%peek() ! get current parameter list context
      call plist%set (this%name, real(value,kind(1.0d0))) ! create the parameter
      this%state = STATE_NAME ! expecting the next parameter name or map end
      status = FYAJL_CONTINUE_PARSING
    case (STATE_AVAL)
      if (.not.this%values%is_empty()) then
        !! Verify that the other values are doubles too.
#ifdef INTEL_WORKAROUND
        uptr => this%values%peek()
        select type (uptr)
#else
        select type (uptr => this%values%peek())
#endif
        type is (real(kind(1.0d0)))
        class default
          call unexpected_array_type (this, this%errmsg)
          status = FYAJL_TERMINATE_PARSING
          return
        end select
      end if
      call this%values%push (real(value,kind(1.0d0))) ! save the value for later use
      this%state = STATE_AVAL ! expecting the next array value or array end
      status = FYAJL_CONTINUE_PARSING
    case default
      call unexpected_event (this, this%errmsg)
      status = FYAJL_TERMINATE_PARSING
    end select
  end function double_value

  integer function logical_value (this, value) result (status)
    class(plist_builder) :: this
    logical, intent(in) :: value
    type(parameter_list), pointer :: plist
#ifdef INTEL_WORKAROUND
    class(*), pointer :: uptr
#endif
    select case (this%state)
    case (STATE_PVAL)
      plist => this%pstack%peek() ! get current parameter list context
      call plist%set (this%name, value) ! create the parameter
      this%state = STATE_NAME ! expecting the next parameter name or map end
      status = FYAJL_CONTINUE_PARSING
    case (STATE_AVAL)
      if (.not.this%values%is_empty()) then
        !! Verify that the other values are logical too.
#ifdef INTEL_WORKAROUND
        uptr => this%values%peek()
        select type (uptr)
#else
        select type (uptr => this%values%peek())
#endif
        type is (logical)
        class default
          call unexpected_array_type (this, this%errmsg)
          status = FYAJL_TERMINATE_PARSING
          return
        end select
      end if
      call this%values%push (value) ! save the value for later use
      this%state = STATE_AVAL ! expecting the next array value or array end
      status = FYAJL_CONTINUE_PARSING
    case default
      call unexpected_event (this, this%errmsg)
      status = FYAJL_TERMINATE_PARSING
    end select
  end function logical_value

  integer function string_value (this, value) result (status)
    class(plist_builder) :: this
    character(*), intent(in) :: value
    type(parameter_list), pointer :: plist
#ifdef INTEL_WORKAROUND
    class(*), pointer :: uptr
#endif
    select case (this%state)
    case (STATE_PVAL)
      plist => this%pstack%peek() ! get current parameter list context
      call plist%set (this%name, value) ! create the parameter
      this%state = STATE_NAME ! expecting the next parameter name or map end
      status = FYAJL_CONTINUE_PARSING
    case (STATE_AVAL)
      if (.not.this%values%is_empty()) then
        !! Verify that the other values are strings too.
#ifdef INTEL_WORKAROUND
        uptr => this%values%peek()
        select type (uptr)
#else
        select type (uptr => this%values%peek())
#endif
        type is (character(*))  ! okay, expected type
        class default ! anything else
          call unexpected_array_type (this, this%errmsg)
          status = FYAJL_TERMINATE_PARSING
          return
        end select
        this%maxlen = max(len(value), this%maxlen)
      else
        this%maxlen = 0
      end if
      call this%values%push (value) ! save the value for later use
      this%state = STATE_AVAL ! expecting the next array value or array end
      status = FYAJL_CONTINUE_PARSING
    case default
      call unexpected_event (this, this%errmsg)
      status = FYAJL_TERMINATE_PARSING
    end select
  end function string_value

  integer function start_array (this) result (status)
    class(plist_builder) :: this
    select case (this%state)
    case (STATE_PVAL)
      INSIST(this%values%is_empty())
      this%state = STATE_AVAL ! looking for an array value
      status = FYAJL_CONTINUE_PARSING
    case default
      call unexpected_event (this, this%errmsg)
      status = FYAJL_TERMINATE_PARSING
    end select
  end function start_array

  integer function end_array (this) result (status)
    class(plist_builder) :: this
#ifdef INTEL_WORKAROUND
    class(*), pointer :: uptr
#endif
    select case (this%state)
    case (STATE_AVAL)
      if (this%values%size() > 0) then
#ifdef INTEL_WORKAROUND
        uptr => this%values%peek()
        select type (uptr)
#else
        select type (uptr => this%values%peek())
#endif
        type is (integer)
          call integer_array_value (this)
        type is (logical)
          call logical_array_value (this)
        type is (real(kind(1.0d0)))
          call real_array_value (this)
        type is (character(*))
          call string_array_value (this)
        class default
          INSIST(.false.)
        end select
        INSIST(this%values%is_empty())
        this%state = STATE_NAME ! expecting the next parameter name or map end
        status = FYAJL_CONTINUE_PARSING
      else  ! 0-sized array; not allowed.
        this%errmsg = '0-sized arrays are not allowed'
        status = FYAJL_TERMINATE_PARSING
      end if
    case default
      call unexpected_event (this, this%errmsg)
      status = FYAJL_TERMINATE_PARSING
    end select
  contains
    subroutine integer_array_value (this)
      class(plist_builder) :: this
      integer, allocatable :: array(:)
      type(parameter_list), pointer :: plist
#ifdef INTEL_WORKAROUND
      class(*), pointer :: uptr
#endif
      integer :: n
      allocate(array(this%values%size()))
      do n = 1, size(array)
#ifdef INTEL_WORKAROUND
        uptr => this%values%peek()
        select type (uptr)
#else
        select type (uptr => this%values%peek())
#endif
        type is (integer(fyajl_integer_kind))
          array(n) = uptr
        end select
        call this%values%pop()
      end do
      plist => this%pstack%peek() ! current parameter list
      call plist%set (this%name, array) ! create the parameter
    end subroutine
    subroutine logical_array_value (this)
      class(plist_builder) :: this
      logical, allocatable :: array(:)
      type(parameter_list), pointer :: plist
#ifdef INTEL_WORKAROUND
      class(*), pointer :: uptr
#endif
      integer :: n
      allocate(array(this%values%size()))
      do n = 1, size(array)
#ifdef INTEL_WORKAROUND
        uptr => this%values%peek()
        select type (uptr)
#else
        select type (uptr => this%values%peek())
#endif
        type is (logical)
          array(n) = uptr
        end select
        call this%values%pop()
      end do
      plist => this%pstack%peek() ! current parameter list
      call plist%set (this%name, array) ! create the parameter
    end subroutine
    subroutine real_array_value (this)
      class(plist_builder) :: this
      real(kind(1.0d0)), allocatable :: array(:)
      type(parameter_list), pointer :: plist
#ifdef INTEL_WORKAROUND
      class(*), pointer :: uptr
#endif
      integer :: n
      allocate(array(this%values%size()))
      do n = 1, size(array)
#ifdef INTEL_WORKAROUND
        uptr => this%values%peek()
        select type (uptr)
#else
        select type (uptr => this%values%peek())
#endif
        type is (real(fyajl_real_kind))
          array(n) = uptr
        end select
        call this%values%pop()
      end do
      plist => this%pstack%peek() ! current parameter list
      call plist%set (this%name, array) ! create the parameter
    end subroutine
    !! Create the string array parameter value.
    subroutine string_array_value (this)
      class(plist_builder) :: this
      character(:), allocatable :: array(:)
      type(parameter_list), pointer :: plist
#ifdef INTEL_WORKAROUND
      class(*), pointer :: uptr
#endif
      integer :: n
      allocate(character(this%maxlen) :: array(this%values%size()))
      do n = 1, size(array)
#ifdef INTEL_WORKAROUND
        uptr => this%values%peek()
        select type (uptr)
#else
        select type (uptr => this%values%peek())
#endif
        type is (character(*))
          array(n) = uptr
        end select
        call this%values%pop()
      end do
      plist => this%pstack%peek() ! current parameter list
      call plist%set (this%name, array) ! create the parameter
    end subroutine
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
      errmsg = 'scalar value or "]" expected'
    case (STATE_DONE)
      errmsg = 'no further input expected'
    case default
      INSIST(.false.)
    end select
  end subroutine unexpected_event

  subroutine unexpected_array_type (this, errmsg)
    class(plist_builder), intent(in) :: this
    character(:), allocatable :: errmsg
#ifdef INTEL_WORKAROUND
    class(*), pointer :: uptr
    uptr => this%values%peek()
    select type (uptr)
#else
    select type (uptr => this%values%peek())
#endif
    type is (integer)
      errmsg = 'expecting an integer value'
    type is (real(kind(1.0d0)))
      errmsg = 'expecting a real value'
    type is (character(*))
      errmsg = 'expecting a string value'
    type is (logical)
      errmsg = 'expecting a logical value'
    class default
      INSIST(.false.)
    end select
  end subroutine unexpected_array_type

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
    class(*), allocatable :: value
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

  logical function queue_is_empty (this)
    class(value_queue), intent(in) :: this
    queue_is_empty = .not.associated(this%head)
  end function queue_is_empty

  integer function queue_size (this)
    class(value_queue), intent(in) :: this
    queue_size = this%n
  end function queue_size

  subroutine parameter_list_to_json (plist, unit)
    type(parameter_list), intent(in) :: plist
    integer, intent(in) :: unit
    write(unit,'(a)') '{'
    call parameter_list_to_json_aux (plist, '  ', unit)
    write(unit,'(/,a)') '}'
  end subroutine parameter_list_to_json

  recursive subroutine parameter_list_to_json_aux (plist, indent, unit)

    use map_any_type
    use parameter_entry_type

    type(parameter_list), intent(in) :: plist
    character(*), intent(in) :: indent
    integer, intent(in) :: unit
    
    type(parameter_list_iterator) :: piter
    logical :: first_param
#ifdef INTEL_WORKAROUND
    class(*), pointer :: pentry
#endif

    call piter%init (plist)
    first_param = .true.
    do while (.not.piter%at_end())
      if (first_param) then
        first_param = .false.
      else
        write(unit,'(a)') ','
      end if
      write(unit,'(a)',advance='no') indent // '"' // piter%name() // '"'
#ifdef INTEL_WORKAROUND
      pentry => piter%value()
      select type (pentry)
#else
      select type (pentry => piter%value())
#endif
      type is (parameter_list)
        write(unit,'(a)') ': {'
        call parameter_list_to_json_aux (pentry, indent//'  ', unit)
        write(unit,'(/,a)',advance='no') indent // '}'
      type is (any_scalar)
        write(unit,'(a)',advance='no') ': '
        call pentry%write (unit)
      type is (any_vector)
        write(unit,'(a)',advance='no') ': '
        call pentry%write (unit)
      end select
      call piter%next
    end do

  end subroutine parameter_list_to_json_aux

end module parameter_list_json
