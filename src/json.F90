!!
!! JSON
!!
!! Data structures for representing arbitrary JSON documents and
!! procedures for instantiating such objects from JSON text.
!!
!! NB: THIS IS A WORK-IN-PROGRESS!
!!
!! Neil N. Carlson <neil.n.carlson@gmail.com>
!! September 2015
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2015 Neil N. Carlson
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
!!  Refer to http://www.json.org for a description of the JSON syntax.
!!
!!  The abstract type JSON_VALUE represents a JSON value.  An instance will
!!  have one of the following concrete dynamic types:
!!
!!    JSON_INTEGER storing a JSON number without fractional part
!!    JSON_REAL storing a JSON number with fractional part
!!    JSON_STRING storing a JSON string
!!    JSON_BOOLEAN storing the JSON literals true and false
!!    JSON_NULL representing the JSON literal null
!!    JSON_OBJECT storing a JSON object
!!    JSON_ARRAY storing a JSON array
!!
!!  The primitive types have a public component VALUE that stores the value.
!!  Its type is INTEGER for JSON_INTEGER, REAL for JSON_REAL, CHARACTER for
!!  JSON_STRING, and LOGICAL for JSON_BOOLEAN.  The content of the structure
!!  types are accessed via iterators objects.
!!
!!    TYPE(JSON_OBJECT), TARGET  :: VALUE
!!    TYPE(JSON_OBJECT_ITERATOR) :: ITER
!!    ITER = JSON_OBJECT_ITERATOR(VALUE)
!!    DO WHILE (.NOT.ITER%AT_END()) ! order of object members is insignificant
!!      ITER%NAME() returns the name of the pair
!!      ITER%VALUE() returns a CLASS(JSON_VALUE) pointer to the value of the pair
!!      CALL ITER%NEXT
!!    END DO
!!
!!    TYPE(JSON_ARRAY), TARGET  :: VALUE
!!    TYPE(JSON_ARRAY_ITERATOR) :: ITER
!!    ITER = JSON_ARRAY_ITERATOR(VALUE)
!!    DO WHILE (.NOT.ITER%AT_END()) ! order of ARRAY elements *is* significant
!!      ITER%VALUE() returns a CLASS(JSON_VALUE) pointer to the value of the element
!!      CALL ITER%NEXT
!!    END DO
!!
!!  The following procedures instantiate a CLASS(JSON_VALUE) variable by reading
!!  a JSON document from a string or an input unit opened for unformatted stream reading
!!
!!    CALL JSON_FROM_STRING (STRING, VALUE, STAT, ERRMSG)
!!    CALL JSON_FROM_STREAM (UNIT,   VALUE, STAT, ERRMSG)
!!      CHARACTER(*), INTENT(IN) :: STRING
!!      INTEGER, INTENT(IN) :: UNIT
!!      CLASS(JSON_VALUE), ALLOCATABLE, INTENT(OUT) :: VALUE
!!      INTEGER, INTENT(OUT) :: STAT
!!      CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: ERRMSG
!!
!! IMPLEMENTATION NOTES
!!
!!  (1) The yajl parser callback functions rely on the yajl parser to check
!!  that the syntax of the input is valid.  This *greatly* simplifies the
!!  implementation of the callbacks.
!!
!!  (2) The construction of the JSON value involves a significant amount of
!!  data movement from one structure to another.  In the past, I would have
!!  used pointers to avoid copying of actual data, but here I've taken a
!!  different approach.  Since only one reference to a piece of data needs
!!  to exist at any time, I've used allocatable variables for such data and
!!  made use of the move_alloc intrinsic to hand off the data from one
!!  variable to another.
!!
!!  (3) There are cases where an allocatable CLASS(JSON_STRUCT) object needs
!!  to be passed to a procedure where the corresponding dummy argument is
!!  allocatable CLASS(JSON_VALUE).  If the dummy argument were not allocatable
!!  this is allowed since JSON_STRUCT is of CLASS(JSON_VALUE).  However,
!!  because it is allocatable and the procedure could in principle allocate
!!  anything of CLASS(JSON_VALUE), the actual argument must also be of that
!!  class.  This obstacle is finessed by using move_alloc to move the
!!  allocation of the CLASS(JSON_STRUCT) variable to a CLASS(JSON_VALUE)
!!  temporary variable, and passing that variable instead.
!!

module json

  use yajl_fort
  implicit none
  private

  public :: json_from_stream, json_from_string

  type, abstract, public :: json_value
  contains
    procedure(value_write), deferred :: write
  end type

  abstract interface
    subroutine value_write (this, unit)
      import json_value
      class(json_value), intent(in) :: this
      integer, intent(in) :: unit
    end subroutine
  end interface

  type, extends(json_value), public :: json_integer
    integer(fyajl_integer_kind) :: value
  contains
    procedure :: write => json_integer_write
  end type

  type, extends(json_value), public :: json_real
    real(fyajl_real_kind) :: value
  contains
    procedure :: write => json_real_write
  end type

  type, extends(json_value), public :: json_boolean
    logical :: value
  contains
    procedure :: write => json_boolean_write
  end type

  type, extends(json_value), public :: json_string
    character(:), allocatable :: value
  contains
    procedure :: write => json_string_write
  end type

  type, extends(json_value), public :: json_null
  contains
    procedure :: write => json_null_write
  end type

  type, abstract, extends(json_value) :: json_struct
  end type

  type, extends(json_struct), public :: json_object
    private
    type(object_member), pointer :: first => null()
  contains
    procedure :: add => object_add_value
    procedure :: mapped => object_mapped
    procedure :: write => object_write
    final :: json_object_delete
  end type

  type :: object_member
    character(:), allocatable :: name
    class(json_value), allocatable :: value
    type(object_member), pointer :: prev => null(), next => null()
  contains
    final :: object_member_delete
  end type

  type, public :: json_object_iterator
    private
    type(object_member), pointer :: member => null()
  contains
    procedure :: next => obj_iter_next
    procedure :: at_end => obj_iter_at_end
    procedure :: name => obj_iter_name
    procedure :: value => obj_iter_value
  end type json_object_iterator

  !! Defined JSON_OBJECT_ITERATOR structure constructor
  interface json_object_iterator
    procedure json_object_begin
  end interface

  type, extends(json_struct), public :: json_array
    private
    type(array_element), pointer :: first => null(), last => null()
  contains
    procedure :: append => array_append_value
    procedure :: write => array_write
    final :: json_array_delete
  end type

  type :: array_element
    class(json_value), allocatable :: value
    type(array_element), pointer :: next => null()
  contains
    final :: array_element_delete
  end type

  type, public :: json_array_iterator
    private
    type(array_element), pointer :: element => null()
  contains
    procedure :: next => array_iter_next
    procedure :: at_end => array_iter_at_end
    procedure :: value => array_iter_value
  end type json_array_iterator

  !! Defined JSON_ARRAY_ITERATOR structure constructor
  interface json_array_iterator
    procedure json_array_begin
  end interface

  type :: struct_stack
   type(stack_element), pointer :: top => null()
  contains
    procedure :: push => struct_stack_push
    procedure :: pop  => struct_stack_pop
    final :: struct_stack_delete
  end type

  type :: stack_element
    character(:), allocatable :: name
    class(json_struct), allocatable :: struct
    type(stack_element), pointer :: next => null()
  contains
    final :: stack_element_delete
  end type

  type, extends(fyajl_callbacks) :: json_builder
    type(struct_stack) :: stack
    class(json_value), allocatable :: result
    character(:), allocatable :: errmsg
  contains
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

  subroutine json_null_write (this, unit)
    class(json_null), intent(in) :: this
    integer, intent(in) :: unit
    write(unit,'("null")')
  end subroutine json_null_write

  subroutine json_boolean_write (this, unit)
    class(json_boolean), intent(in) :: this
    integer, intent(in) :: unit
    if (this%value) then
      write(unit,'("true")')
    else
      write(unit,'("false")')
    end if
  end subroutine json_boolean_write

  subroutine json_integer_write (this, unit)
    class(json_integer), intent(in) :: this
    integer, intent(in) :: unit
    write(unit,'(i0)') this%value
  end subroutine json_integer_write

  subroutine json_real_write (this, unit)
    class(json_real), intent(in) :: this
    integer, intent(in) :: unit
    write(unit,'(es13.5)') this%value  !FIXME: need proper format
  end subroutine json_real_write

  subroutine json_string_write (this, unit)
    class(json_string), intent(in) :: this
    integer, intent(in) :: unit
    write(unit,'(a)') '"' // this%value // '"'
  end subroutine json_string_write

!!!! JSON_OBJECT TYPE BOUND PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! Final subroutine for JSON_OBJECT objects.  This is recursive because the
  !! CLASS(JSON_VALUE) values in the object may themselves be JSON_OBJECT objects
  recursive subroutine json_object_delete (this)
    type(json_object), intent(inout) :: this
    if (associated(this%first)) deallocate(this%first)
  end subroutine json_object_delete

  !! Final subroutine for OBJECT_MEMBER objects.  This recursively follows the
  !! NEXT pointer.  When deallocating a linked list, only the root needs to be
  !! explicitly deallocated.  When the desire is to deallocate a single
  !! OBJECT_MEMBER object, first nullify the NEXT pointer to prevent the
  !! recursive finalization from deallocating more than it should.
  recursive subroutine object_member_delete (this)
    type(object_member), intent(inout) :: this
    if (associated(this%next)) deallocate(this%next)
  end subroutine object_member_delete

  !! Returns true if a mapping for NAME exists; otherwise returns false.
  logical function object_mapped (this, name)
    class(json_object), intent(in) :: this
    character(*), intent(in) :: name
    object_mapped = associated(find_object_member(this, name))
  end function object_mapped

  !! Adds the (NAME, VALUE) pair to the object.
  !! If NAME is already mapped, its value is replaced by VALUE.
  subroutine object_add_value (this, name, value)
    class(json_object), intent(inout) :: this
    character(*), intent(in) :: name
    class(json_value), allocatable, intent(inout) :: value
    type(object_member), pointer :: member
    member => find_object_member(this, name)
    if (associated(member)) then
      deallocate(member%value)
      call move_alloc (value, member%value)
    else
      call append_object_member (this, new_object_member(name, value))
    end if
  end subroutine

  !! Returns a pointer to the OBJECT_MEMBER having the specified name,
  !! or a null pointer if none was found.
  function find_object_member (this, name) result (member)
    class(json_object), intent(in) :: this
    character(*), intent(in) :: name
    type(object_member), pointer :: member
    member => this%first
    do while (associated(member))
      if (member%name == name) exit
      member => member%next
    end do
  end function find_object_member

  !! Returns a pointer to a new initialized (but unlinked) OBJECT_MEMBER.
  function new_object_member (name, value) result (member)
    character(*), intent(in) :: name
    class(json_value), allocatable, intent(inout) :: value
    type(object_member), pointer :: member
    allocate(member)
    member%name = name
    call move_alloc (value, member%value)
  end function new_object_member

  !! Blindly links the given OBJECT_MEMBER (as made by NEW_OBJECT_MEMBER) to
  !! the end of the list; caller must ensure the name is not already mapped.
  subroutine append_object_member (this, member)
    class(json_object), intent(inout) :: this
    type(object_member), pointer, intent(in) :: member
    type(object_member), pointer :: last
    if (associated(this%first)) then
      last => this%first%prev
      last%next => member
      member%prev => last
      this%first%prev => member
    else
      member%prev => member
      this%first => member
    end if
  end subroutine append_object_member

  recursive subroutine object_write (this, unit)
    class(json_object), intent(in) :: this
    integer, intent(in) :: unit
    type(json_object_iterator) :: iter
    class(json_value), pointer :: value
    logical :: first
    write(unit,'("{")')
    iter = json_object_iterator(this)
    first = .true.
    do while (.not.iter%at_end())
      if (first) then
        first = .false.
      else
        write(unit,'(",")')
      end if
      write(unit,'(a)',advance='no') '"' // iter%name() // '":'
      value => iter%value()
      call value%write (unit)
      call iter%next
    end do
    write(unit,'("}")')
  end subroutine object_write

!!!! JSON_OBJECT_ITERATOR TYPE BOUND PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function json_object_begin (object) result (iter)
    class(json_object), intent(in) :: object
    type(json_object_iterator) :: iter
    iter%member => object%first
  end function json_object_begin

  logical function obj_iter_at_end (this)
    class(json_object_iterator), intent(in) :: this
    obj_iter_at_end = .not.associated(this%member)
  end function obj_iter_at_end

  function obj_iter_name (this) result (name)
    class(json_object_iterator), intent(in) :: this
    character(:), allocatable :: name
    name = this%member%name
  end function obj_iter_name

  function obj_iter_value (this) result (value)
    class(json_object_iterator), intent(in) :: this
    class(json_value), pointer :: value
    value => this%member%value
  end function obj_iter_value

  subroutine obj_iter_next (this)
    class(json_object_iterator), intent(inout) :: this
    if(associated(this%member)) this%member => this%member%next
  end subroutine obj_iter_next


!!!! JSON_ARRAY TYPE BOUND PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! Final subroutine for JSON_ARRAY objects.  This is recursive because the
  !! CLASS(JSON_VALUE) values in the object may themselves be JSON_ARRAY
  !! or contain JSON_ARRAY objects.
  recursive subroutine json_array_delete (this)
    type(json_array), intent(inout) :: this
    if (associated(this%first)) deallocate(this%first)
  end subroutine json_array_delete

  !! Final subroutine for ARRAY_ELEMENT objects.  This recursively follows
  !! the NEXT pointer.  When deallocating a linked list, only the root needs
  !! to be explicitly deallocated.  When the desire is to deallocate a single
  !! ARRAY_ELEMENT object, first nullify the NEXT pointer to prevent the
  !! recursive finalization from deallocating more than it should.
  recursive subroutine array_element_delete (this)
    type(array_element), intent(inout) :: this
    if (associated(this%next)) deallocate(this%next)
  end subroutine array_element_delete

  !! Appends VALUE to the ordered list of array values.
  subroutine array_append_value (this, value)
    class(json_array), intent(inout) :: this
    class(json_value), allocatable, intent(inout) :: value
    type(array_element), pointer :: element
    allocate(element)
    call move_alloc (value, element%value)
    call append_array_element (this, element)
  end subroutine array_append_value

  !! Appends the given ARRAY_ELEMENT to the end of the list.
  subroutine append_array_element (this, element)
    class(json_array), intent(inout) :: this
    type(array_element), pointer, intent(in) :: element
    type(array_element), pointer :: last
    if (associated(this%last)) then
      last => this%last
      last%next => element
      this%last => element
    else
      this%first => element
      this%last  => element
    end if
  end subroutine append_array_element

  recursive subroutine array_write (this, unit)
    class(json_array), intent(in) :: this
    integer, intent(in) :: unit
    type(json_array_iterator) :: iter
    class(json_value), pointer :: value
    logical :: first
    write(unit,'("[")')
    iter = json_array_iterator(this)
    first = .true.
    do while (.not.iter%at_end())
      if (first) then
        first = .false.
      else
        write(unit,'(",")')
      end if
      value => iter%value()
      call value%write (unit)
      call iter%next
    end do
    write(unit,'("]")')
  end subroutine array_write

!!!! JSON_ARRAY_ITERATOR TYPE BOUND PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function json_array_begin (array) result (iter)
    class(json_array), intent(in) :: array
    type(json_array_iterator) :: iter
    iter%element => array%first
  end function json_array_begin

  logical function array_iter_at_end (this)
    class(json_array_iterator), intent(in) :: this
    array_iter_at_end = .not.associated(this%element)
  end function array_iter_at_end

  function array_iter_value (this) result (value)
    class(json_array_iterator), intent(in) :: this
    class(json_value), pointer :: value
    value => this%element%value
  end function array_iter_value

  subroutine array_iter_next (this)
    class(json_array_iterator), intent(inout) :: this
    if(associated(this%element)) this%element => this%element%next
  end subroutine array_iter_next

!!!! STRUCT_STACK TYPE BOUND PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! Final subroutine for STRUCT_STACK objects.
  subroutine struct_stack_delete (this)
    type(struct_stack), intent(inout) :: this
    if (associated(this%top)) deallocate(this%top)
  end subroutine struct_stack_delete

  !! Final subroutine for STRUCT_ELEMENT objects.  This recursively follows
  !! the NEXT pointer.  When deallocating a linked list, only the root needs
  !! to be explicitly deallocated.  When the desire is to deallocate a single
  !! STRUCT_ELEMENT object, first nullify the NEXT pointer to prevent the
  !! recursive finalization from deallocating more than it should.
  recursive subroutine stack_element_delete (this)
    type(stack_element), intent(inout) :: this
    if (associated(this%next)) deallocate(this%next)
  end subroutine stack_element_delete

  !! Push the struct onto the top of the stack with optional name.
  subroutine struct_stack_push (this, struct, name)
    class(struct_stack), intent(inout) :: this
    class(json_struct), allocatable, intent(inout) :: struct
    character(*), intent(in), optional :: name
    type(stack_element), pointer :: top
    allocate(top)
    call move_alloc (struct, top%struct)
    if (present(name)) top%name = name
    top%next => this%top
    this%top => top
  end subroutine struct_stack_push

  !! Pop the struct off the top of the stack with optional name.
  subroutine struct_stack_pop (this, struct, name)
    class(struct_stack), intent(inout) :: this
    class(json_struct), allocatable, intent(out) :: struct
    character(:), allocatable, intent(out), optional :: name
    type(stack_element), pointer :: top
    if (associated(this%top)) then
      top => this%top
      call move_alloc (top%struct, struct)
      if (present(name)) call move_alloc (top%name, name)
      this%top => top%next
      top%next => null()
      deallocate(top)
    end if
  end subroutine struct_stack_pop

!!!! YAJL_FORT PARSER CALLBACK FUNCTIONS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer function start_map (this) result (status)
    class(json_builder) :: this
    class(json_struct), allocatable :: struct
    allocate(json_object :: struct)
    call this%stack%push (struct)
    status = FYAJL_CONTINUE_PARSING
  end function start_map

  integer function end_map (this) result (status)
    class(json_builder) :: this
    class(json_struct), allocatable :: value
    class(json_value), allocatable :: val
    call this%stack%pop (value)
    call move_alloc (value, val)  ! see Note 3
    status = store_value(this, val)
  end function end_map

  integer function map_key (this, value) result (status)
    class(json_builder) :: this
    character(*), intent(in) :: value
    class(json_struct), allocatable :: struct
    call this%stack%pop (struct)
    select type (struct)
    type is (json_object) ! this must be true if yajl is doing its job
      if (struct%mapped(value)) then
        this%errmsg = 'object member with this name already exists'
        status = FYAJL_TERMINATE_PARSING
        return
      end if
    end select
    call this%stack%push (struct, value)
    status = FYAJL_CONTINUE_PARSING
  end function map_key

  integer function start_array (this) result (status)
    class(json_builder) :: this
    class(json_struct), allocatable :: struct
    allocate(json_array :: struct)
    call this%stack%push (struct)
    status = FYAJL_CONTINUE_PARSING
  end function start_array

  integer function end_array (this) result (status)
    class(json_builder) :: this
    class(json_struct), allocatable :: value
    class(json_value), allocatable :: val
    call this%stack%pop (value) ! json_array if yajl is doing its job
    call move_alloc (value, val)  ! see Note 3
    status = store_value(this, val)
  end function end_array

  integer function null_value (this) result (status)
    class(json_builder) :: this
    class(json_value), allocatable :: val
    val = json_null()
    status = store_value(this, val)
  end function null_value

  integer function integer_value (this, value) result (status)
    class(json_builder) :: this
    integer(fyajl_integer_kind), intent(in) :: value
    class(json_value), allocatable :: val
    val = json_integer(value)
    status = store_value(this, val)
  end function integer_value

  integer function double_value (this, value) result (status)
    class(json_builder) :: this
    real(fyajl_real_kind), intent(in) :: value
    class(json_value), allocatable :: val
    val = json_real(value)
    status = store_value(this, val)
  end function double_value

  integer function logical_value (this, value) result (status)
    class(json_builder) :: this
    logical, intent(in) :: value
    class(json_value), allocatable :: val
    val = json_boolean(value)
    status = store_value(this, val)
  end function logical_value

  integer function string_value (this, value) result (status)
    class(json_builder) :: this
    character(*), intent(in) :: value
    class(json_value), allocatable :: val
    val = json_string(value)
    status = store_value(this, val)
  end function string_value

  !! This auxiliary procedure stores the passed json_value.  If an object or
  !! array is under construction, it is added to that structure; otherwise,
  !! the json_value is the final result from parsing.

  integer function store_value (this, value) result (status)
    class(json_builder) :: this
    class(json_value), allocatable, intent(inout) :: value
    class(json_struct), allocatable :: struct
    character(:), allocatable :: name
    call this%stack%pop (struct, name)
    if (allocated(struct)) then
      select type (struct)
      type is (json_object)
        call struct%add (name, value)
      type is (json_array)
        call struct%append (value)
      end select
      call this%stack%push (struct)
    else
      call move_alloc (value, this%result)
    end if
    status = FYAJL_CONTINUE_PARSING
  end function store_value

  subroutine json_from_stream (unit, value, stat, errmsg, bufsize)

    use,intrinsic :: iso_fortran_env, only: iostat_end
    use,intrinsic :: iso_c_binding, only: c_char

    integer, intent(in) :: unit
    class(json_value), allocatable, intent(out) :: value
    integer, intent(out) :: stat
    character(:), allocatable, intent(out) :: errmsg
    integer, intent(in), optional :: bufsize  ! for testing purposes mostly

    type(json_builder), target :: builder
    type(fyajl_parser), target :: parser
    type(fyajl_status) :: yajl_stat

    integer, parameter :: BUFFER_SIZE = 4096
    character(kind=c_char), allocatable :: buffer(:)
    integer :: buflen, last_pos, curr_pos, ios

    !TODO: check unit is open with unformatted stream access

    stat = 0

    if (present(bufsize)) then
      allocate(buffer(bufsize))
    else
      allocate(buffer(BUFFER_SIZE))
    end if

    !! Initialize the parser
    call parser%init (builder)
    call parser%set_option (FYAJL_ALLOW_COMMENTS)
    call parser%set_option (FYAJL_ALLOW_TRAILING_GARBAGE)

    inquire(unit,pos=last_pos)  ! starting position in stream
    do
      do buflen = 1, size(buffer)
        read(unit,iostat=ios) buffer(buflen)
        if (ios /= 0) exit
      end do
      if (ios /= 0 .and. ios /= iostat_end) then
        allocate(character(16) :: errmsg)
        write(errmsg,'(i0)') ios
        errmsg = 'read error: iostat=' // trim(errmsg)
        stat = -1
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
        if (yajl_stat /= FYAJL_STATUS_OK) then
          errmsg = fyajl_get_error(parser, .false., buffer(:buflen))
          if (yajl_stat == FYAJL_STATUS_CLIENT_CANCELED) errmsg = errmsg // builder%errmsg
          stat = -1
        else
          call move_alloc (builder%result, value)
        end if
        exit
      end if
    end do

  end subroutine json_from_stream

  subroutine json_from_string (string, value, stat, errmsg)

    use,intrinsic :: iso_c_binding, only: c_loc, c_f_pointer

    character(*), intent(in), target :: string
    class(json_value), allocatable, intent(out) :: value
    integer, intent(out) :: stat
    character(:), allocatable, intent(out) :: errmsg

    type(json_builder), target :: builder
    type(fyajl_parser), target :: parser
    type(fyajl_status) :: yajl_stat
    character, pointer :: buffer(:)

    stat = 0

    !! This may be a little dicey...
    call c_f_pointer (c_loc(string(1:1)), buffer, shape=[len(string)])

    call parser%init (builder)
    !call parser%set_option (FYAJL_ALLOW_COMMENTS)
    !call parser%set_option (FYAJL_ALLOW_TRAILING_GARBAGE)
    call parser%parse (buffer, yajl_stat)
    if (yajl_stat /= FYAJL_STATUS_OK) then
      errmsg = fyajl_get_error(parser, .true., buffer)
      if (yajl_stat == FYAJL_STATUS_CLIENT_CANCELED) errmsg = errmsg // builder%errmsg
      stat = -1
      return
    end if
    call parser%complete_parse (yajl_stat)
    if (yajl_stat /= FYAJL_STATUS_OK) then
      errmsg = fyajl_get_error(parser, .false., buffer)
      if (yajl_stat == FYAJL_STATUS_CLIENT_CANCELED) errmsg = errmsg // builder%errmsg
      stat = -1
      return
    end if
    call move_alloc (builder%result, value)

  end subroutine json_from_string

end module json
