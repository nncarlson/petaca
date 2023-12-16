!!
!! PARAMETER_LIST_TYPE
!!
!! This module defines a group of derived types that implement a hierarchical
!! data structure for storing lists of parameter name/value pairs. A value can
!! be a scalar or array of arbitrary type. Such parameter list structures are
!! meant to be used to pass information between program units, encapsulating
!! data that would otherwise be passed as individual procedure arguments.
!! The primary client interface is the PARAMETER_LIST derived type.
!!
!! Neil N. Carlson <neil.n.carlson@gmail.com>
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2011, 2013, 2014, 2023 Neil N. Carlson
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

module parameter_list_type

  use,intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
  implicit none
  private

  type, abstract, public :: parameter_value
    !! meta data associated with the parameter will go here
  contains
#ifdef NAG_BUG20231206
    procedure, non_overridable :: copy
#else
    procedure, non_overridable, private :: copy
#endif
    generic :: assignment(=) => copy
    procedure(copy), deferred :: copy_impl
    !TODO: make write a deferred procedure?
  end type

  type, extends(parameter_value), public :: any_scalar
    private
    class(*), allocatable :: value
  contains
    procedure :: set_value => any_scalar_set_value
    procedure :: value_ptr => any_scalar_value_ptr  !TODO: needed? delete?
    generic   :: get_value => &
        any_scalar_get_value, any_scalar_get_character, any_scalar_get_logical, &
        any_scalar_get_int32, any_scalar_get_int64, any_scalar_get_real32, any_scalar_get_real64
    procedure, private :: &
        any_scalar_get_value, any_scalar_get_character, any_scalar_get_logical, &
        any_scalar_get_int32, any_scalar_get_int64, any_scalar_get_real32, any_scalar_get_real64
    procedure :: copy_impl => any_scalar_copy
  end type

  !! User-defined constructor; needed due to private components.
  interface any_scalar
    procedure any_scalar_value
  end interface

  type, extends(parameter_value), public :: any_vector
    private
    class(*), allocatable :: value(:)
  contains
    procedure :: set_value => any_vector_set_value
    procedure :: value_ptr => any_vector_value_ptr
    generic   :: get_value => &
        any_vector_get_value, any_vector_get_character, any_vector_get_logical, &
        any_vector_get_int32, any_vector_get_int64, any_vector_get_real32, any_vector_get_real64
    procedure, private :: &
        any_vector_get_value, any_vector_get_character, any_vector_get_logical, &
        any_vector_get_int32, any_vector_get_int64, any_vector_get_real32, any_vector_get_real64
    procedure :: copy_impl => any_vector_copy
  end type

  !! User-defined constructor; needed due to private components.
  interface any_vector
    procedure any_vector_value
  end interface

  type, extends(parameter_value), public :: any_matrix
    private
    class(*), allocatable :: value(:,:)
  contains
    procedure :: set_value => any_matrix_set_value
    procedure :: value_ptr => any_matrix_value_ptr
    generic   :: get_value => &
        any_matrix_get_value, any_matrix_get_character, any_matrix_get_logical, &
        any_matrix_get_int32, any_matrix_get_int64, any_matrix_get_real32, any_matrix_get_real64
    procedure, private :: &
        any_matrix_get_value, any_matrix_get_character, any_matrix_get_logical, &
        any_matrix_get_int32, any_matrix_get_int64, any_matrix_get_real32, any_matrix_get_real64
    procedure :: copy_impl => any_matrix_copy
  end type

  !! User-defined constructor; needed due to private components.
  interface any_matrix
    procedure any_matrix_value
  end interface

  type, extends(parameter_value), public :: parameter_list
    private
    character(:), allocatable :: path_
    type(list_item), pointer :: first => null()
  contains
    procedure :: path
    procedure :: set_path
    procedure :: is_parameter
    procedure :: is_sublist
    procedure :: is_scalar
    procedure :: is_vector
    procedure :: is_matrix
    procedure :: count
    procedure :: sublist
    generic :: set => set_scalar, set_vector, set_matrix
    procedure, private :: set_scalar, set_vector, set_matrix
    generic :: get_any => get_any_scalar, get_any_vector, get_any_matrix
    procedure, private :: get_any_scalar, get_any_vector, get_any_matrix
    generic :: get => get_scalar_string, get_vector_string, get_matrix_string, &
        get_scalar_logical, get_vector_logical, get_matrix_logical, &
        get_scalar_int32, get_vector_int32, get_matrix_int32, &
        get_scalar_int64, get_vector_int64, get_matrix_int64, &
        get_scalar_real32, get_vector_real32, get_matrix_real32, &
        get_scalar_real64, get_vector_real64, get_matrix_real64
    procedure, private :: get_scalar_string, get_vector_string, get_matrix_string, &
        get_scalar_logical, get_vector_logical, get_matrix_logical, &
        get_scalar_int32, get_vector_int32, get_matrix_int32, &
        get_scalar_int64, get_vector_int64, get_matrix_int64, &
        get_scalar_real32, get_vector_real32, get_matrix_real32, &
        get_scalar_real64, get_vector_real64, get_matrix_real64
    procedure :: copy_impl => parameter_list_copy
    final :: dealloc_parameter_list
  end type

  type :: list_item
    character(:), allocatable :: name
    class(parameter_value), allocatable :: value
    type(list_item), pointer :: next => null(), prev => null()
  contains
    final :: dealloc_list_item
  end type

  type, public :: parameter_list_iterator
    private
    type(list_item), pointer :: item => null()
    logical :: sublists_only = .false.
  contains
    procedure :: next => iter_next
    procedure :: at_end => iter_at_end
    procedure :: name => iter_name
    procedure :: value => iter_value
    procedure :: is_sublist => iter_is_sublist
    procedure :: is_scalar => iter_is_scalar
    procedure :: is_vector => iter_is_vector
    procedure :: is_matrix => iter_is_matrix
    procedure :: sublist => iter_sublist
    procedure :: scalar => iter_scalar
    procedure :: vector => iter_vector
    procedure :: matrix => iter_matrix
    procedure :: count => iter_count
  end type parameter_list_iterator

  interface parameter_list_iterator
    procedure iter_begin
  end interface

contains

  !! Base class defined assignment operator using the non-virtual interface
  !! idiom. Checks compatibility of the arguments and then delegates to the
  !! extension-specific implementation.

  recursive subroutine copy(lhs, rhs)
    class(parameter_value), intent(inout) :: lhs
    class(parameter_value), intent(in) :: rhs
    if (same_type_as(lhs, rhs)) then
      call lhs%copy_impl(rhs)
    else
      error stop 'parameter_value: assignment between different dynamic types'
    end if
  end subroutine

!!!! SCALAR_ANY TYPE BOUND PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! User-defined constructor for ANY_SCALAR objects.
  function any_scalar_value(value) result(obj)
    class(*), intent(in) :: value
    type(any_scalar) :: obj
    call any_scalar_set_value(obj, value)
  end function

  !! Should have the same effect as intrinsic assignment of ANY_SCALAR objects.
  subroutine any_scalar_copy(lhs, rhs)
    class(any_scalar), intent(inout) :: lhs
    class(parameter_value), intent(in) :: rhs
    if (allocated(lhs%value)) deallocate(lhs%value)
    select type (rhs)
    type is (any_scalar)
#if defined(NAG_BUG20231204) || defined(INTEL_BUG20231205)
      if (allocated(lhs%value)) deallocate(lhs%value)
      allocate(lhs%value, source=rhs%value)
#else
      lhs%value = rhs%value
#endif
    end select
  end subroutine

  !! Set value; works for any type.
  subroutine any_scalar_set_value(this, value)
    class(any_scalar), intent(out) :: this
    class(*), intent(in) :: value
    !allocate(this%value, source=value)
    this%value = value
  end subroutine

  !! Get value; works for any type.
  subroutine any_scalar_get_value(this, value)
    class(any_scalar), intent(in) :: this
    class(*), allocatable, intent(out) :: value
    !allocate(value, source=this%value)
    value = this%value
  end subroutine

  !! Return a pointer to the value; works for any type.
  function any_scalar_value_ptr(this) result(uptr)
    class(any_scalar), target, intent(in) :: this
    class(*), pointer :: uptr
    uptr => this%value
  end function

  !! Get value procedures for specific types !!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine any_scalar_get_int32(this, value, errc)
    class(any_scalar), intent(in) :: this
    integer(int32), intent(out) :: value
    logical, intent(out) :: errc
    select type (v => this%value)
    type is (integer(int32))
      value = v
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

  subroutine any_scalar_get_int64(this, value, errc)
    class(any_scalar), intent(in) :: this
    integer(int64), intent(out) :: value
    logical, intent(out) :: errc
    select type (v => this%value)
    type is (integer(int64))
      value = v
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

  subroutine any_scalar_get_real32(this, value, errc)
    class(any_scalar), intent(in) :: this
    real(real32), intent(out) :: value
    logical, intent(out) :: errc
    select type (v => this%value)
    type is (real(real32))
      value = v
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

  subroutine any_scalar_get_real64(this, value, errc)
    class(any_scalar), intent(in) :: this
    real(real64), intent(out) :: value
    logical, intent(out) :: errc
    select type (v => this%value)
    type is (real(real64))
      value = v
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

  subroutine any_scalar_get_character(this, value, errc)
    class(any_scalar), intent(in) :: this
    character(:), allocatable, intent(out) :: value
    logical, intent(out) :: errc
    select type (v => this%value)
    type is (character(*))
      value = v
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

  subroutine any_scalar_get_logical(this, value, errc)
    class(any_scalar), intent(in) :: this
    logical, intent(out) :: value
    logical, intent(out) :: errc
    select type (v => this%value)
    type is (logical)
      value = v
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

!!!! VECTOR_ANY TYPE BOUND PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! User-defined constructor for ANY_VECTOR objects.
  function any_vector_value(value) result(obj)
    class(*), intent(in) :: value(:)
    type(any_vector) :: obj
    call any_vector_set_value(obj, value)
  end function

  !! Should have the same effect as intrinsic assignment of ANY_VECTOR objects.
  subroutine any_vector_copy(lhs, rhs)
    class(any_vector), intent(inout) :: lhs
    class(parameter_value), intent(in) :: rhs
    select type (rhs)
    type is (any_vector)
#ifdef INTEL_BUG20231205
      if (allocated(lhs%value)) deallocate(lhs%value)
      allocate(lhs%value, source=rhs%value)
#else
      lhs%value = rhs%value
#endif
    end select
  end subroutine

  !! Set value; works for any type.
  subroutine any_vector_set_value(this, value)
    class(any_vector), intent(out) :: this
    class(*), intent(in) :: value(:)
    this%value = value
  end subroutine

  !! Get value; works for any type.
  subroutine any_vector_get_value(this, value)
    class(any_vector), intent(in) :: this
    class(*), allocatable, intent(out) :: value(:)
    value = this%value
  end subroutine

  !! Return a pointer to the value; works for any type.
  function any_vector_value_ptr(this) result(uptr)
    class(any_vector), target, intent(in) :: this
    class(*), pointer :: uptr(:)
    uptr => this%value
  end function

  !! Return a int32 kind integer value.
  subroutine any_vector_get_int32(this, value, errc)
    class(any_vector), intent(in) :: this
    integer(int32), allocatable :: value(:)
    logical, intent(out) :: errc
    select type (v => this%value)
    type is (integer(int32))
      value = v
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

  !! Get value procedures for specific types !!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine any_vector_get_int64(this, value, errc)
    class(any_vector), intent(in) :: this
    integer(int64), allocatable :: value(:)
    logical, intent(out) :: errc
    select type (v => this%value)
    type is (integer(int64))
      value = v
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

  subroutine any_vector_get_real32(this, value, errc)
    class(any_vector), intent(in) :: this
    real(real32), allocatable :: value(:)
    logical, intent(out) :: errc
    select type (v => this%value)
    type is (real(real32))
      value = v
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

  subroutine any_vector_get_real64(this, value, errc)
    class(any_vector), intent(in) :: this
    real(real64), allocatable :: value(:)
    logical, intent(out) :: errc
    select type (v => this%value)
    type is (real(real64))
      value = v
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

  subroutine any_vector_get_character(this, value, errc)
    class(any_vector), intent(in) :: this
    character(:), allocatable, intent(out) :: value(:)
    logical, intent(out) :: errc
    select type (v => this%value)
    type is (character(*))
      value = v
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

  subroutine any_vector_get_logical(this, value, errc)
    class(any_vector), intent(in) :: this
    logical, allocatable, intent(out) :: value(:)
    logical, intent(out) :: errc
    select type (v => this%value)
    type is (logical)
      value = v
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

!!!! MATRIX_ANY TYPE BOUND PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! User-defined constructor for ANY_MATRIX objects.
  function any_matrix_value(value) result(obj)
    class(*), intent(in) :: value(:,:)
    type(any_matrix) :: obj
    call any_matrix_set_value(obj, value)
  end function

  !! Should have the same effect as intrinsic assignment of ANY_MATRIX objects.
  subroutine any_matrix_copy(lhs, rhs)
    class(any_matrix), intent(inout) :: lhs
    class(parameter_value), intent(in) :: rhs
    select type (rhs)
    type is (any_matrix)
#ifdef INTEL_BUG20231205
      if (allocated(lhs%value)) deallocate(lhs%value)
      allocate(lhs%value, source=rhs%value)
#else
      lhs%value = rhs%value
#endif
    end select
  end subroutine

  !! Set value; works for any type.
  subroutine any_matrix_set_value(this, value)
    class(any_matrix), intent(out) :: this
    class(*), intent(in) :: value(:,:)
    this%value = value
  end subroutine

  !! Get value; works for any type.
  subroutine any_matrix_get_value(this, value)
    class(any_matrix), intent(in) :: this
    class(*), allocatable, intent(out) :: value(:,:)
    value = this%value
  end subroutine

  !! Return a pointer to the value; works for any type.
  function any_matrix_value_ptr(this) result(uptr)
    class(any_matrix), target, intent(in) :: this
    class(*), pointer :: uptr(:,:)
    uptr => this%value
  end function

  !! Get value procedures for specific types !!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine any_matrix_get_int32(this, value, errc)
    class(any_matrix), intent(in) :: this
    integer(int32), allocatable :: value(:,:)
    logical, intent(out) :: errc
    select type (v => this%value)
    type is (integer(int32))
      value = v
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

  subroutine any_matrix_get_int64(this, value, errc)
    class(any_matrix), intent(in) :: this
    integer(int64), allocatable :: value(:,:)
    logical, intent(out) :: errc
    select type (v => this%value)
    type is (integer(int64))
      value = v
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

  subroutine any_matrix_get_real32(this, value, errc)
    class(any_matrix), intent(in) :: this
    real(real32), allocatable :: value(:,:)
    logical, intent(out) :: errc
    select type (v => this%value)
    type is (real(real32))
      value = v
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

  subroutine any_matrix_get_real64(this, value, errc)
    class(any_matrix), intent(in) :: this
    real(real64), allocatable :: value(:,:)
    logical, intent(out) :: errc
    select type (v => this%value)
    type is (real(real64))
      value = v
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

  subroutine any_matrix_get_character(this, value, errc)
    class(any_matrix), intent(in) :: this
    character(:), allocatable, intent(out) :: value(:,:)
    logical, intent(out) :: errc
    select type (v => this%value)
    type is (character(*))
      value = v
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

  subroutine any_matrix_get_logical(this, value, errc)
    class(any_matrix), intent(in) :: this
    logical, allocatable, intent(out) :: value(:,:)
    logical, intent(out) :: errc
    select type (v => this%value)
    type is (logical)
      value = v
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

!!!! PARAMETER_LIST TYPE BOUND PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! Final procedure for PARAMETER_LIST objects.
  recursive subroutine dealloc_parameter_list(this)
    type(parameter_list), intent(inout) :: this
    if (associated(this%first)) deallocate(this%first)
  end subroutine

  !! Final procedure for LIST_ITEM objects.  This recursively follows the
  !! NEXT pointer. When deallocating a linked-list structure only the root
  !! needs to be explicitly deallocated. When the desire is to deallocate a
  !! single LIST_ITEM object, first nullify the NEXT point to prevent the
  !! recursive finalization from possibly deallocating more than it should.

  recursive subroutine dealloc_list_item(this)
    type(list_item), intent(inout) :: this
    if (associated(this%next)) deallocate(this%next)
  end subroutine

  !! Defined assignment subroutine for PARAMETER_LIST_OBJECTS. Makes a deep
  !! copy of the hierarchical parameter list structure. Terminal values --
  !! those stored as class(*) components of ANY_SCALAR, ANY_VECTOR, and
  !! ANY_MATRIX -- are themselves shallow copies.

  recursive subroutine parameter_list_copy(lhs, rhs)
    class(parameter_list), intent(inout) :: lhs
    class(parameter_value), intent(in) :: rhs
    select type (rhs)
    type is (parameter_list)
      if (allocated(rhs%path_)) then
        lhs%path_ = rhs%path_
      else
        if (allocated(lhs%path_)) deallocate(lhs%path_)
      end if
      if (associated(lhs%first, rhs%first)) return  ! rhs and lhs are the same
      if (associated(lhs%first)) deallocate(lhs%first)
      if (associated(rhs%first)) lhs%first => list_item_copy(rhs%first)
    end select
  end subroutine

  !! Returns the path of this parameter list.
  function path(this)
    class(parameter_list), intent(in) :: this
    character(:), allocatable :: path
    if (allocated(this%path_)) then
      path = this%path_
    else
      path = '$'
    end if
  end function

  !! Sets the path of this parameter list.
  subroutine set_path(this, path)
    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: path
    this%path_ = path
  end subroutine

  !! Returns true if the named parameter exists.
  logical function is_parameter(this, name)
    class(parameter_list), intent(in) :: this
    character(*), intent(in) :: name
    is_parameter = associated(find_pval(this, name))
  end function

  !! Returns true if the named parameter exists and is a sublist.
  logical function is_sublist(this, name)
    class(parameter_list), intent(in) :: this
    character(*), intent(in) :: name
    is_sublist = associated(cast_to_parameter_list(find_pval(this, name)))
  end function

  !! Returns true if the named parameter exists and is scalar-valued.
  logical function is_scalar(this, name)
    class(parameter_list), intent(in) :: this
    character(*), intent(in) :: name
    is_scalar = associated(cast_to_any_scalar(find_pval(this, name)))
  end function

  !! Returns true if the named parameter exists and is vector-valued.
  logical function is_vector(this, name)
    class(parameter_list), intent(in) :: this
    character(*), intent(in) :: name
    is_vector = associated(cast_to_any_vector(find_pval(this, name)))
  end function

  !! Returns true if the named parameter exists and is matrix-valued.
  logical function is_matrix(this, name)
    class(parameter_list), intent(in) :: this
    character(*), intent(in) :: name
    is_matrix = associated(cast_to_any_matrix(find_pval(this, name)))
  end function

  !! Returns the number of stored parameters (of any kind) in the list.
  integer function count(this)
    class(parameter_list), intent(in) :: this
    type(list_item), pointer :: item
    count = 0
    item => this%first
    do while (associated(item))
      count = count + 1
      item => item%next
    end do
  end function

  !! Sets the scalar value of the named parameter.  If the parameter exists,
  !! its value, which must be of type ANY_SCALAR, is overwritten with the
  !! given value.  If an error is encountered, STAT and ERRMSG return info.

  subroutine set_scalar(this, name, value, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    class(*), intent(in) :: value
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    type(any_scalar), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_scalar(find_pval(this, name))
    if (associated(pval)) then
      call pval%set_value(value)
    else if (this%is_parameter(name)) then
      call error('not a scalar-valued parameter: "' // name // '"', stat, errmsg)
    else
      call append_list_item(this, new_list_item(name, any_scalar(value)))
    end if

  end subroutine

  !! Sets the rank-1 array value of the named parameter.  If the parameter
  !! exists, its value, which must be of type ANY_VECTOR, is overwritten with
  !! the given value. If an error is encountered, STAT and ERRMSG return info.

  subroutine set_vector(this, name, value, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    class(*), intent(in) :: value(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    type(any_vector), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_vector(find_pval(this, name))
    if (associated(pval)) then
      call pval%set_value(value)
    else if (this%is_parameter(name)) then
      call error('not a vector-valued parameter: "' // name // '"', stat, errmsg)
    else
      call append_list_item(this, new_list_item(name, any_vector(value)))
    end if

  end subroutine

  !! Sets the rank-2 array value of the named parameter.  If the parameter
  !! exists, its value, which must be of type ANY_MATRIX, is overwritten with
  !! the given value. If an error is encountered, STAT and ERRMSG return info.

  subroutine set_matrix(this, name, value, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    class(*), intent(in) :: value(:,:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    type(any_matrix), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_matrix(find_pval(this, name))
    if (associated(pval)) then
      call pval%set_value(value)
    else if (this%is_parameter(name)) then
      call error('not a matrix-valued parameter: "' // name // '"', stat, errmsg)
    else
      call append_list_item(this, new_list_item(name, any_matrix(value)))
    end if

  end subroutine

  !! Returns a pointer to the named parameter sublist. It is an error if the
  !! parameter exists and is not a sublist. An empty sublist parameter is
  !! created if necessary.

  recursive function sublist(this, name, stat, errmsg) result(list)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    class(parameter_list), pointer :: list
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    call error_clear(stat, errmsg)
    list => cast_to_parameter_list(find_pval(this,name))
    if (associated(list)) then
      return
    else if (this%is_parameter(name)) then
      call error('parameter is not a sublist: "' // name // '"', stat, errmsg)
    else
      call append_list_item(this, new_list_item(name, parameter_list(this%path()//'.'//name)))
      list => this%sublist(name)
    end if

  end function

  !! Returns the scalar value of the named parameter in the CLASS(*) allocatable
  !! variable VALUE. If the named parameter does not exist and the optional
  !! argument DEFAULT is present, the parameter is created and assigned the
  !! value specified by DEFAULT, and that value returned by VALUE.  Otherwise it
  !! is an error. It is an error if the parameter exists and is not scalar-valued.

  recursive subroutine get_any_scalar(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    class(*), allocatable, intent(out) :: value
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    class(*), intent(in), optional :: default

    type(any_scalar), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_scalar(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value)
    else if (this%is_parameter(name)) then
      call error('not a scalar-valued parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get_any(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine

  !! Returns the vector value of the named parameter in the CLASS(*) allocatable
  !! array VALUE. If the named parameter does not exist and the optional array
  !! DEFAULT is present, the parameter is created and assigned the value
  !! specified by DEFAULT, and that value returned by VALUE. Otherwise it is an
  !! error. It is an error if the parameter exists and is not vector-valued.

  recursive subroutine get_any_vector(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    class(*), allocatable, intent(out) :: value(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    class(*), intent(in), optional :: default(:)

    type(any_vector), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_vector(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value)
    else if (this%is_parameter(name)) then
      call error('not a vector-valued parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get_any(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine

  !! Returns the matrix value of the named parameter in the CLASS(*) allocatable
  !! array VALUE. If the named parameter does not exist and the optional array
  !! DEFAULT is present, the parameter is created and assigned the value
  !! specified by DEFAULT, and that value returned by VALUE. Otherwise it is an
  !! error. It is an error if the parameter exists and is not matrix-valued.

  recursive subroutine get_any_matrix(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    class(*), allocatable, intent(out) :: value(:,:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    class(*), intent(in), optional :: default(:,:)

    type(any_matrix), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_matrix(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value)
    else if (this%is_parameter(name)) then
      call error('not a matrix-valued parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get_any(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine

  !! The follow subroutines get the scalar value of the named parameter for
  !! various specific types. If the parameter doesn't exist, it is created
  !! with the specified default value, if specified; otherwise it is an error.
  !! It is an error if the parameter value type doesn't match the type of the
  !! value argument.

  recursive subroutine get_scalar_int32(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    integer(int32), intent(out) :: value
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    integer(int32), intent(in), optional :: default

    logical :: errc
    type(any_scalar), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_scalar(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value, errc)
      if (errc) call error('not an integer(int32) parameter: "' // name // '"', stat, errmsg)
    else if (this%is_parameter(name)) then
      call error('not a scalar parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine get_scalar_int32

  recursive subroutine get_scalar_int64(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    integer(int64), intent(out) :: value
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    integer(int64), intent(in), optional :: default

    logical :: errc
    type(any_scalar), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_scalar(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value, errc)
      if (errc) call error('not an integer(int64) parameter: "' // name // '"', stat, errmsg)
    else if (this%is_parameter(name)) then
      call error('not a scalar parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine get_scalar_int64

  recursive subroutine get_scalar_real32(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    real(real32), intent(out) :: value
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    real(real32), intent(in), optional :: default

    logical :: errc
    type(any_scalar), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_scalar(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value, errc)
      if (errc) call error('not an real(real32) parameter: "' // name // '"', stat, errmsg)
    else if (this%is_parameter(name)) then
      call error('not a scalar parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine get_scalar_real32

  recursive subroutine get_scalar_real64(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    real(real64), intent(out) :: value
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    real(real64), intent(in), optional :: default

    logical :: errc
    type(any_scalar), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_scalar(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value, errc)
      if (errc) call error('not an real(real64) parameter: "' // name // '"', stat, errmsg)
    else if (this%is_parameter(name)) then
      call error('not a scalar parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine get_scalar_real64

  recursive subroutine get_scalar_logical(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    logical, intent(out) :: value
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    logical, intent(in), optional :: default

    logical :: errc
    type(any_scalar), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_scalar(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value, errc)
      if (errc) call error('not a logical parameter: "' // name // '"', stat, errmsg)
    else if (this%is_parameter(name)) then
      call error('not a scalar parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine get_scalar_logical

  recursive subroutine get_scalar_string(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    character(:), allocatable, intent(out) :: value
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    character(*), intent(in), optional :: default

    logical :: errc
    type(any_scalar), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_scalar(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value, errc)
      if (errc) call error('not a string parameter: "' // name // '"', stat, errmsg)
    else if (this%is_parameter(name)) then
      call error('not a scalar parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine get_scalar_string

  !! The follow subroutines get the rank-1 array value of the named parameter
  !! for various specific types.  If the parameter doesn't exist, it is created
  !! with the specified default value, if specified; otherwise it is an error.
  !! It is an error if the parameter value type doesn't match the type of the
  !! value argument.

  recursive subroutine get_vector_int32(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    integer(int32), allocatable, intent(out) :: value(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    integer(int32), intent(in), optional :: default(:)

    logical :: errc
    type(any_vector), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_vector(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value, errc)
      if (errc) call error('not an integer(int32) parameter: "' // name // '"', stat, errmsg)
    else if (this%is_parameter(name)) then
      call error('not a vector parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine get_vector_int32

  recursive subroutine get_vector_int64(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    integer(int64), allocatable, intent(out) :: value(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    integer(int64), intent(in), optional :: default(:)

    logical :: errc
    type(any_vector), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_vector(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value, errc)
      if (errc) call error('not an integer(int64) parameter: "' // name // '"', stat, errmsg)
    else if (this%is_parameter(name)) then
      call error('not a vector parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine get_vector_int64

  recursive subroutine get_vector_real32(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    real(real32), allocatable, intent(out) :: value(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    real(real32), intent(in), optional :: default(:)

    logical :: errc
    type(any_vector), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_vector(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value, errc)
      if (errc) call error('not an real(real32) parameter: "' // name // '"', stat, errmsg)
    else if (this%is_parameter(name)) then
      call error('not a vector parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine get_vector_real32

  recursive subroutine get_vector_real64(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    real(real64), allocatable, intent(out) :: value(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    real(real64), intent(in), optional :: default(:)

    logical :: errc
    type(any_vector), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_vector(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value, errc)
      if (errc) call error('not an real(real64) parameter: "' // name // '"', stat, errmsg)
    else if (this%is_parameter(name)) then
      call error('not a vector parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine get_vector_real64

  recursive subroutine get_vector_logical(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    logical, allocatable, intent(out) :: value(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    logical, intent(in), optional :: default(:)

    logical :: errc
    type(any_vector), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_vector(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value, errc)
      if (errc) call error('not a logical parameter: "' // name // '"', stat, errmsg)
    else if (this%is_parameter(name)) then
      call error('not a vector parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine get_vector_logical

  recursive subroutine get_vector_string(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    character(:), allocatable, intent(out) :: value(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    character(*), intent(in), optional :: default(:)

    logical :: errc
    type(any_vector), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_vector(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value, errc)
      if (errc) call error('not a string parameter: "' // name // '"', stat, errmsg)
    else if (this%is_parameter(name)) then
      call error('not a vector parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine get_vector_string

  !! The follow subroutines get the rank-2 array value of the named parameter
  !! for various specific types.  If the parameter doesn't exist, it is created
  !! with the specified default value, if specified; otherwise it is an error.
  !! It is an error if the parameter value type doesn't match the type of the
  !! value argument.

  recursive subroutine get_matrix_int32(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    integer(int32), allocatable, intent(out) :: value(:,:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    integer(int32), intent(in), optional :: default(:,:)

    logical :: errc
    type(any_matrix), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_matrix(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value, errc)
      if (errc) call error('not an integer(int32) parameter: "' // name // '"', stat, errmsg)
    else if (this%is_parameter(name)) then
      call error('not a matrix parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine get_matrix_int32

  recursive subroutine get_matrix_int64(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    integer(int64), allocatable, intent(out) :: value(:,:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    integer(int64), intent(in), optional :: default(:,:)

    logical :: errc
    type(any_matrix), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_matrix(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value, errc)
      if (errc) call error('not an integer(int64) parameter: "' // name // '"', stat, errmsg)
    else if (this%is_parameter(name)) then
      call error('not a matrix parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine get_matrix_int64

  recursive subroutine get_matrix_real32(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    real(real32), allocatable, intent(out) :: value(:,:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    real(real32), intent(in), optional :: default(:,:)

    logical :: errc
    type(any_matrix), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_matrix(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value, errc)
      if (errc) call error('not an real(real32) parameter: "' // name // '"', stat, errmsg)
    else if (this%is_parameter(name)) then
      call error('not a matrix parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine get_matrix_real32

  recursive subroutine get_matrix_real64(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    real(real64), allocatable, intent(out) :: value(:,:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    real(real64), intent(in), optional :: default(:,:)

    logical :: errc
    type(any_matrix), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_matrix(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value, errc)
      if (errc) call error('not an real(real64) parameter: "' // name // '"', stat, errmsg)
    else if (this%is_parameter(name)) then
      call error('not a matrix parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine get_matrix_real64

  recursive subroutine get_matrix_logical(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    logical, allocatable, intent(out) :: value(:,:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    logical, intent(in), optional :: default(:,:)

    logical :: errc
    type(any_matrix), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_matrix(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value, errc)
      if (errc) call error('not a logical parameter: "' // name // '"', stat, errmsg)
    else if (this%is_parameter(name)) then
      call error('not a matrix parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine get_matrix_logical

  recursive subroutine get_matrix_string(this, name, value, stat, errmsg, default)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    character(:), allocatable, intent(out) :: value(:,:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    character(*), intent(in), optional :: default(:,:)

    logical :: errc
    type(any_matrix), pointer :: pval

    call error_clear(stat, errmsg)
    pval => cast_to_any_matrix(find_pval(this, name))
    if (associated(pval)) then
      call pval%get_value(value, errc)
      if (errc) call error('not a string parameter: "' // name // '"', stat, errmsg)
    else if (this%is_parameter(name)) then
      call error('not a matrix parameter: "' // name // '"', stat, errmsg)
    else if (present(default)) then
      call this%set(name, default)
      call this%get(name, value)
    else
      call error('no such parameter: "' // name // '"', stat, errmsg)
    end if

  end subroutine get_matrix_string

  !!!! AUXILIARY CLASS-CASTING FUNCTIONS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! These cast a CLASS(PARAMETER_VALUE) pointer to a pointer to one of the
  !! extended types provided the dynamic type of the pointer is of that class;
  !! otherwise a null pointer is returned.

  function cast_to_parameter_list(pval) result(cast)
    class(parameter_value), pointer, intent(in) :: pval
    class(parameter_list), pointer :: cast
    cast => null()
    if (associated(pval)) then
      select type (pval)
      class is (parameter_list)
        cast => pval
      end select
    end if
  end function

  function cast_to_any_scalar(pval) result(cast)
    class(parameter_value), pointer, intent(in) :: pval
    class(any_scalar), pointer :: cast
    cast => null()
    if (associated(pval)) then
      select type (pval)
      class is (any_scalar)
        cast => pval
      end select
    end if
  end function

  function cast_to_any_vector(pval) result(cast)
    class(parameter_value), pointer, intent(in) :: pval
    class(any_vector), pointer :: cast
    cast => null()
    if (associated(pval)) then
      select type (pval)
      class is (any_vector)
        cast => pval
      end select
    end if
  end function

  function cast_to_any_matrix(pval) result(cast)
    class(parameter_value), pointer, intent(in) :: pval
    class(any_matrix), pointer :: cast
    cast => null()
    if (associated(pval)) then
      select type (pval)
      class is (any_matrix)
        cast => pval
      end select
    end if
  end function

  !!!! AUXILLARY LIST_ITEM PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! This creates a copy of the forward linked-list structure rooted with the
  !! given LIST_ITEM object. A pointer to the copy root is returned by the
  !! function. The PREV pointers in the copy are properly defined, including
  !! that of the root, which points to the end of the list, and thus is
  !! suitable as the target of PARAMETER_LIST%FIRST.

  recursive function list_item_copy(item) result(copy)
    type(list_item), intent(in) :: item
    type(list_item), pointer :: copy
    allocate(copy)
    copy%name = item%name
    allocate(copy%value, mold=item%value) ! NB: only intrinsic assignment with source=
#ifdef NAG_BUG20231206
    call copy%value%copy(item%value)
#else
    copy%value = item%value  ! defined assignment (requires rhs be allocated!)
#endif
    if (associated(item%next)) then
      copy%next => list_item_copy(item%next)
      copy%prev => copy%next%prev
      copy%next%prev => copy
    else
      copy%prev => copy
    end if
  end function

  !! Return a pointer to a new initialized (but unlinked) LIST_ITEM.
  function new_list_item(name, value) result(item)
    character(*), intent(in) :: name
    class(parameter_value), intent(in) :: value
    type(list_item), pointer :: item
    allocate(item)
    item%name = name
    allocate(item%value, mold=value) ! NB: only intrinsic assignment with source=
#ifdef NAG_BUG20231206
    call item%value%copy(value)
#else
    item%value = value  ! defined assignment (requires rhs be allocated!)
#endif
    item%prev => item
    item%next => null()
  end function

  !! Blindly link the given LIST_ITEM (as made by NEW_LIST_ITEM) to the end
  !! of the list; it does not check that its name is unique (someone else must).
  subroutine append_list_item(this, item)
    class(parameter_list), intent(inout) :: this !FIXME: class or type?
    type(list_item), pointer :: item
    type(list_item), pointer :: tail
    if (associated(this%first)) then
      tail => this%first%prev
      tail%next => item
      item%prev => tail
      this%first%prev => item
    else
      item%next => null()
      item%prev => item
      this%first => item
    end if
  end subroutine

  !! Returns a pointer to the parameter value in the parameter list that
  !! has the given name, or a null pointer if it is none is found.
  function find_pval(this, name) result(pval)
    class(parameter_list), intent(in) :: this  !FIXME: class or type?
    character(*), intent(in) :: name
    class(parameter_value), pointer :: pval
    type(list_item), pointer :: item
    pval => null()
    item => this%first
    do while (associated(item))
      if (item%name == name) exit
      item => item%next
    end do
    if (associated(item)) pval => item%value
  end function

  !!!! AUXILIARY ERROR HANDLING PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine error_clear(stat, errmsg)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    if (present(stat)) stat = 0
  end subroutine

  subroutine error(errmsg_, stat, errmsg)
    use,intrinsic :: iso_fortran_env, only: error_unit
    character(*), intent(in) :: errmsg_
    integer, intent(out), optional :: stat
#ifdef GNU_PR93762
    character(:), allocatable :: errmsg
#else
    character(:), allocatable, intent(out), optional :: errmsg
#endif
    if (present(stat)) then
      stat = 1
#ifdef GNU_PR93762
      errmsg = errmsg_
#else
      if (present(errmsg)) errmsg = errmsg_
#endif
    else
      write(error_unit,'(a)') 'ERROR: ' // errmsg_
      stop 1
    end if
  end subroutine

!!!! PARAMETER_LIST_ITERATOR TYPE BOUND PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! Defined PARAMETER_LIST_ITERATOR constructor that is positioned
  !! at the initial parameter of the specified parameter list.
  function iter_begin(plist, sublists_only) result(iter)
    class(parameter_list), intent(in) :: plist  !TODO: class or type?
    logical, intent(in), optional :: sublists_only
    type(parameter_list_iterator) :: iter
    if (present(sublists_only)) iter%sublists_only = sublists_only
    iter%item => plist%first
    if (iter%sublists_only) then
      do while (associated(iter%item))
        if (associated(cast_to_parameter_list(iter%item%value))) exit
        iter%item => iter%item%next
      end do
    end if
  end function

  !! Advances the iterator to the next parameter.
  subroutine iter_next(this)
    class(parameter_list_iterator), intent(inout) :: this
    if (.not.associated(this%item)) return
    this%item => this%item%next
    if (this%sublists_only) then ! skip ahead to the next sublist
      do while (associated(this%item))
        if (associated(cast_to_parameter_list(this%item%value))) exit
        this%item => this%item%next
      end do
    end if
  end subroutine

  !! Returns true if the iterator has reached the end.
  logical function iter_at_end(this) result(at_end)
    class(parameter_list_iterator), intent(in) :: this
    at_end = .not.associated(this%item)
  end function

  !! Return the name of the current parameter.
  function iter_name(this) result(name)
    class(parameter_list_iterator), intent(in) :: this
    character(:), allocatable :: name
    name = this%item%name
  end function

  !! Returns a CLASS(PARAMETER_VALUE) pointer to the current parameter value.
  function iter_value(this) result(pval)
    class(parameter_list_iterator), intent(in) :: this
    class(parameter_value), pointer :: pval
    pval => this%item%value
  end function

  !! Returns true if the current parameter is a sublist.
  logical function iter_is_sublist(this) result(is_sublist)
    class(parameter_list_iterator), intent(in) :: this
    !is_sublist = associated(iter_sublist(this))
    is_sublist = associated(cast_to_parameter_list(iter_value(this)))
  end function

  !! Returns true if the current parameter has a scalar value.
  logical function iter_is_scalar(this) result(is_scalar)
    class(parameter_list_iterator), intent(in) :: this
    is_scalar = associated(cast_to_any_scalar(iter_value(this)))
  end function

  !! Returns true if the current parameter has a vector value.
  logical function iter_is_vector(this) result(is_vector)
    class(parameter_list_iterator), intent(in) :: this
    is_vector = associated(cast_to_any_vector(iter_value(this)))
  end function

  !! Returns true if the current parameter has a matrix value.
  logical function iter_is_matrix(this) result(is_matrix)
    class(parameter_list_iterator), intent(in) :: this
    is_matrix = associated(cast_to_any_matrix(iter_value(this)))
  end function

  !! If the current parameter has a scalar value, return a CLASS(*)
  !! pointer to it; otherwise return a null pointer.
  function iter_scalar(this) result(scalar)
    class(parameter_list_iterator), intent(in) :: this
    class(*), pointer :: scalar
    type(any_scalar), pointer :: pval
    scalar => null()
    pval => cast_to_any_scalar(iter_value(this))
    if (associated(pval)) scalar => pval%value_ptr()
  end function

  !! If the current parameter has a vector value, return a CLASS(*)
  !! rank-1 array pointer to it; otherwise return a null pointer.
  function iter_vector(this) result(vector)
    class(parameter_list_iterator), intent(in) :: this
    class(*), pointer :: vector(:)
    type(any_vector), pointer :: pval
    vector => null()
    pval => cast_to_any_vector(iter_value(this))
    if (associated(pval)) vector => pval%value_ptr()
  end function

  !! If the current parameter has a matrix value, return a CLASS(*)
  !! rank-2 array pointer to it; otherwise return a null pointer.
  function iter_matrix(this) result(matrix)
    class(parameter_list_iterator), intent(in) :: this
    class(*), pointer :: matrix(:,:)
    type(any_matrix), pointer :: pval
    matrix => null()
    pval => cast_to_any_matrix(iter_value(this))
    if (associated(pval)) matrix => pval%value_ptr()
  end function

  !! If the current parameter is a sublist, return a pointer to it;
  !! otherwise return a null pointer.
  function iter_sublist(this) result(sublist)
    class(parameter_list_iterator), intent(in) :: this
    type(parameter_list), pointer :: sublist
    sublist => cast_to_parameter_list(iter_value(this))
  end function

  !! Returns the number of remaining parameters, including the current one.
  integer function iter_count(this) result(n)
    class(parameter_list_iterator), intent(in) :: this
    class(parameter_list_iterator), allocatable :: itemp
    n = 0
    itemp = this
    do while (.not.itemp%at_end())
      n = n + 1
      call itemp%next
    end do
  end function

end module parameter_list_type
