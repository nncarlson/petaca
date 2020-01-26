!!
!! PARAMETER_LIST_TYPE
!!
!!  Neil N. Carlson <neil.n.carlson@gmail.com>
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
!! PARAMETER_LIST TYPE BOUND PROCEDURES
!!
!!  SET(NAME, VALUE [,STAT [,ERRMSG]]) defines a parameter with the given NAME
!!    and assigns it the given VALUE, which may be a scalar, or array of rank 1
!!    or 2 of any intrinsic or derived type. If the parameter already exists
!!    and has a value with the same rank as VALUE, its value is replaced with
!!    the given one; the type of the values need not be the same.  Otherwise it
!!    is an error.  Note that a shallow copy of the passed value, as created by
!!    sourced-allocation, is stored in the parameter list.  This differs from
!!    a deep copy for derived-type values with (direct or indirect) pointer
!!    components.
!!
!!  GET(NAME, VALUE [,DEFAULT] [,STAT [,ERRMSG]]) gets the value of the named
!!    parameter.  A copy of the value is returned in the argument VALUE, which
!!    may be a scalar or array of rank 1 or 2 of the following specific types:
!!    integer (int32, int64 kinds), real (real32, real64 kinds), default logical
!!    and character. An array argument must be allocatable and a character
!!    argument must be deferred-length allocatable.  VALUE is allocated in these
!!    latter cases with the proper size/length to hold the parameter value. If
!!    present, the optional argument DEFAULT must have the same type, kind and
!!    rank as VALUE.  If the named parameter does not exist, it is created with
!!    the value prescribed by DEFAULT, and that value is return in VALUE. It is
!!    an error if the named parameter does not exist and DEFAULT is not present.
!!    It is an error if the type, kind and rank of VALUE does not match the
!!    stored value of the named parameter.
!!
!!  GET_ANY(NAME, VALUE [,DEFAULT] [,STAT [,ERRMSG]]) gets the value of the
!!    named parameter.  A copy of the value is returned in VALUE, which is an
!!    allocatable CLASS(*) variable or array of rank 1 or 2.  This is a more
!!    general version of GET in that any type of parameter value can be
!!    returned.  The downside is that the application code must use a select-
!!    type construct in order to use the returned value.  It is an error if
!!    the named parameter does not exist and DEFAULT is not present.  It is
!!    an error if the named parameter is a sublist.  It is an error if the
!!    rank of the VALUE does not match the rank of the stored value.
!!
!!  SUBLIST(NAME [,STAT [,ERRMSG]]) returns a TYPE(PARAMETER_LIST) pointer to
!!    the named parameter sublist. A sublist parameter is created with an empty
!!    parameter list value if the sublist does not already exist.  It is an
!!    error if the parameter exists but is not a sublist.
!!
!!  IS_PARAMETER(NAME) returns true if there is a parameter with given name.
!!
!!  IS_SUBLIST(NAME) returns true if there is a parameter with the given name
!!    and it is a sublist.
!!
!!  IS_SCALAR(NAME) returns true if there is a parameter with the given name
!!    and it is a scalar.
!!
!!  IS_VECTOR(NAME) returns true if there is a parameter with the given name
!!    and it is a rank-1 array.
!!
!!  IS_MATRIX(NAME) returns true if there is a parameter with the given name
!!    and it is a rank-2 array.
!!
!!  COUNT() returns the number of parameters stored in the parameter list.
!!
!!  NAME() returns the name of the parameter list.  If no name was assigned to
!!    the parameter list, the name 'ANONYMOUS' is returned.  Be careful not to
!!    confuse the name of a sublist parameter with the name of the parameter
!!    list that is its value; they are not the same thing.
!!
!!  SET_NAME(NAME) sets the name of the parameter list to NAME.  A parameter
!!    list created by SUBLIST is automatically assigned a default name.  It is
!!    the name of the parent parameter list appended with '->' followed by the
!!    sublist parameter name.  Use this function to override the default name. 
!!
!! PARAMETER_LIST_ITERATOR TYPE BOUND PROCEDURES
!!
!!  PARAMETER_LIST_ITERATOR(PLIST [,SUBLISTS_ONLY]) is a constructor that
!!    evaluates to an iterator that is positioned at the initial parameter of
!!    the parameter list PLIST, or the end if no parameters exist.  If the
!!    optional argument SUBLISTS_ONLY is present with value true, parameters
!!    other than sublists are skipped by the iterator.
!!
!!  NEXT() advances the iterator to the next parameter, or to the end if there
!!    are no more parameters remaining to be visited.  This call has no effect
!!    if the iterator is already positioned at the end.
!!
!!  AT_END() returns true if the iterator is positioned at the end.
!!
!!  NAME() returns the name of the current parameter.  The iterator must not be
!!    positioned at the end.
!!
!!  ENTRY() returns a CLASS(PARAMETER_ENTRY) pointer to the value of the
!!    current parameter.  The iterator must not be positioned at the end. The
!!    pointer has one of the following dynamic types: ANY_SCALAR, ANY_VECTOR,
!!    and PARAMETER_LIST.
!!
!!  IS_LIST() returns true if the current parameter is a sublist.
!!
!!  IS_SCALAR() returns true if the current parameter has a scalar value.
!!
!!  IS_VECTOR() returns true if the current parameter has a rank-1 array value.
!!
!!  IS_MATRIX() returns true if the current parameter has a rank-2 array value.
!!
!!  SUBLIST() returns a TYPE(PARAMETER_LIST) pointer to the current parameter
!!    sublist, if it is indeed a sublist; otherwise it returns a null pointer.
!!
!!  SCALAR() returns a CLASS(*) pointer to the value of the current parameter
!!    provided it is a scalar; otherwise it returns a null pointer.
!!
!!  VECTOR() returns a rank-1 CLASS(*) array pointer to the value of the
!!    current parameter provided it is a vector; otherwise it returns a null
!!    pointer.
!!
!!  MATRIX() returns a rank-2 CLASS(*) array pointer to the value of the
!!    current parameter provided it is a matrix; otherwise it returns a null
!!    pointer.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! NOTES
!!
!! 1) The passed-object argument of the VALUE_PTR method of the ANY_SCALAR,
!!    ANY_VECTOR, and ANY_MATRIX types is declared with the target attribute
!!    (the function returns a pointer to a non-pointer component of the object).
!!    This means that the object, in the caller, must also have the target
!!    attribute.  In the untweaked procedures that reference this note, the
!!    object is an associate name in a class-is block of a select-type
!!    construct.  That associate name should have the target attribute (the
!!    selector does) but recent changes to the NAG compiler have broken that
!!    (edit 942 and earlier work but 962 does not).  In this case this result
!!    is silent run time errors further up the calling chain -- pointers
!!    silently lose their targets.
!!

#include "f90_assert.fpp"

module parameter_list_type

  use,intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
  use parameter_entry_class
  use map_any_type
  implicit none
  private

  public :: parameter_entry, any_scalar, any_vector, any_matrix

  type, extends(parameter_entry), public :: parameter_list
    private
    character(:), allocatable :: name_
    type(map_any) :: params = map_any()
  contains
    procedure :: name
    procedure :: set_name
    procedure :: is_parameter
    procedure :: is_sublist
    procedure :: is_scalar
    procedure :: is_vector
    procedure :: is_matrix
    procedure :: count
    procedure :: sublist
    generic :: set => set_scalar, set_vector, set_matrix
    procedure, private :: set_scalar
    procedure, private :: set_vector
    procedure, private :: set_matrix
    generic :: get_any => get_any_scalar, get_any_vector, get_any_matrix
    procedure, private :: get_any_scalar
    procedure, private :: get_any_vector
    procedure, private :: get_any_matrix
    generic :: get => get_scalar_logical, get_scalar_string, &
               get_scalar_int32, get_scalar_int64, get_scalar_real32, get_scalar_real64, &
               get_vector_logical, get_vector_string, &
               get_vector_int32, get_vector_int64, get_vector_real32, get_vector_real64, &
               get_matrix_logical, get_matrix_string, &
               get_matrix_int32, get_matrix_int64, get_matrix_real32, get_matrix_real64
    procedure, private :: get_scalar_int32
    procedure, private :: get_scalar_int64
    procedure, private :: get_scalar_real32
    procedure, private :: get_scalar_real64
    procedure, private :: get_scalar_logical
    procedure, private :: get_scalar_string
    procedure, private :: get_vector_int32
    procedure, private :: get_vector_int64
    procedure, private :: get_vector_real32
    procedure, private :: get_vector_real64
    procedure, private :: get_vector_logical
    procedure, private :: get_vector_string
    procedure, private :: get_matrix_int32
    procedure, private :: get_matrix_int64
    procedure, private :: get_matrix_real32
    procedure, private :: get_matrix_real64
    procedure, private :: get_matrix_logical
    procedure, private :: get_matrix_string
  end type

#ifdef INTEL_BUG20140921
  interface parameter_list
    procedure parameter_list_intel
  end interface
#endif

  type, public :: parameter_list_iterator
    private
    type(map_any_iterator) :: mapit
    logical :: sublists_only = .false.
  contains
    procedure :: next => iter_next
    procedure :: at_end => iter_at_end
    procedure :: name => iter_name
    procedure :: entry => iter_entry
    procedure :: is_list => iter_is_list
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

#ifdef INTEL_BUG20140921
  !! Has the same effect that the default constructor should but does not.
  function parameter_list_intel (name) result (plist)
    character(*), intent(in), optional :: name
    type(parameter_list) :: plist
    if (present(name)) plist%name_ = name
  end function parameter_list_intel
#endif

  !!
  !! AUXILLARY CLASS-CASTING FUNCTIONS
  !!
  !! A few routines for casting pointers of one class to pointers of a subclass.
  !! We need to do this repeatedly, and the code to do it is a little verbose;
  !! these routines encapsulate that process.  Some background:
  !!
  !! A MAP_ANY object is used to hold the parameter name/value pairs, and a
  !! map value is accessed via a CLASS(*) pointer.  We know, however, that
  !! the dynamic type of the value is of class PARAMENTER_ENTRY.
  !!
  !! A CLASS(PARAMETER_ENTRY) pointer has one of four possible dynamic types:
  !! PARAMETER_LIST, ANY_SCALAR, ANY_VECTOR, or ANY_MATRIX.
  !!
  !! For convenience each routine can be passed a null pointer; a null pointer
  !! is returned in that case.
  !!

  !! Casts the given CLASS(*) pointer to a CLASS(PARAMETER_ENTRY) pointer.
  function cast_to_parameter_entry (uptr) result (cast)
    class(*), pointer, intent(in) :: uptr
    class(parameter_entry), pointer :: cast
    cast => null()
    if (associated(uptr)) then
      select type (uptr)
      class is (parameter_entry)
        cast => uptr
      end select
      ASSERT(associated(cast))
    end if
  end function cast_to_parameter_entry

  !! Casts a CLASS(PARAMETER_ENTRY) pointer to a CLASS(PARAMETER_LIST) pointer
  !! provided the dynamic type of the pointer is of that class; otherwise a
  !! null pointer is returned.
  function cast_to_parameter_list (pentry) result (cast)
    class(parameter_entry), pointer, intent(in) :: pentry
    class(parameter_list), pointer :: cast
    cast => null()
    if (associated(pentry)) then
      select type (pentry)
      type is (parameter_list)
        cast => pentry
      end select
    end if
  end function cast_to_parameter_list

  !! Casts a CLASS(PARAMETER_ENTRY) pointer to a CLASS(ANY_SCALAR) pointer
  !! provided the dynamic type of the pointer is of that class; otherwise a
  !! null pointer is returned.
  function cast_to_any_scalar (pentry) result (cast)
    class(parameter_entry), pointer, intent(in) :: pentry
    class(any_scalar), pointer :: cast
    cast => null()
    if (associated(pentry)) then
      select type (pentry)
      type is (any_scalar)
        cast => pentry
      end select
    end if
  end function cast_to_any_scalar

  !! Casts a CLASS(PARAMETER_ENTRY) pointer to a CLASS(ANY_VECTOR) pointer
  !! provided the dynamic type of the pointer is of that class; otherwise a
  !! null pointer is returned.
  function cast_to_any_vector (pentry) result (cast)
    class(parameter_entry), pointer, intent(in) :: pentry
    class(any_vector), pointer :: cast
    cast => null()
    if (associated(pentry)) then
      select type (pentry)
      type is (any_vector)
        cast => pentry
      end select
    end if
  end function cast_to_any_vector

  !! Casts a CLASS(PARAMETER_ENTRY) pointer to a CLASS(ANY_MATRIX) pointer
  !! provided the dynamic type of the pointer is of that class; otherwise a
  !! null pointer is returned.
  function cast_to_any_matrix (pentry) result (cast)
    class(parameter_entry), pointer, intent(in) :: pentry
    class(any_matrix), pointer :: cast
    cast => null()
    if (associated(pentry)) then
      select type (pentry)
      type is (any_matrix)
        cast => pentry
      end select
    end if
  end function cast_to_any_matrix

  !!!! OTHER AUXILLARY ROUTINES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! Returns a pointer to the parameter entry in the parameter list that
  !! has the given name, or a null pointer if it is none is found.  This
  !! is the core look-up function; others are built on top of it.
  function find_entry (map, name) result (pentry)
    class(map_any), intent(in) :: map
    character(*), intent(in) :: name
    class(parameter_entry), pointer :: pentry
#ifdef NO_2008_PTR_FUN_RESULT_IS_VAR
    class(*), pointer :: uptr
    uptr => map%value(name)
    pentry => cast_to_parameter_entry(uptr)
#else
    pentry => cast_to_parameter_entry(map%value(name))
#endif
  end function find_entry

  !! Returns a CLASS(ANY_SCALAR) pointer to the named parameter value.
  !! Pointer is null if no such parameter exists or is of the wrong kind.
  function find_any_scalar_entry (map, name) result (entry)
    class(map_any), intent(in) :: map
    character(*), intent(in) :: name
    class(any_scalar), pointer :: entry
#ifdef NO_2008_PTR_FUN_RESULT_IS_VAR
    class(parameter_entry), pointer :: pentry
    pentry => find_entry(map, name)
    entry => cast_to_any_scalar(pentry)
#else
    entry => cast_to_any_scalar(find_entry(map, name))
#endif
  end function find_any_scalar_entry

  !! Returns a CLASS(ANY_VECTOR) pointer to the named parameter value.
  !! Pointer is null if no such parameter exists or is of the wrong kind.
  function find_any_vector_entry (map, name) result (entry)
    class(map_any), intent(in) :: map
    character(*), intent(in) :: name
    class(any_vector), pointer :: entry
#ifdef NO_2008_PTR_FUN_RESULT_IS_VAR
    class(parameter_entry), pointer :: pentry
    pentry => find_entry(map, name)
    entry => cast_to_any_vector(pentry)
#else
    entry => cast_to_any_vector(find_entry(map, name))
#endif
  end function find_any_vector_entry

  !! Returns a CLASS(ANY_MATRIX) pointer to the named parameter value.
  !! Pointer is null if no such parameter exists or is of the wrong kind.
  function find_any_matrix_entry (map, name) result (entry)
    class(map_any), intent(in) :: map
    character(*), intent(in) :: name
    class(any_matrix), pointer :: entry
#ifdef NO_2008_PTR_FUN_RESULT_IS_VAR
    class(parameter_entry), pointer :: pentry
    pentry => find_entry(map, name)
    entry => cast_to_any_matrix(pentry)
#else
    entry => cast_to_any_matrix(find_entry(map, name))
#endif
  end function find_any_matrix_entry

  !! Returns a CLASS(PARAMETER_LIST) pointer to the named parameter sublist.
  !! Pointer is null if no such parameter exists or is of the wrong kind.
  function find_parameter_list_entry (map, name) result (entry)
    class(map_any), intent(in) :: map
    character(*), intent(in) :: name
    class(parameter_list), pointer :: entry
    entry => cast_to_parameter_list(find_entry(map, name))
  end function find_parameter_list_entry

  !!!! AUXILLARY ERROR HANDLING PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine error_clear (stat, errmsg)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    if (present(stat)) stat = 0
  end subroutine error_clear

  subroutine error (errmsg_, stat, errmsg)
    use,intrinsic :: iso_fortran_env, only: error_unit
    character(*), intent(in) :: errmsg_
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    if (present(stat)) then
      stat = 1
      if (present(errmsg)) errmsg = errmsg_
    else
      write(error_unit,'(a)') 'ERROR: ' // errmsg_
      stop 1
    end if
  end subroutine error

!!!! PARAMETER_LIST TYPE BOUND PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! Returns the name of this parameter list.
  function name (this)
    class(parameter_list), intent(in) :: this
    character(:), allocatable :: name
    if (allocated(this%name_)) then
      name = this%name_
    else
      name = '$'
    end if
  end function name

  !! Sets the name of this parameter list.
  subroutine set_name (this, name)
    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    this%name_ = name
  end subroutine set_name

  !! Returns true if the named parameter exists.
  logical function is_parameter (this, name)
    class(parameter_list), intent(in) :: this
    character(*), intent(in) :: name
#ifdef NO_2008_PTR_FUN_RESULT_IS_VAR
    class(parameter_entry), pointer :: pentry
    pentry => find_entry(this%params, name)
    is_parameter = associated(pentry)
#else
    is_parameter = associated(find_entry(this%params, name))
#endif
  end function is_parameter

  !! Returns true if the named parameter exists and is a sublist.
  logical function is_sublist (this, name)
    class(parameter_list), intent(in) :: this
    character(*), intent(in) :: name
#ifdef NO_2008_PTR_FUN_RESULT_IS_VAR
    class(parameter_entry), pointer :: pentry
    pentry => find_entry(this%params, name)
    is_sublist = associated(cast_to_parameter_list(pentry))
#else
    is_sublist = associated(cast_to_parameter_list(find_entry(this%params, name)))
#endif
  end function is_sublist

  !! Returns true if the named parameter exists and is a scalar.
  logical function is_scalar (this, name)
    class(parameter_list), intent(in) :: this
    character(*), intent(in) :: name
    is_scalar = associated(find_any_scalar_entry(this%params, name))
  end function is_scalar

  !! Returns true if the named parameter exists and is a vector.
  logical function is_vector (this, name)
    class(parameter_list), intent(in) :: this
    character(*), intent(in) :: name
    is_vector = associated(find_any_vector_entry(this%params, name))
  end function is_vector

  !! Returns true if the named parameter exists and is a matrix.
  logical function is_matrix (this, name)
    class(parameter_list), intent(in) :: this
    character(*), intent(in) :: name
    is_matrix = associated(find_any_matrix_entry(this%params, name))
  end function is_matrix

  !! Returns the number of stored parameters.
  integer function count (this)
    class(parameter_list), intent(in) :: this
    count = this%params%size()
  end function count

  !! Returns a pointer to the named parameter sublist.  It is an error if the
  !! parameter exists and is not a sublist.  An empty sublist parameter is
  !! created if necessary.

  function sublist (this, name, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    class(parameter_list), pointer :: sublist
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (.not.associated(pentry)) then
      call this%params%insert (name, parameter_list(this%name()//'->'//name))
      !! IMPORTANT: pentry must point to the one in the map which is a copy!
      pentry => find_entry(this%params, name)
      ASSERT(associated(pentry))
    end if
    sublist => cast_to_parameter_list(pentry)
    if (.not.associated(sublist)) then
      call error ('parameter is not a sublist: "' // name // '"', stat, errmsg)
    end if

  end function sublist

  !! Sets the scalar value of the named parameter.  If the parameter exists,
  !! its value, which must be of type ANY_SCALAR, is overwritten with the
  !! given value.  If an error is encountered, STAT and ERRMSG return info.

  subroutine set_scalar (this, name, value, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    class(*), intent(in) :: value
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (parameter_list)
        call error ('parameter is a sublist: "' // name // '"', stat, errmsg)
      type is (any_scalar)
        call pentry%set_value (value)
      class default
        call error ('not a scalar parameter: "' // name // '"', stat, errmsg)
      end select
    else
      call this%params%insert(name, any_scalar(value))
    end if

  end subroutine set_scalar

  !! Sets the rank-1 array value of the named parameter.  If the parameter
  !! exists, its value, which must be of type ANY_VECTOR, is overwritten with
  !! the given value. If an error is encountered, STAT and ERRMSG return info.

  subroutine set_vector (this, name, value, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    class(*), intent(in) :: value(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (parameter_list)
        call error ('parameter is a sublist: "' // name // '"', stat, errmsg)
      type is (any_vector)
        call pentry%set_value (value)
      class default
        call error ('not a vector parameter: "' // name // '"', stat, errmsg)
      end select
    else
      call this%params%insert(name, any_vector(value))
    end if

  end subroutine set_vector

  !! Sets the rank-2 array value of the named parameter.  If the parameter
  !! exists, its value, which must be of type ANY_MATRIX, is overwritten with
  !! the given value. If an error is encountered, STAT and ERRMSG return info.

  subroutine set_matrix (this, name, value, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    class(*), intent(in) :: value(:,:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (parameter_list)
        call error ('parameter is a sublist: "' // name // '"', stat, errmsg)
      type is (any_matrix)
        call pentry%set_value (value)
      class default
        call error ('not a matrix parameter: "' // name // '"', stat, errmsg)
      end select
    else
      call this%params%insert(name, any_matrix(value))
    end if

  end subroutine set_matrix

  !! Returns the scalar value of the named parameter in the CLASS(*)
  !! allocatable variable VALUE.  If the named parameter does not exist
  !! and the optional argument DEFAULT is present, the parameter is
  !! created and assigned the value specified by DEFAULT, and that value
  !! returned by VALUE.  Otherwise it is an error.  It is an error if
  !! the parameter exists and is a sublist, or it has a non-scalar value.

  subroutine get_any_scalar (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    class(*), allocatable, intent(out) :: value
    class(*), intent(in), optional :: default
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    class(parameter_entry), pointer :: pentry
    class(*), pointer :: scalar

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_scalar)
        scalar => pentry%value_ptr()
        allocate(value,source=scalar)
      class default
        call error ('not a scalar parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_scalar (this, name, default)
        allocate(value,source=default)
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_any_scalar

  !! Returns the vector value of the named parameter in the CLASS(*)
  !! allocatable array VALUE.  If the named parameter does not exist
  !! and the optional array DEFAULT is present, the parameter is
  !! created and assigned the value specified by DEFAULT, and that value
  !! returned by VALUE.  Otherwise it is an error.  It is an error if
  !! the parameter exists and is a sublist, or it has a non-vector value.

  subroutine get_any_vector (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    class(*), allocatable, intent(out) :: value(:)
    class(*), intent(in), optional :: default(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    class(parameter_entry), pointer :: pentry
    class(*), pointer :: vector(:)

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_vector)
        vector => pentry%value_ptr()
#ifdef NO_2008_SOURCED_ALLOC_ARRAY
        allocate(value(lbound(vector,1):ubound(vector,1)),source=vector)
#else
        allocate(value,source=vector)
#endif
      class default
        call error ('not a vector parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_vector (this, name, default)
#ifdef NO_2008_SOURCED_ALLOC_ARRAY
        allocate(value(lbound(default,1):ubound(default,1)),source=default)
#else
        allocate(value,source=default)
#endif
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_any_vector

  !! Returns the matrix value of the named parameter in the CLASS(*)
  !! allocatable array VALUE.  If the named parameter does not exist
  !! and the optional array DEFAULT is present, the parameter is
  !! created and assigned the value specified by DEFAULT, and that value
  !! returned by VALUE.  Otherwise it is an error.  It is an error if
  !! the parameter exists and is a sublist, or it has a non-matrix value.

  subroutine get_any_matrix (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    class(*), allocatable, intent(out) :: value(:,:)
    class(*), intent(in), optional :: default(:,:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    class(parameter_entry), pointer :: pentry
    class(*), pointer :: matrix(:,:)

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_matrix)
        matrix => pentry%value_ptr()
#ifdef NO_2008_SOURCED_ALLOC_ARRAY
        allocate(value(lbound(matrix,1):ubound(matrix,1), &
                       lbound(matrix,2):ubound(matrix,2)),source=matrix)
#else
        allocate(value,source=matrix)
#endif
      class default
        call error ('not a matrix parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_matrix (this, name, default)
#ifdef NO_2008_SOURCED_ALLOC_ARRAY
        allocate(value(lbound(default,1):ubound(default,1), &
                       lbound(default,1):ubound(default,2)),source=default)
#else
        allocate(value,source=default)
#endif
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_any_matrix

  !! The follow subroutines get the scalar value of the named parameter for
  !! various specific types.  If the parameter doesn't exist, it is created
  !! with the specified default value, if specified; otherwise it is an error.
  !! It is an error if the parameter value type doesn't match the type of the
  !! value argument.

  subroutine get_scalar_int32 (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    integer(int32), intent(out) :: value
    integer(int32), intent(in), optional :: default
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    logical :: errc
    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_scalar)
        call pentry%get_value (value, errc)
        if (errc) call error ('not an integer(int32) parameter: "' // name // '"', stat, errmsg)
      class default
        call error ('not a scalar parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_scalar (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_scalar_int32

  subroutine get_scalar_int64 (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    integer(int64), intent(out) :: value
    integer(int64), intent(in), optional :: default
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    logical :: errc
    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_scalar)
        call pentry%get_value (value, errc)
        if (errc) call error ('not an integer(int64) parameter: "' // name // '"', stat, errmsg)
      class default
        call error ('not a scalar parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_scalar (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_scalar_int64

  subroutine get_scalar_real32 (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    real(real32), intent(out) :: value
    real(real32), intent(in), optional :: default
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    logical :: errc
    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_scalar)
        call pentry%get_value (value, errc)
        if (errc) call error ('not a real(real32) parameter: "' // name // '"', stat, errmsg)
      class default
        call error ('not a scalar parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_scalar (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_scalar_real32

  subroutine get_scalar_real64 (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    real(real64), intent(out) :: value
    real(real64), intent(in), optional :: default
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    logical :: errc
    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_scalar)
        call pentry%get_value (value, errc)
        if (errc) call error ('not a real(real64) parameter: "' // name // '"', stat, errmsg)
      class default
        call error ('not a scalar parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_scalar (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_scalar_real64

  subroutine get_scalar_logical (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    logical, intent(out) :: value
    logical, intent(in), optional :: default
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    logical :: errc
    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_scalar)
        call pentry%get_value (value, errc)
        if (errc) call error ('not a logical parameter: "' // name // '"', stat, errmsg)
      class default
        call error ('not a scalar parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_scalar (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_scalar_logical

  subroutine get_scalar_string (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    character(:), allocatable, intent(out) :: value
    character(*), intent(in), optional :: default
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    logical :: errc
    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_scalar)
        call pentry%get_value (value, errc)
        if (errc) call error ('not a string parameter: "' // name // '"', stat, errmsg)
      class default
        call error ('not a scalar parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_scalar (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_scalar_string

  !! The follow subroutines get the rank-1 array value of the named parameter
  !! for various specific types.  If the parameter doesn't exist, it is created
  !! with the specified default value, if specified; otherwise it is an error.
  !! It is an error if the parameter value type doesn't match the type of the
  !! value argument.

  subroutine get_vector_int32 (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    integer(int32), allocatable, intent(out) :: value(:)
    integer(int32), intent(in), optional :: default(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    logical :: errc
    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_vector)
        call pentry%get_value(value, errc)
        if (errc) call error ('not an integer(int32) parameter: "' // name // '"', stat, errmsg)
      class default
        call error ('not a vector parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_vector (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_vector_int32

  subroutine get_vector_int64 (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    integer(int64), allocatable, intent(out) :: value(:)
    integer(int64), intent(in), optional :: default(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    logical :: errc
    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_vector)
        call pentry%get_value(value, errc)
        if (errc) call error ('not an integer(int64) parameter: "' // name // '"', stat, errmsg)
      class default
        call error ('not a vector parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_vector (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_vector_int64

  subroutine get_vector_real32 (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    real(real32), allocatable, intent(out) :: value(:)
    real(real32), intent(in), optional :: default(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    logical :: errc
    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_vector)
        call pentry%get_value(value, errc)
        if (errc) call error ('not an real(real64) parameter: "' // name // '"', stat, errmsg)
      class default
        call error ('not a vector parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_vector (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_vector_real32

  subroutine get_vector_real64 (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    real(real64), allocatable, intent(out) :: value(:)
    real(real64), intent(in), optional :: default(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    logical :: errc
    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_vector)
        call pentry%get_value(value, errc)
        if (errc) call error ('not an real(real64) parameter: "' // name // '"', stat, errmsg)
      class default
        call error ('not a vector parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_vector (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_vector_real64

  subroutine get_vector_logical (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    logical, allocatable, intent(out) :: value(:)
    logical, intent(in), optional :: default(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    logical :: errc
    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_vector)
        call pentry%get_value (value, errc)
        if (errc) call error ('not a logical parameter: "' // name // '"', stat, errmsg)
      class default
        call error ('not a vector parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_vector (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_vector_logical

  subroutine get_vector_string (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    character(:), allocatable, intent(out) :: value(:)
    character(*), intent(in), optional :: default(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    logical :: errc
    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_vector)
        call pentry%get_value (value, errc)
        if (errc) call error ('not a string parameter: "' // name // '"', stat, errmsg)
      class default
        call error ('not a vector parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_vector (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_vector_string

  !! The follow subroutines get the rank-2 array value of the named parameter
  !! for various specific types.  If the parameter doesn't exist, it is created
  !! with the specified default value, if specified; otherwise it is an error.
  !! It is an error if the parameter value type doesn't match the type of the
  !! value argument.

  subroutine get_matrix_int32 (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    integer(int32), allocatable, intent(out) :: value(:,:)
    integer(int32), intent(in), optional :: default(:,:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    logical :: errc
    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_matrix)
        call pentry%get_value(value, errc)
        if (errc) call error ('not an integer(int32) parameter: "' // name // '"', stat, errmsg)
      class default
        call error ('not a matrix parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_matrix (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_matrix_int32

  subroutine get_matrix_int64 (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    integer(int64), allocatable, intent(out) :: value(:,:)
    integer(int64), intent(in), optional :: default(:,:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    logical :: errc
    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_matrix)
        call pentry%get_value(value, errc)
        if (errc) call error ('not an integer(int64) parameter: "' // name // '"', stat, errmsg)
      class default
        call error ('not a matrix parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_matrix (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_matrix_int64

  subroutine get_matrix_real32 (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    real(real32), allocatable, intent(out) :: value(:,:)
    real(real32), intent(in), optional :: default(:,:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    logical :: errc
    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_matrix)
        call pentry%get_value(value, errc)
        if (errc) call error ('not an real(real64) parameter: "' // name // '"', stat, errmsg)
      class default
        call error ('not a matrix parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_matrix (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_matrix_real32

  subroutine get_matrix_real64 (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    real(real64), allocatable, intent(out) :: value(:,:)
    real(real64), intent(in), optional :: default(:,:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    logical :: errc
    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_matrix)
        call pentry%get_value(value, errc)
        if (errc) call error ('not an real(real64) parameter: "' // name // '"', stat, errmsg)
      class default
        call error ('not a matrix parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_matrix (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_matrix_real64

  subroutine get_matrix_logical (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    logical, allocatable, intent(out) :: value(:,:)
    logical, intent(in), optional :: default(:,:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    logical :: errc
    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_matrix)
        call pentry%get_value (value, errc)
        if (errc) call error ('not a logical parameter: "' // name // '"', stat, errmsg)
      class default
        call error ('not a matrix parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_matrix (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_matrix_logical

  subroutine get_matrix_string (this, name, value, default, stat, errmsg)

    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    character(:), allocatable, intent(out) :: value(:,:)
    character(*), intent(in), optional :: default(:,:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg

    logical :: errc
    class(parameter_entry), pointer :: pentry

    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_matrix)
        call pentry%get_value (value, errc)
        if (errc) call error ('not a string parameter: "' // name // '"', stat, errmsg)
      class default
        call error ('not a matrix parameter: "' // name // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_matrix (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // name // '"', stat, errmsg)
      end if
    end if

  end subroutine get_matrix_string

!!!! PARAMETER_LIST_ITERATOR TYPE BOUND PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! Defined PARAMETER_LIST_ITERATOR constructor that is positioned
  !! at the initial parameter of the specified parameter list.
  function iter_begin (plist, sublists_only) result (iter)
    class(parameter_list), intent(in) :: plist
    logical, intent(in), optional :: sublists_only
    type(parameter_list_iterator) :: iter
    if (present(sublists_only)) iter%sublists_only = sublists_only
    iter%mapit = map_any_iterator(plist%params)
    if (iter%sublists_only) then
      do while (.not.iter%mapit%at_end())
        select type (uptr => iter%mapit%value())
        class is (parameter_list)
          exit
        end select
        call iter%mapit%next
      end do
    end if
  end function iter_begin

  !! Advances the iterator to the next parameter.
  subroutine iter_next (this)
    class(parameter_list_iterator), intent(inout) :: this
    call this%mapit%next
    if (this%sublists_only) then
      do while (.not.this%mapit%at_end())
        select type (uptr => this%mapit%value())
        class is (parameter_list)
          exit
        end select
        call this%mapit%next
      end do
    end if
  end subroutine iter_next

  !! Returns true if the iterator has reached the end.
  logical function iter_at_end (this) result (at_end)
    class(parameter_list_iterator), intent(in) :: this
    at_end = this%mapit%at_end()
  end function iter_at_end

  !! Returns the name of the current parameter.
  function iter_name (this)
    class(parameter_list_iterator), intent(in) :: this
    character(:), allocatable :: iter_name
    iter_name = this%mapit%key()
  end function iter_name

  !! Returns a CLASS(PARAMETER_ENTRY) pointer to the current parameter value.
  function iter_entry (this)
    class(parameter_list_iterator), intent(in) :: this
    class(parameter_entry), pointer :: iter_entry
#ifdef NO_2008_PTR_FUN_RESULT_IS_VAR
    class(*), pointer :: uptr
    uptr => this%mapit%value()
    iter_entry => cast_to_parameter_entry(uptr)
#else
    iter_entry => cast_to_parameter_entry(this%mapit%value())
#endif
  end function iter_entry

  !! Returns true if the current parameter is a sublist.
  logical function iter_is_list (this) result (is_list)
    class(parameter_list_iterator), intent(in) :: this
    is_list = associated(iter_sublist(this))
  end function iter_is_list

  !! Returns true if the current parameter has a scalar value.
  logical function iter_is_scalar (this) result (is_scalar)
    class(parameter_list_iterator), intent(in) :: this
#ifdef NO_2008_PTR_FUN_RESULT_IS_VAR
    class(parameter_entry), pointer :: pentry
    pentry => iter_entry(this)
    is_scalar = associated(cast_to_any_scalar(pentry))
#else
    is_scalar = associated(cast_to_any_scalar(iter_entry(this)))
#endif
  end function iter_is_scalar

  !! Returns true if the current parameter has a vector value.
  logical function iter_is_vector (this) result (is_vector)
    class(parameter_list_iterator), intent(in) :: this
#ifdef NO_2008_PTR_FUN_RESULT_IS_VAR
    class(parameter_entry), pointer :: pentry
    pentry => iter_entry(this)
    is_vector = associated(cast_to_any_vector(pentry))
#else
    is_vector = associated(cast_to_any_vector(iter_entry(this)))
#endif
  end function iter_is_vector

  !! Returns true if the current parameter has a matrix value.
  logical function iter_is_matrix (this) result (is_matrix)
    class(parameter_list_iterator), intent(in) :: this
#ifdef NO_2008_PTR_FUN_RESULT_IS_VAR
    class(parameter_entry), pointer :: pentry
    pentry => iter_entry(this)
    is_matrix = associated(cast_to_any_matrix(pentry))
#else
    is_matrix = associated(cast_to_any_matrix(iter_entry(this)))
#endif
  end function iter_is_matrix

  !! If the current parameter has a scalar value, return a CLASS(*)
  !! pointer to it; otherwise return a null pointer.
  !! N.B. See Note 1 regarding the NAG workaround.
  function iter_scalar (this) result (scalar)
    class(parameter_list_iterator), intent(in) :: this
    class(*), pointer :: scalar
    class(*), pointer :: uptr
    uptr => this%mapit%value()
    select type (uptr)
    !select type (uptr => this%mapit%value()) Requires F2008, R602/C602
    class is (any_scalar)
      scalar => uptr%value_ptr()
    class default
      scalar => null()
    end select
  end function iter_scalar

  !! If the current parameter has a vector value, return a CLASS(*)
  !! rank-1 array pointer to it; otherwise return a null pointer.
  !! N.B. See Note 1 regarding the NAG workaround.
  function iter_vector (this) result (vector)
    class(parameter_list_iterator), intent(in) :: this
    class(*), pointer :: vector(:)
    class(*), pointer :: uptr
    uptr => this%mapit%value()
    select type (uptr)
    !select type (uptr => this%mapit%value()) Requires F2008, R602/C602
    class is (any_vector)
      vector => uptr%value_ptr()
    class default
      vector => null()
    end select
  end function iter_vector

  !! If the current parameter has a matrix value, return a CLASS(*)
  !! rank-1 array pointer to it; otherwise return a null pointer.
  !! N.B. See Note 1 regarding the NAG workaround.
  function iter_matrix (this) result (matrix)
    class(parameter_list_iterator), intent(in) :: this
    class(*), pointer :: matrix(:,:)
    class(*), pointer :: uptr
    uptr => this%mapit%value()
    select type (uptr)
    !select type (uptr => this%mapit%value()) Requires F2008, R602/C602
    class is (any_matrix)
      matrix => uptr%value_ptr()
    class default
      matrix => null()
    end select
  end function iter_matrix

  !! If the current parameter is a sublist, return a pointer to it;
  !! otherwise return a null pointer.
  function iter_sublist (this) result (sublist)
    class(parameter_list_iterator), intent(in) :: this
    class(parameter_list), pointer :: sublist
    class(*), pointer :: uptr
    uptr => this%mapit%value()
    select type (uptr)
    !select type (uptr => this%mapit%value()) Requires F2008, R602/C602
    class is (parameter_list)
      sublist => uptr
    class default
      sublist => null()
    end select
  end function iter_sublist

  !! Returns the number of remaining parameters, including the current one.
  integer function iter_count (this) result (n)
    class(parameter_list_iterator), intent(in) :: this
    class(map_any_iterator), allocatable :: itemp
    n = 0
    allocate(itemp, source=this%mapit)
    do while (.not.itemp%at_end())
      n = n + 1
      call itemp%next
    end do
  end function iter_count

end module parameter_list_type
