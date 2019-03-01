!!
!! PARAMETER_ENTRY_CLASS
!!
!!  This module defines the abstract base type PARAMETER_ENTRY. Values of this
!!  class are held as the values of a parameter list.  The module also defines
!!  two extensions of this type: ANY_SCALAR and ANY_VECTOR which hold scalar
!!  and rank-1 array values of any type, respectively. The PARAMETER_LIST type
!!  is itself an extension of this base type.
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
!!  The abstract base type PARAMETER_ENTRY is defined.  Values of this class
!!  are held as the parameter values in a parameter list.  The PARAMETER_LIST
!!  type is one extension of this base type, but additional extensions are
!!  defined here: ANY_SCALAR, ANY_VECTOR, ANY_MATRIX hold scalar and rank-1
!!  and rank-2 array values of any type, respectively, in a CLASS(*)
!!  allocatable component.
!!
!!  Sourced-allocation is used internally to copy values when storing or
!!  retrieving values.  This means that all copies are shallow.  This is only
!!  an issue for values of derived types that have pointer components, either
!!  directly or indirectly.  In all other cases the copies that are made are
!!  genuine copies.  Similarly, intrinsic assignment of these extended types
!!  also works as expected, except for values of derived types that have
!!  pointer components.
!!
!!  ANY_SCALAR TYPE BOUND PROCEDURES
!!
!!    SET_VALUE(VALUE) sets the value stored by the object to a (shallow) copy
!!      of the scalar argument VALUE, which may be of any intrinsic or derived
!!      type.
!!
!!    GET_VALUE(VALUE) gets the value stored by the object.  The argument VALUE
!!      is a CLASS(*) allocatable variable that returns a (shallow) copy of the
!!      stored value.
!!
!!    GET_VALUE(VALUE, ERRC) gets a (shallow) copy of the value stored by the
!!      object.  This is a generic subroutine that depends on the type of the
!!      scalar argument VALUE: it may be integer (INT32, INT64 kinds), real
!!      (REAL32, REAL64 kinds), default logical, or deferred-length allocatable
!!      character.  The kind parameters are those from the intrinsic module
!!      ISO_FORTRAN_ENV, and typically include default integer and real, and
!!      double precision kinds.  If the type and kind of VALUE does not match
!!      the stored value, the logical argument ERRC returns true; otherwise it
!!      returns false.
!!
!!    VALUE_PTR() returns a CLASS(*) pointer to the value stored by the object.
!!      The object should have the TARGET attribute.
!!
!!    WRITE(UNIT) writes the value stored by the object to the given logical
!!      unit, which must be opened for formatted write access.  No newlines are
!!      are written.
!!
!!  ANY_VECTOR TYPE BOUND PROCEDURES
!!
!!    SET_VALUE(VALUE) sets the value stored by the object to a (shallow) copy
!!      of the rank-1 array argument VALUE, which may be of any intrinsic or
!!      derived type.
!!
!!    GET_VALUE(VALUE) gets the value stored by the object. The argument VALUE
!!      is a CLASS(*) allocatable rank-1 array that returns a (shallow) copy
!!      of the stored value.
!!
!!    GET_VALUE(VALUE, ERRC) gets a (shallow) copy of the value stored by the
!!      object.  This is a generic subroutine depending on the type of the
!!      rank-1 allocatable array argument VALUE: it may be integer (INT32,
!!      INT64 kinds), real (REAL32, REAL64 kinds), default logical, or
!!      deferred-length character.  The kind parameters are those from the
!!      intrinsic module ISO_FORTRAN_ENV, and typically include default integer
!!      and real, and double precision kinds.  If the type and kind of VALUE
!!      does not match the stored value, the logical argument ERRC returns true;
!!      otherwise it returns false.
!!
!!    VALUE_PTR() returns a CLASS(*) pointer to the value stored by the object.
!!      The object should have the TARGET attribute.
!!
!!    WRITE(UNIT) writes the value stored by the object to the given logical
!!      unit, which must be opened for formatted write access.  The array is
!!      written using the syntax used for array constants: the elements are
!!      separated by commas and the list enclosed in brackets ([ and ]).
!!      No newlines are are written.
!!
!!  ANY_MATRIX TYPE BOUND PROCEDURES
!!
!!    SET_VALUE(VALUE) sets the value stored by the object to a (shallow) copy
!!      of the rank-2 array argument VALUE, which may be of any intrinsic or
!!      derived type.
!!
!!    GET_VALUE(VALUE) gets the value stored by the object. The argument VALUE
!!      is a CLASS(*) allocatable rank-2 array that returns a (shallow) copy
!!      of the stored value.
!!
!!    GET_VALUE(VALUE, ERRC) gets a (shallow) copy of the value stored by the
!!      object.  This is a generic subroutine depending on the type of the
!!      rank-2 allocatable array argument VALUE: it may be integer (INT32,
!!      INT64 kinds), real (REAL32, REAL64 kinds), default logical, or
!!      deferred-length character.  The kind parameters are those from the
!!      intrinsic module ISO_FORTRAN_ENV, and typically include default integer
!!      and real, and double precision kinds.  If the type and kind of VALUE
!!      does not match the stored value, the logical argument ERRC returns true;
!!      otherwise it returns false.
!!
!!    VALUE_PTR() returns a CLASS(*) pointer to the value stored by the object.
!!      The object should have the TARGET attribute.
!!
!!    WRITE(UNIT) writes the value stored by the object to the given logical
!!      unit, which must be opened for formatted write access.  The array is
!!      written using the syntax used for array values: the elements are
!!      written in storage order as a list of lists with lists enclosed in
!!      brackets ([ and ]) and elements and sublists separated by commas. The
!!      inner lists are the columns of the matrix. No newlines are are written.
!!

module parameter_entry_class

  use,intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
  implicit none
  private

  type, abstract, public :: parameter_entry
    !! meta data associated with the parameter will go here
  end type

  type, extends(parameter_entry), public :: any_scalar
    private
    class(*), allocatable :: value
  contains
    procedure :: set_value => set_scalar_value
    procedure :: value_ptr => scalar_value_ptr
    generic   :: get_value => get_scalar_value, get_scalar_character, get_scalar_logical, &
                 get_scalar_int32, get_scalar_int64, get_scalar_real32, get_scalar_real64
    procedure, private :: get_scalar_value
    procedure, private :: get_scalar_int32
    procedure, private :: get_scalar_int64
    procedure, private :: get_scalar_real32
    procedure, private :: get_scalar_real64
    procedure, private :: get_scalar_character
    procedure, private :: get_scalar_logical
    procedure :: write => write_scalar
  end type

  !! User-defined constructor.
  interface any_scalar
    procedure any_scalar_value
  end interface

  type, extends(parameter_entry), public :: any_vector
    private
    class(*), allocatable :: value(:)
  contains
    procedure :: set_value => set_vector_value
    procedure :: value_ptr => vector_value_ptr
    generic   :: get_value => get_vector_value, get_vector_character, get_vector_logical, &
                 get_vector_int32, get_vector_int64, get_vector_real32, get_vector_real64
    procedure, private :: get_vector_value
    procedure, private :: get_vector_int32
    procedure, private :: get_vector_int64
    procedure, private :: get_vector_real32
    procedure, private :: get_vector_real64
    procedure, private :: get_vector_character
    procedure, private :: get_vector_logical
    procedure :: write => write_vector
  end type

  !! User-defined constructor.
  interface any_vector
    procedure any_vector_value
  end interface

  type, extends(parameter_entry), public :: any_matrix
    private
    class(*), allocatable :: value(:,:)
  contains
    procedure :: set_value => set_matrix_value
    procedure :: value_ptr => matrix_value_ptr
    generic   :: get_value => get_matrix_value, get_matrix_character, get_matrix_logical, &
                 get_matrix_int32, get_matrix_int64, get_matrix_real32, get_matrix_real64
    procedure, private :: get_matrix_value
    procedure, private :: get_matrix_int32
    procedure, private :: get_matrix_int64
    procedure, private :: get_matrix_real32
    procedure, private :: get_matrix_real64
    procedure, private :: get_matrix_character
    procedure, private :: get_matrix_logical
    procedure :: write => write_matrix
  end type

  !! User-defined constructor.
  interface any_matrix
    procedure any_matrix_value
  end interface

contains

 !!
 !! SCALAR_ANY TYPE BOUND PROCEDURES
 !!

  !! User-defined constructor.
  function any_scalar_value (value) result (obj)
    class(*), intent(in) :: value
    type(any_scalar) :: obj
    call set_scalar_value (obj, value)
  end function any_scalar_value

  !! Set value; works for any type.
  subroutine set_scalar_value (this, value)
    class(any_scalar), intent(out) :: this
    class(*), intent(in) :: value
    allocate(this%value, source=value)
  end subroutine set_scalar_value

  !! Get value; works for any type.
  subroutine get_scalar_value (this, value)
    class(any_scalar), intent(in) :: this
    class(*), allocatable, intent(out) :: value
    allocate(value, source=this%value)
  end subroutine get_scalar_value

  !! Return a pointer to the value; works for any type.
  function scalar_value_ptr (this) result (uptr)
    class(any_scalar), target, intent(in) :: this
    class(*), pointer :: uptr
    uptr => this%value
  end function scalar_value_ptr

  !! Return a int32 kind integer value.
  subroutine get_scalar_int32 (this, value, errc)
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

  !! Return a int64 kind integer value.
  subroutine get_scalar_int64 (this, value, errc)
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

  !! Return a real32 kind real value.
  subroutine get_scalar_real32 (this, value, errc)
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

  !! Return a real64 kind real value.
  subroutine get_scalar_real64 (this, value, errc)
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

  !! Return a default kind character value; deferred-length allocatable.
  subroutine get_scalar_character (this, value, errc)
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

  !! Return a default kind logical value.
  subroutine get_scalar_logical (this, value, errc)
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

  subroutine write_scalar (this, unit)
    class(any_scalar), intent(in) :: this
    integer, intent(in) :: unit
    character(len=31) :: string
    select type (uptr => this%value)
    type is (integer(int32))
      write(unit,'(i0)',advance='no') uptr
    type is (integer(int64))
      write(unit,'(i0)',advance='no') uptr
    type is (real(real32))
      write(string,fmt=*) uptr
      write(unit,'(a)',advance='no') trim(adjustl(string))
    type is (real(real64))
      write(string,fmt=*) uptr
      write(unit,'(a)',advance='no') trim(adjustl(string))
    type is (logical)
      if (uptr) then
        write(unit,'(a)',advance='no') 'true'
      else
        write(unit,'(a)',advance='no') 'false'
      end if
    type is (character(*))
      write(unit,'(3a)',advance='no') '"', uptr, '"'
    class default
      write(unit,'(a)',advance='no') "???"
    end select
  end subroutine write_scalar

 !!
 !! VECTOR_ANY TYPE BOUND PROCEDURES
 !!

  !! User-defined constructor
  function any_vector_value (value) result (obj)
    class(*), intent(in) :: value(:)
    type(any_vector) :: obj
    call set_vector_value (obj, value)
  end function any_vector_value

  !! Set value; works for any type.
  subroutine set_vector_value (this, value)
    class(any_vector), intent(out) :: this
    class(*), intent(in) :: value(:)
    allocate(this%value(lbound(value,1):ubound(value,1)), source=value)
  end subroutine set_vector_value

  !! Get value; works for any type.
  subroutine get_vector_value (this, value)
    class(any_vector), intent(in) :: this
    class(*), allocatable, intent(out) :: value(:)
    allocate(value(lbound(this%value,1):ubound(this%value,1)), source=this%value)
  end subroutine get_vector_value

  !! Return a pointer to the value; works for any type.
  function vector_value_ptr (this) result (uptr)
    class(any_vector), target, intent(in) :: this
    class(*), pointer :: uptr(:)
    uptr => this%value
  end function vector_value_ptr

  !! Return a int32 kind integer value.
  subroutine get_vector_int32 (this, value, errc)
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

  !! Return a int64 kind integer value.
  subroutine get_vector_int64 (this, value, errc)
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

  !! Return a real32 kind real value.
  subroutine get_vector_real32 (this, value, errc)
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

  !! Return a real64 kind real value.
  subroutine get_vector_real64 (this, value, errc)
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

  subroutine get_vector_character (this, value, errc)
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

  subroutine get_vector_logical (this, value, errc)
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

  subroutine write_vector (this, unit)
    class(any_vector), intent(in) :: this
    integer, intent(in) :: unit
    integer :: n
    character(len=31) :: string
    select type (val => this%value)
    type is (integer(int32))
      write(unit,'("[")',advance='no')
      do n = 1, size(val)
        if (n > 1) write(unit,'(", ")',advance='no')
        write(unit,'(i0)',advance='no') val(n)
      end do
      write(unit,'("]")',advance='no')
    type is (integer(int64))
      write(unit,'("[")',advance='no')
      do n = 1, size(val)
        if (n > 1) write(unit,'(", ")',advance='no')
        write(unit,'(i0)',advance='no') val(n)
      end do
      write(unit,'("]")',advance='no')
    type is (real(real32))
      write(unit,'("[")',advance='no')
      do n = 1, size(val)
        if (n > 1) write(unit,'(", ")',advance='no')
        write(string,fmt=*) val(n)
        write(unit,'(a)',advance='no') trim(adjustl(string))
      end do
      write(unit,'("]")',advance='no')
    type is (real(real64))
      write(unit,'("[")',advance='no')
      do n = 1, size(val)
        if (n > 1) write(unit,'(", ")',advance='no')
        write(string,fmt=*) val(n)
        write(unit,'(a)',advance='no') trim(adjustl(string))
      end do
      write(unit,'("]")',advance='no')
    type is (logical)
      write(unit,'("[")',advance='no')
      do n = 1, size(val)
        if (n > 1) write(unit,'(", ")',advance='no')
        if (val(n)) then
          write(unit,'("true")',advance='no')
        else
          write(unit,'("false")',advance='no')
        end if
      end do
      write(unit,'("]")',advance='no')
    type is (character(*))
      write(unit,'("[")',advance='no')
      do n = 1, size(val)
        if (n > 1) write(unit,'(", ")',advance='no')
        write(unit,'(3a)',advance='no') '"', trim(val(n)), '"'
      end do
      write(unit,'("]")',advance='no')
    class default
      write(unit,'("[")',advance='no')
      do n = 1, size(val)
        if (n > 1) write(unit,'(", ")',advance='no')
        write(unit,'(a)',advance='no') '*'
      end do
      write(unit,'("]")',advance='no')
    end select
  end subroutine write_vector

 !!
 !! MATRIX_ANY TYPE BOUND PROCEDURES
 !!

  !! User-defined constructor
  function any_matrix_value (value) result (obj)
    class(*), intent(in) :: value(:,:)
    type(any_matrix) :: obj
    call set_matrix_value (obj, value)
  end function any_matrix_value

  !! Set value; works for any type.
  subroutine set_matrix_value (this, value)
    class(any_matrix), intent(out) :: this
    class(*), intent(in) :: value(:,:)
    allocate(this%value(lbound(value,1):ubound(value,1),&
                        lbound(value,2):ubound(value,2)), source=value)
  end subroutine set_matrix_value

  !! Get value; works for any type.
  subroutine get_matrix_value (this, value)
    class(any_matrix), intent(in) :: this
    class(*), allocatable, intent(out) :: value(:,:)
    allocate(value(lbound(this%value,1):ubound(this%value,1), &
                   lbound(this%value,2):ubound(this%value,2)), source=this%value)
  end subroutine get_matrix_value

  !! Return a pointer to the value; works for any type.
  function matrix_value_ptr (this) result (uptr)
    class(any_matrix), target, intent(in) :: this
    class(*), pointer :: uptr(:,:)
    uptr => this%value
  end function matrix_value_ptr

  !! Return a int32 kind integer value.
  subroutine get_matrix_int32 (this, value, errc)
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

! A fragment that does what F2003 automatic reallocation should do:
!if (allocated(this%value)) then
!  if (allocated(value)) then
!    if (size(value) /= size(value)) deallocate(value)
!  end if
!  if (.not.allocated(value)) allocate(value(size(this%value))
!else
!  if (allocated(value)) deallocate(value)
!end if

  !! Return a int64 kind integer value.
  subroutine get_matrix_int64 (this, value, errc)
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

  !! Return a real32 kind real value.
  subroutine get_matrix_real32 (this, value, errc)
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

  !! Return a real64 kind real value.
  subroutine get_matrix_real64 (this, value, errc)
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

  subroutine get_matrix_character (this, value, errc)
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

  subroutine get_matrix_logical (this, value, errc)
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

  subroutine write_matrix (this, unit)
    class(any_matrix), intent(in) :: this
    integer, intent(in) :: unit
    integer :: n, m
    character(len=31) :: string
    select type (val => this%value)
    type is (integer(int32))
      write(unit,'("[")',advance='no')
      do n = 1, size(val,2)
        if (n > 1) write(unit,'(", ")',advance='no')
        write(unit,'("[")',advance='no')
        do m = 1, size(val,1)
          if (m > 1) write(unit,'(", ")',advance='no')
          write(unit,'(i0)',advance='no') val(m,n)
        end do
        write(unit,'("]")',advance='no')
      end do
      write(unit,'("]")',advance='no')
    type is (integer(int64))
      write(unit,'("[")',advance='no')
      do n = 1, size(val,2)
        if (n > 1) write(unit,'(", ")',advance='no')
        write(unit,'("[")',advance='no')
        do m = 1, size(val,1)
          if (m > 1) write(unit,'(", ")',advance='no')
          write(unit,'(i0)',advance='no') val(m,n)
        end do
        write(unit,'("]")',advance='no')
      end do
      write(unit,'("]")',advance='no')
    type is (real(real32))
      write(unit,'("[")',advance='no')
      do n = 1, size(val,2)
        if (n > 1) write(unit,'(", ")',advance='no')
        write(unit,'("[")',advance='no')
        do m = 1, size(val,1)
          if (m > 1) write(unit,'(", ")',advance='no')
          write(string,fmt=*) val(m,n)
          write(unit,'(a)',advance='no') trim(adjustl(string))
        end do
        write(unit,'("]")',advance='no')
      end do
      write(unit,'("]")',advance='no')
    type is (real(real64))
      write(unit,'("[")',advance='no')
      do n = 1, size(val,2)
        if (n > 1) write(unit,'(", ")',advance='no')
        write(unit,'("[")',advance='no')
        do m = 1, size(val,1)
          if (m > 1) write(unit,'(", ")',advance='no')
          write(string,fmt=*) val(m,n)
          write(unit,'(a)',advance='no') trim(adjustl(string))
        end do
        write(unit,'("]")',advance='no')
      end do
      write(unit,'("]")',advance='no')
    type is (logical)
      write(unit,'("[")',advance='no')
      do n = 1, size(val,2)
        if (n > 1) write(unit,'(", ")',advance='no')
        write(unit,'("[")',advance='no')
        do m = 1, size(val,1)
          if (m > 1) write(unit,'(", ")',advance='no')
          if (val(m,n)) then
            write(unit,'("true")',advance='no')
          else
            write(unit,'("false")',advance='no')
          end if
        end do
        write(unit,'("]")',advance='no')
      end do
      write(unit,'("]")',advance='no')
    type is (character(*))
      write(unit,'("[")',advance='no')
      do n = 1, size(val,2)
        if (n > 1) write(unit,'(", ")',advance='no')
        write(unit,'("[")',advance='no')
        do m = 1, size(val,1)
          if (m > 1) write(unit,'(", ")',advance='no')
          write(unit,'(3a)',advance='no') '"', trim(val(m,n)), '"'
        end do
        write(unit,'("]")',advance='no')
      end do
      write(unit,'("]")',advance='no')
    class default
      write(unit,'("[")',advance='no')
      do n = 1, size(val,2)
        if (n > 1) write(unit,'(", ")',advance='no')
        write(unit,'("[")',advance='no')
        do m = 1, size(val,1)
          if (m > 1) write(unit,'(", ")',advance='no')
          write(unit,'(a)',advance='no') '*'
        end do
        write(unit,'("]")',advance='no')
      end do
      write(unit,'("]")',advance='no')
    end select
  end subroutine write_matrix

end module parameter_entry_class
