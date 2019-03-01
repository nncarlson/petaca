!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2013 Neil N. Carlson
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

program test_any_scalar_type

  use parameter_entry_class
  use,intrinsic :: iso_fortran_env, only: int32, int64, real32, real64

  integer :: stat = 0

  call test_set_get_int32
  call test_set_get_int64
  call test_set_get_real32
  call test_set_get_real64
  call test_set_get_logical
  call test_set_get_character
  call test_get_value
  call test_value_ptr
  call test_derived_type_value
  call test_assignment
  call test_shallow_assignment

  if (stat /= 0) stop 1

contains

  subroutine test_set_get_int32

    type(any_scalar) :: x
    integer(int32) :: val
    integer(int64) :: bad
    logical :: errc

    x = any_scalar(13_int32)  ! constructor

    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_int32 failed test 1')
    if (val /= 13) call write_fail ('test_set_get_int32 failed test 2')
    call x%get_value (bad, errc)
    if (.not.errc) call write_fail ('test_set_get_int32 failed test 3')

    call x%set_value (17_int32) ! overwrite
    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_int32 failed test 4')
    if (val /= 17) call write_fail ('test_set_get_int32 failed test 5')

  end subroutine

  subroutine test_set_get_int64

    type(any_scalar) :: x
    integer(int64) :: val
    integer(int32) :: bad
    logical :: errc

    x = any_scalar(13_int64)  ! constructor

    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_int64 failed test 1')
    if (val /= 13) call write_fail ('test_set_get_int64 failed test 2')
    call x%get_value (bad, errc)
    if (.not.errc) call write_fail ('test_set_get_int64 failed test 3')

    call x%set_value (17_int64) ! overwrite
    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_int64 failed test 4')
    if (val /= 17) call write_fail ('test_set_get_int64 failed test 5')

  end subroutine

  subroutine test_set_get_real32

    type(any_scalar) :: x
    real(real32) :: val
    real(real64) :: bad
    logical :: errc

    x = any_scalar(13.0_real32)  ! constructor

    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_real32 failed test 1')
    if (val /= 13.0) call write_fail ('test_set_get_real32 failed test 2')
    call x%get_value (bad, errc)
    if (.not.errc) call write_fail ('test_set_get_real32 failed test 3')

    call x%set_value (17.0_real32) ! overwrite
    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_real32 failed test 4')
    if (val /= 17.0) call write_fail ('test_set_get_real32 failed test 5')

  end subroutine

  subroutine test_set_get_real64

    type(any_scalar) :: x
    real(real64) :: val
    real(real32) :: bad
    logical :: errc

    x = any_scalar(13.0_real64)  ! constructor

    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_real64 failed test 1')
    if (val /= 13.0) call write_fail ('test_set_get_real64 failed test 2')
    call x%get_value (bad, errc)
    if (.not.errc) call write_fail ('test_set_get_real64 failed test 3')

    call x%set_value (17.0_real64) ! overwrite
    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_real64 failed test 4')
    if (val /= 17.0) call write_fail ('test_set_get_real64 failed test 5')

  end subroutine

  subroutine test_set_get_logical

    type(any_scalar) :: x
    logical :: val
    integer :: bad
    logical :: errc

    x = any_scalar(.true.)  ! constructor

    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_logical failed test 1')
    if (val .neqv. .true.) call write_fail ('test_set_get_logical failed test 2')
    call x%get_value (bad, errc)
    if (.not.errc) call write_fail ('test_set_get_logical failed test 3')

    call x%set_value (.false.) ! overwrite
    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_logical failed test 4')
    if (val .neqv. .false.) call write_fail ('test_set_get_logical failed test 5')

  end subroutine

  subroutine test_set_get_character

    type(any_scalar) :: x
    character(:), allocatable :: val
    integer :: bad
    logical :: errc

    x = any_scalar('foo')  ! constructor

    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_character failed test 1')
    if (val /= 'foo') call write_fail ('test_set_get_character failed test 2')
    call x%get_value (bad, errc)
    if (.not.errc) call write_fail ('test_set_get_character failed test 3')

    call x%set_value ('fubar') ! overwrite
    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_character failed test 4')
    if (val /= 'fubar') call write_fail ('test_set_get_character failed test 5')

  end subroutine

  subroutine test_get_value

    type(any_scalar) :: x
    class(*), allocatable :: val

    call x%set_value (23)
    call x%get_value (val)
    select type (val)
    type is (integer)
      if (val /= 23) call write_fail ('test_get_value failed test 1')
    class default
      call write_fail ('test_get_value failed test 2')
    end select

    call x%set_value (3.14)
    call x%get_value (val)
    select type (val)
    type is (real)
      if (val /= 3.14) call write_fail ('test_get_value failed test 3')
    class default
      call write_fail ('test_get_value failed test 4')
    end select

    call x%set_value ('bizbat')
    call x%get_value (val)
    select type (val)
    type is (character(*))
      if (val /= 'bizbat') call write_fail ('test_get_value failed test 5')
    class default
      call write_fail ('test_get_value failed test 6')
    end select

  end subroutine

  subroutine test_value_ptr

    type(any_scalar), target :: x
    class(*), pointer :: val

    call x%set_value (23)
    val => x%value_ptr()
    select type (val)
    type is (integer)
      if (val /= 23) call write_fail ('test_value_ptr failed test 1')
    class default
      call write_fail ('test_value_ptr failed test 2')
    end select

    call x%set_value (3.14)
    val => x%value_ptr()
    select type (val)
    type is (real)
      if (val /= 3.14) call write_fail ('test_value_ptr failed test 3')
    class default
      call write_fail ('test_value_ptr failed test 4')
    end select

    call x%set_value ('bizbat')
    val => x%value_ptr()
    select type (val)
    type is (character(*))
      if (val /= 'bizbat') call write_fail ('test_get_value failed test 5')
    class default
      call write_fail ('test_get_value failed test 6')
    end select

  end subroutine

  subroutine test_derived_type_value

    type(any_scalar) :: x
    class(*), allocatable :: val
    class(*), pointer :: ptr

    type point
      real x, y
    end type

    x = any_scalar(point(1.0,2.0))
    call x%get_value (val)
    select type (val)
    type is (point)
      if (val%x /= 1.0 .or. val%y /= 2.0) call write_fail ('test_derived_type_value failed test 1')
    class default
      call write_fail ('test_derived_type_value failed test 2')
    end select

    call x%set_value(point(3.0,4.0))
    ptr => x%value_ptr()
    select type (ptr)
    type is (point)
      if (ptr%x /= 3.0 .or. ptr%y /= 4.0) call write_fail ('test_derived_type_value failed test 3')
    class default
      call write_fail ('test_derived_type_value failed test 4')
    end select

  end subroutine

  subroutine test_assignment

    type(any_scalar) :: x, y
    integer :: ival
    real :: rval
    character(:), allocatable :: cval
    logical :: errc
    class(*), allocatable :: val

    type point
      real x, y
    end type

    !! Intrinsic type values
    call x%set_value (1)
    y = x
    call x%set_value (3.14)
    call y%get_value (ival, errc)
    if (errc) call write_fail ('test_assignment failed test 1')
    if (ival /= 1) call write_fail ('test_assignment failed test 2')
    call x%get_value (rval, errc)
    if (errc) call write_fail ('test_assignment failed test 3')
    if (rval /= 3.14) call write_fail ('test_assignment failed test 4')

    !! Simple derived type values
    call x%set_value (point(1.0,2.0))
    y = x
    call x%set_value (3.14)
    call y%get_value (val)
    select type (val)
    type is (point)
      if (val%x /= 1.0 .or. val%y /= 2.0) call write_fail ('test_assignment failed test 5')
    class default
      call write_fail ('test_assignment failed test 6')
    end select
    call x%get_value (rval, errc)
    if (errc) call write_fail ('test_assignment failed test 7')
    if (rval /= 3.14) call write_fail ('test_assignment failed test 8')

    !! Character type values
    call x%set_value ('foo')
    y = x
    call x%set_value (3.14)
    call y%get_value (cval, errc)
    if (errc) call write_fail ('test_assignment failed test 9')
    if (cval /= 'foo') call write_fail ('test_assignment failed test 10')
    call x%get_value (rval, errc)
    if (errc) call write_fail ('test_assignment failed test 11')
    if (rval /= 3.14) call write_fail ('test_assignment failed test 12')

  end subroutine

  subroutine test_shallow_assignment

    type(any_scalar) :: x, y
    class(*), allocatable :: xval, yval
    integer, pointer :: xp, yp

    type box
      integer, pointer :: n => null()
    end type
    type(box) :: b

    allocate(b%n)
    b%n = 1

    call x%set_value (b) ! x value is shallow copy of b
    y = x ! y value is a shallow copy of x

    call x%get_value (xval)
    select type (xval)
    type is (box)
      xp => xval%n
    end select

    call y%get_value (yval)
    select type (yval)
    type is (box)
      yp => yval%n
    end select

    if (.not.associated(xp,yp))  call write_fail ('test_shallow_assignment test 1 failed')
    if (.not.associated(xp,b%n)) call write_fail ('test_shallow_assignment test 2 failed')

    deallocate(b%n) ! clean-up in case we use a memory checker

  end subroutine

  subroutine write_fail (errmsg)
    use,intrinsic :: iso_fortran_env, only: error_unit
    character(*), intent(in) :: errmsg
    stat = 1
    write(error_unit,'(a)') errmsg
  end subroutine

end program test_any_scalar_type
