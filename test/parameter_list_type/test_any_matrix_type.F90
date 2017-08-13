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

program test_any_matrix_type

  use parameter_entry_class
  use,intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
#ifdef NAGFOR
  use,intrinsic :: f90_unix, only: exit
#endif

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

  call exit (stat)

contains

  subroutine test_set_get_int32

    type(any_matrix) :: x
    integer(int32), allocatable :: val(:,:), ref(:,:)
    integer(int64), allocatable :: bad(:,:)
    logical :: errc

    ref = reshape([integer(int32) :: 5, 7, 3, 1, 0, 5], shape=[2,3])
    x = any_matrix(ref)  ! constructor

    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_int32 failed test 1')
    if (.not.allocated(val)) then
      call write_fail ('test_set_get_int32 failed test 1b')
    else
      if (any(shape(val) /= shape(ref))) then
        call write_fail ('test_set_get_int32 failed test 1c')
      else
        if (any(val /= ref)) call write_fail ('test_set_get_int32 failed test 2')
      end if
    end if
    call x%get_value (bad, errc)
    if (.not.errc) call write_fail ('test_set_get_int32 failed test 3')

    ref = reshape([integer(int32) :: 11, 13], shape=[1,2])
    call x%set_value (ref) ! overwrite
    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_int32 failed test 4')
    if (.not.allocated(val)) then
      call write_fail ('test_set_get_int32 failed test 4b')
    else
      if (any(shape(val) /= shape(ref))) then
        call write_fail ('test_set_get_int32 failed test 4c')
      else
        if (any(val /= ref)) call write_fail ('test_set_get_int32 failed test 5')
      end if
    end if

  end subroutine

  subroutine test_set_get_int64

    type(any_matrix) :: x
    integer(int64), allocatable :: val(:,:), ref(:,:)
    integer(int32), allocatable :: bad(:,:)
    logical :: errc

    ref = reshape([integer(int64) :: 5, 7, 3, 1, 0, 5], shape=[2,3])
    x = any_matrix(ref)  ! constructor

    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_int64 failed test 1')
    if (.not.allocated(val)) then
      call write_fail ('test_set_get_int64 failed test 1b')
    else
      if (any(shape(val) /= shape(ref))) then
        call write_fail ('test_set_get_int64 failed test 1c')
      else
        if (any(val /= ref)) call write_fail ('test_set_get_int64 failed test 2')
      end if
    end if
    call x%get_value (bad, errc)
    if (.not.errc) call write_fail ('test_set_get_int64 failed test 3')

    ref = reshape([integer(int32) :: 11, 13], shape=[1,2])
    call x%set_value (ref) ! overwrite
    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_int64 failed test 4')
    if (.not.allocated(val)) then
      call write_fail ('test_set_get_int64 failed test 4b')
    else
      if (any(shape(val) /= shape(ref))) then
        call write_fail ('test_set_get_int64 failed test 4c')
      else
        if (any(val /= ref)) call write_fail ('test_set_get_int64 failed test 5')
      end if
    end if

  end subroutine

  subroutine test_set_get_real32

    type(any_matrix) :: x
    real(real32), allocatable :: val(:,:), ref(:,:)
    real(real64), allocatable :: bad(:,:)
    logical :: errc

    ref = reshape([real(real32) :: 5, 7, 3, 1, 0, 5], shape=[2,3])
    x = any_matrix(ref)  ! constructor

    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_real32 failed test 1')
    if (.not.allocated(val)) then
      call write_fail ('test_set_get_real32 failed test 1b')
    else
      if (any(shape(val) /= shape(ref))) then
        call write_fail ('test_set_get_real32 failed test 1c')
      else
        if (any(val /= ref)) call write_fail ('test_set_get_real32 failed test 2')
      end if
    end if
    call x%get_value (bad, errc)
    if (.not.errc) call write_fail ('test_set_get_real32 failed test 3')

    ref = reshape([real(real32) :: 11, 13], shape=[1,2])
    call x%set_value (ref) ! overwrite
    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_real32 failed test 4')
    if (.not.allocated(val)) then
      call write_fail ('test_set_get_real32 failed test 4b')
    else
      if (any(shape(val) /= shape(ref))) then
        call write_fail ('test_set_get_real32 failed test 4c')
      else
        if (any(val /= ref)) call write_fail ('test_set_get_real32 failed test 5')
      end if
    end if

  end subroutine

  subroutine test_set_get_real64

    type(any_matrix) :: x
    real(real64), allocatable :: val(:,:), ref(:,:)
    real(real32), allocatable :: bad(:,:)
    logical :: errc

    ref = reshape([real(real64) :: 5, 7, 3, 1, 0, 5], shape=[2,3])
    x = any_matrix(ref)  ! constructor

    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_real64 failed test 1')
    if (.not.allocated(val)) then
      call write_fail ('test_set_get_real64 failed test 1b')
    else
      if (any(shape(val) /= shape(ref))) then
        call write_fail ('test_set_get_real64 failed test 1c')
      else
        if (any(val /= ref)) call write_fail ('test_set_get_real64 failed test 2')
      end if
    end if
    call x%get_value (bad, errc)
    if (.not.errc) call write_fail ('test_set_get_real64 failed test 3')

    ref = reshape([real(real64) :: 11, 13], shape=[1,2])
    call x%set_value (ref) ! overwrite
    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_real64 failed test 4')
    if (.not.allocated(val)) then
      call write_fail ('test_set_get_real64 failed test 4b')
    else
      if (any(shape(val) /= shape(ref))) then
        call write_fail ('test_set_get_real64 failed test 4c')
      else
        if (any(val /= ref)) call write_fail ('test_set_get_real64 failed test 5')
      end if
    end if

  end subroutine

  subroutine test_set_get_logical

    type(any_matrix) :: x
    logical, allocatable :: val(:,:), ref(:,:)
    integer, allocatable :: bad(:,:)
    logical :: errc

    ref = reshape([.true.,.false.], shape=[1,2])
    x = any_matrix(ref)  ! constructor

    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_logical failed test 1')
    if (.not.allocated(val)) then
      call write_fail ('test_set_get_logical failed test 1b')
    else
      if (any(shape(val) /= shape(ref))) then
        call write_fail ('test_set_get_logical failed test 1c')
      else
        if (any(val .neqv. ref)) call write_fail ('test_set_get_logical failed test 2')
      end if
    end if
    call x%get_value (bad, errc)
    if (.not.errc) call write_fail ('test_set_get_logical failed test 3')

    ref = reshape([.false.], shape=[1,1])
    call x%set_value (ref) ! overwrite
    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_logical failed test 4')
    if (.not.allocated(val)) then
      call write_fail ('test_set_get_logical failed test 4b')
    else
      if (any(shape(val) /= shape(ref))) then
        call write_fail ('test_set_get_logical failed test 4c')
      else
        if (any(val .neqv. ref)) call write_fail ('test_set_get_logical failed test 5')
      end if
    end if

  end subroutine

  subroutine test_set_get_character

    type(any_matrix) :: x
    character(:), allocatable :: val(:,:), ref(:,:)
    integer, allocatable :: bad(:,:)
    logical :: errc

    ref = reshape(['foo','bar'], shape=[2,1])
    x = any_matrix(ref)  ! constructor

    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_character failed test 1')
    if (.not.allocated(val)) then
      call write_fail ('test_set_get_character failed test 1b')
    else
      if (any(shape(val) /= shape(ref))) then
        call write_fail ('test_set_get_character failed test 1c')
      else
        if (any(val /= ref)) call write_fail ('test_set_get_character failed test 2')
      end if
    end if
    call x%get_value (bad, errc)
    if (.not.errc) call write_fail ('test_set_get_character failed test 3')

    ref = reshape(['fubar'], shape=[1,1])
    call x%set_value (ref) ! overwrite
    call x%get_value (val, errc)
    if (errc) call write_fail ('test_set_get_character failed test 4')
    if (.not.allocated(val)) then
      call write_fail ('test_set_get_character failed test 4b')
    else
      if (any(shape(val) /= shape(ref))) then
        call write_fail ('test_set_get_character failed test 4c')
      else
        if (any(val /= ref)) call write_fail ('test_set_get_character failed test 5')
      end if
    end if

  end subroutine

  subroutine test_get_value

    type(any_matrix) :: x
    class(*), allocatable :: val(:,:)
    integer, allocatable :: iref(:,:)
    real, allocatable :: rref(:,:)
    character(:), allocatable :: cref(:,:)

    iref = reshape([1,2,3,4,5,6], shape=[3,2])
    call x%set_value (iref)
    call x%get_value (val)
    if (.not.allocated(val)) then
      call write_fail ('test_get_value failed test 1b')
    else
      if (any(shape(val) /= shape(iref))) then
        call write_fail ('test_get_value failed test 1c')
      else
        select type (val)
        type is (integer)
          if (any(val /= iref)) call write_fail ('test_get_value failed test 1')
        class default
          call write_fail ('test_get_value failed test 2')
        end select
      end if
    end if

    rref = reshape([1.0,3.14], shape=[1,2])
    call x%set_value (rref)
    call x%get_value (val)
    if (.not.allocated(val)) then
      call write_fail ('test_get_value failed test 3b')
    else
      if (any(shape(val) /= shape(rref))) then
        call write_fail ('test_get_value failed test 3c')
      else
        select type (val)
        type is (real)
          if (any(val /= rref)) call write_fail ('test_get_value failed test 3')
        class default
          call write_fail ('test_get_value failed test 4')
        end select
      end if
    end if

    cref = reshape(['biz','bat'], shape=[2,1])
    call x%set_value (cref)
    call x%get_value (val)
    if (.not.allocated(val)) then
      call write_fail ('test_get_value failed test 5b')
    else
      if (any(shape(val) /= shape(cref))) then
        call write_fail ('test_get_value failed test 5c')
      else
        select type (val)
        type is (character(*))
          if (any(val /= cref)) call write_fail ('test_get_value failed test 5')
        class default
          call write_fail ('test_get_value failed test 6')
        end select
      end if
    end if

  end subroutine

  subroutine test_value_ptr

    type(any_matrix), target :: x
    class(*), pointer :: val(:,:)
    integer, allocatable :: iref(:,:)
    real, allocatable :: rref(:,:)
    character(:), allocatable :: cref(:,:)

    iref = reshape([1,2,3,4,5,6], shape=[3,2])
    call x%set_value (iref)
    val => x%value_ptr()
    select type (val)
    type is (integer)
      if (any(val /= iref)) call write_fail ('test_value_ptr failed test 1')
    class default
      call write_fail ('test_value_ptr failed test 2')
    end select

    rref = reshape([1.0,3.14], shape=[1,2])
    call x%set_value (rref)
    val => x%value_ptr()
    select type (val)
    type is (real)
      if (any(val /= rref)) call write_fail ('test_value_ptr failed test 3')
    class default
      call write_fail ('test_value_ptr failed test 4')
    end select

    cref = reshape(['biz','bat'], shape=[2,1])
    call x%set_value (cref)
    val => x%value_ptr()
    select type (val)
    type is (character(*))
      if (any(val /= cref)) call write_fail ('test_value_ptr failed test 5')
    class default
      call write_fail ('test_value_ptr failed test 6')
    end select

  end subroutine

  subroutine test_derived_type_value

    type(any_matrix) :: x
    class(*), allocatable :: val(:,:)
    class(*), pointer :: ptr(:,:)

    type point
      real x, y
    end type

    x = any_matrix(reshape([point(1.0,2.0),point(3.0,4.0)], shape=[1,2]))
    call x%get_value (val)
    select type (val)
    type is (point)
      if (any(val%x /= reshape([1.0,3.0],shape=[1,2])) .or. &
          any(val%y /= reshape([2.0,4.0],shape=[1,2]))) call write_fail ('test_derived_type_value failed test 1')
    class default
      call write_fail ('test_derived_type_value failed test 2')
    end select

    call x%set_value(reshape([point(1.0,2.0)], shape=[1,1]))
    ptr => x%value_ptr()
    select type (ptr)
    type is (point)
      if (ptr(1,1)%x /= 1.0 .or. ptr(1,1)%y /= 2.0) call write_fail ('test_derived_type_value failed test 3')
    class default
      call write_fail ('test_derived_type_value failed test 4')
    end select

  end subroutine

  subroutine test_assignment

    type(any_matrix) :: x, y
    integer, allocatable :: ival(:,:)
    real, allocatable :: rval(:,:)
    character(:), allocatable :: cval(:,:)
    logical :: errc
    class(*), allocatable :: val(:,:)

    type point
      real x, y
    end type

    !! Intrinsic type values
    call x%set_value (reshape([1,2],shape=[2,1]))
    y = x
    call x%set_value (reshape([3.14],shape=[1,1]))
    call y%get_value (ival, errc)
    if (errc) call write_fail ('test_assignment failed test 1')
    if (any(ival /= reshape([1,2],shape=[2,1]))) call write_fail ('test_assignment failed test 2')
    call x%get_value (rval, errc)
    if (errc) call write_fail ('test_assignment failed test 3')
    if (rval(1,1) /= 3.14) call write_fail ('test_assignment failed test 4')

    !! Simple derived type values
    call x%set_value (reshape([point(1.0,2.0)],shape=[1,1]))
    y = x
    call x%set_value (reshape([1.0,3.14],shape=[1,2]))
    call y%get_value (val)
    select type (val)
    type is (point)
      if (val(1,1)%x /= 1.0 .or. val(1,1)%y /= 2.0) call write_fail ('test_assignment failed test 5')
    class default
      call write_fail ('test_assignment failed test 6')
    end select
    call x%get_value (rval, errc)
    if (errc) call write_fail ('test_assignment failed test 7')
    if (any(rval /= reshape([1.0,3.14],shape=[1,2]))) call write_fail ('test_assignment failed test 4')

    !! Character type values
    call x%set_value (reshape(['foo','bar'],shape=[1,2]))
    y = x
    call x%set_value (reshape([3.14],shape=[1,1]))
    call y%get_value (cval, errc)
    if (errc) call write_fail ('test_assignment failed test 9')
    if (any(cval(1,:) /= ['foo','bar'])) call write_fail ('test_assignment failed test 10')
    call x%get_value (rval, errc)
    if (errc) call write_fail ('test_assignment failed test 11')
    if (rval(1,1) /= 3.14) call write_fail ('test_assignment failed test 12')

  end subroutine

  subroutine test_shallow_assignment

    type(any_matrix) :: x, y
    class(*), allocatable :: xval(:,:), yval(:,:)
    integer, pointer :: xp1, yp1, xp2, yp2

    type box
      integer, pointer :: n => null()
    end type
    type(box) :: b(1,2)

    allocate(b(1,1)%n, b(1,2)%n)
    b(1,1)%n = 1
    b(1,2)%n = 2

    call x%set_value (b) ! x value is shallow copy of b
    y = x ! y value is a shallow copy of x

    call x%get_value (xval)
    select type (xval)
    type is (box)
      xp1 => xval(1,1)%n
      xp2 => xval(1,2)%n
    end select

    call y%get_value (yval)
    select type (yval)
    type is (box)
      yp1 => yval(1,1)%n
      yp2 => yval(1,2)%n
    end select

    if (.not.associated(xp1,yp1))  call write_fail ('test_shallow_assignment test 1 failed')
    if (.not.associated(xp1,b(1,1)%n)) call write_fail ('test_shallow_assignment test 2 failed')
    if (.not.associated(xp2,yp2))  call write_fail ('test_shallow_assignment test 3 failed')
    if (.not.associated(xp2,b(1,2)%n)) call write_fail ('test_shallow_assignment test 4 failed')
    
    deallocate(b(1,1)%n,b(1,2)%n) ! clean-up in case we use a memory checker

  end subroutine

  subroutine write_fail (errmsg)
    use,intrinsic :: iso_fortran_env, only: error_unit
    character(*), intent(in) :: errmsg
    stat = 1
    write(error_unit,'(a)') errmsg
  end subroutine

end program test_any_matrix_type
