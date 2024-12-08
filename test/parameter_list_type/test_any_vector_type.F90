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

program test_any_vector_type

  use parameter_list_type
  use,intrinsic :: iso_fortran_env, only: int32, int64, real32, real64

  integer :: stat = 0

  call test_set_get_int32
  call test_set_get_int64
  call test_set_get_real32
  call test_set_get_real64
  call test_set_get_cmplx32
  call test_set_get_cmplx64
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

    type(any_vector) :: x
    integer(int32), allocatable :: val(:)
    integer(int64), allocatable :: bad(:)
    logical :: errc

    x = any_vector([5_int32, 7_int32])  ! constructor

    call x%get_value(val, errc)
    if (errc) call write_fail('test_set_get_int32 failed test 1')
    if (.not.allocated(val)) then
      call write_fail('test_set_get_int32 failed test 1b')
    else
      if (size(val) /= 2) then
        call write_fail('test_set_get_int32 failed test 1c')
      else
        if (any(val /= [5,7])) call write_fail('test_set_get_int32 failed test 2')
      end if
    end if
    call x%get_value(bad, errc)
    if (.not.errc) call write_fail('test_set_get_int32 failed test 3')

    call x%set_value([11_int32]) ! overwrite
    call x%get_value(val, errc)
    if (errc) call write_fail('test_set_get_int32 failed test 4')
    if (.not.allocated(val)) then
      call write_fail('test_set_get_int32 failed test 4b')
    else
      if (size(val) /= 1) then
        call write_fail('test_set_get_int32 failed test 4c')
      else
        if (any(val /= [11])) call write_fail('test_set_get_int32 failed test 5')
      end if
    end if

  end subroutine

  subroutine test_set_get_int64

    type(any_vector) :: x
    integer(int64), allocatable :: val(:)
    integer(int32), allocatable :: bad(:)
    logical :: errc

    x = any_vector([5_int64, 7_int64])  ! constructor

    call x%get_value(val, errc)
    if (errc) call write_fail('test_set_get_int64 failed test 1')
    if (.not.allocated(val)) then
      call write_fail('test_set_get_int64 failed test 1b')
    else
      if (size(val) /= 2) then
        call write_fail('test_set_get_int64 failed test 1c')
      else
        if (any(val /= [5,7])) call write_fail('test_set_get_int64 failed test 2')
      end if
    end if
    call x%get_value(bad, errc)
    if (.not.errc) call write_fail('test_set_get_int64 failed test 3')

    call x%set_value([11_int64]) ! overwrite
    call x%get_value(val, errc)
    if (errc) call write_fail('test_set_get_int64 failed test 4')
    if (.not.allocated(val)) then
      call write_fail('test_set_get_int64 failed test 4b')
    else
      if (size(val) /= 1) then
        call write_fail('test_set_get_int64 failed test 4c')
      else
        if (any(val /= [11])) call write_fail('test_set_get_int64 failed test 5')
      end if
    end if

  end subroutine

  subroutine test_set_get_real32

    type(any_vector) :: x
    real(real32), allocatable :: val(:)
    real(real64), allocatable :: bad(:)
    logical :: errc

    x = any_vector([5.0_real32, 7.0_real32])  ! constructor

    call x%get_value(val, errc)
    if (errc) call write_fail('test_set_get_real32 failed test 1')
    if (.not.allocated(val)) then
      call write_fail('test_set_get_real32 failed test 1b')
    else
      if (size(val) /= 2) then
        call write_fail('test_set_get_real32 failed test 1c')
      else
        if (any(val /= [5.0,7.0])) call write_fail('test_set_get_real32 failed test 2')
      end if
    end if
    call x%get_value(bad, errc)
    if (.not.errc) call write_fail('test_set_get_real32 failed test 3')

    call x%set_value([11.0_real32]) ! overwrite
    call x%get_value(val, errc)
    if (errc) call write_fail('test_set_get_real32 failed test 4')
    if (.not.allocated(val)) then
      call write_fail('test_set_get_real32 failed test 4b')
    else
      if (size(val) /= 1) then
        call write_fail('test_set_get_real32 failed test 4c')
      else
        if (any(val /= [11.0])) call write_fail('test_set_get_real32 failed test 5')
      end if
    end if

  end subroutine

  subroutine test_set_get_real64

    type(any_vector) :: x
    real(real64), allocatable :: val(:)
    real(real32), allocatable :: bad(:)
    logical :: errc

    x = any_vector([5.0_real64, 7.0_real64])  ! constructor

    call x%get_value(val, errc)
    if (errc) call write_fail('test_set_get_real64 failed test 1')
    if (.not.allocated(val)) then
      call write_fail('test_set_get_real64 failed test 1b')
    else
      if (size(val) /= 2) then
        call write_fail('test_set_get_real64 failed test 1c')
      else
        if (any(val /= [5.0,7.0])) call write_fail('test_set_get_real64 failed test 2')
      end if
    end if
    call x%get_value(bad, errc)
    if (.not.errc) call write_fail('test_set_get_real64 failed test 3')

    call x%set_value([11.0_real64]) ! overwrite
    call x%get_value(val, errc)
    if (errc) call write_fail('test_set_get_real64 failed test 4')
    if (.not.allocated(val)) then
      call write_fail('test_set_get_real64 failed test 4b')
    else
      if (size(val) /= 1) then
        call write_fail('test_set_get_real64 failed test 4c')
      else
        if (any(val /= [11.0])) call write_fail('test_set_get_real64 failed test 5')
      end if
    end if

  end subroutine

  subroutine test_set_get_cmplx32

    type(any_vector) :: x
    complex(real32), allocatable :: val(:), src(:)
    complex(real64), allocatable :: bad(:)
    logical :: errc

    src = [cmplx(1,2,kind=real32), cmplx(3,4,kind=real32)]
    x = any_vector(src)  ! constructor

    call x%get_value(val, errc)
    if (errc) call write_fail('test_set_get_cmplx32 failed test 1')
    if (.not.allocated(val)) then
      call write_fail('test_set_get_cmplx32 failed test 1b')
    else
      if (size(val) /= 2) then
        call write_fail('test_set_get_cmplx32 failed test 1c')
      else
        if (any(val /= src)) call write_fail('test_set_get_cmplx32 failed test 2')
      end if
    end if
    call x%get_value(bad, errc)
    if (.not.errc) call write_fail('test_set_get_cmplx32 failed test 3')

    src = [cmplx(5,6,kind=real32)]
    call x%set_value(src) ! overwrite
    call x%get_value(val, errc)
    if (errc) call write_fail('test_set_get_cmplx32 failed test 4')
    if (.not.allocated(val)) then
      call write_fail('test_set_get_cmplx32 failed test 4b')
    else
      if (size(val) /= 1) then
        call write_fail('test_set_get_cmplx32 failed test 4c')
      else
        if (any(val /= src)) call write_fail('test_set_get_cmplx32 failed test 5')
      end if
    end if

  end subroutine

  subroutine test_set_get_cmplx64

    type(any_vector) :: x
    complex(real64), allocatable :: val(:), src(:)
    complex(real32), allocatable :: bad(:)
    logical :: errc

    src = [cmplx(1,2,kind=real64), cmplx(3,4,kind=real64)]
    x = any_vector(src)  ! constructor

    call x%get_value(val, errc)
    if (errc) call write_fail('test_set_get_cmplx64 failed test 1')
    if (.not.allocated(val)) then
      call write_fail('test_set_get_cmplx64 failed test 1b')
    else
      if (size(val) /= 2) then
        call write_fail('test_set_get_cmplx64 failed test 1c')
      else
        if (any(val /= src)) call write_fail('test_set_get_cmplx64 failed test 2')
      end if
    end if
    call x%get_value(bad, errc)
    if (.not.errc) call write_fail('test_set_get_cmplx64 failed test 3')

    src = [cmplx(5,6,kind=real64)]
    call x%set_value(src) ! overwrite
    call x%get_value(val, errc)
    if (errc) call write_fail('test_set_get_cmplx64 failed test 4')
    if (.not.allocated(val)) then
      call write_fail('test_set_get_cmplx64 failed test 4b')
    else
      if (size(val) /= 1) then
        call write_fail('test_set_get_cmplx64 failed test 4c')
      else
        if (any(val /= src)) call write_fail('test_set_get_cmplx64 failed test 5')
      end if
    end if

  end subroutine

  subroutine test_set_get_logical

    type(any_vector) :: x
    logical, allocatable :: val(:)
    integer, allocatable :: bad(:)
    logical :: errc

    x = any_vector([.true.,.false.])  ! constructor

    call x%get_value(val, errc)
    if (errc) call write_fail('test_set_get_logical failed test 1')
    if (.not.allocated(val)) then
      call write_fail('test_set_get_logical failed test 1b')
    else
      if (size(val) /= 2) then
        call write_fail('test_set_get_logical failed test 1c')
      else
        if (any(val .neqv. [.true.,.false.])) call write_fail('test_set_get_logical failed test 2')
      end if
    end if
    call x%get_value(bad, errc)
    if (.not.errc) call write_fail('test_set_get_logical failed test 3')

    call x%set_value([.false.]) ! overwrite
    call x%get_value(val, errc)
    if (errc) call write_fail('test_set_get_logical failed test 4')
    if (.not.allocated(val)) then
      call write_fail('test_set_get_logical failed test 4b')
    else
      if (size(val) /= 1) then
        call write_fail('test_set_get_logical failed test 4c')
      else
        if (any(val .neqv. [.false.])) call write_fail('test_set_get_logical failed test 5')
      end if
    end if

  end subroutine

  subroutine test_set_get_character

    type(any_vector) :: x
    character(:), allocatable :: val(:)
    integer, allocatable :: bad(:)
    logical :: errc

    x = any_vector(['foo','bar'])  ! constructor

    call x%get_value(val, errc)
    if (errc) call write_fail('test_set_get_character failed test 1')
    if (.not.allocated(val)) then
      call write_fail('test_set_get_character failed test 1b')
    else
      if (size(val) /= 2) then
        call write_fail('test_set_get_character failed test 1c')
      else
        if (any(val /= ['foo','bar'])) call write_fail('test_set_get_character failed test 2')
      end if
    end if
    call x%get_value(bad, errc)
    if (.not.errc) call write_fail('test_set_get_character failed test 3')

    call x%set_value(['fubar']) ! overwrite
    call x%get_value(val, errc)
    if (errc) call write_fail('test_set_get_character failed test 4')
    if (.not.allocated(val)) then
      call write_fail('test_set_get_character failed test 4b')
    else
      if (size(val) /= 1) then
        call write_fail('test_set_get_character failed test 4c')
      else
        if (any(val /= ['fubar'])) call write_fail('test_set_get_character failed test 5')
      end if
    end if

  end subroutine

  subroutine test_get_value

    type(any_vector) :: x
    class(*), allocatable :: val(:)

    call x%set_value([1,2,3])
    call x%get_value(val)
    if (.not.allocated(val)) then
      call write_fail('test_get_value failed test 1b')
    else
      if (size(val) /= 3) then
        call write_fail('test_get_value failed test 1c')
      else
        select type (val)
        type is (integer)
          if (any(val /= [1,2,3])) call write_fail('test_get_value failed test 1')
        class default
          call write_fail('test_get_value failed test 2')
        end select
      end if
    end if

    call x%set_value([1.0,3.14])
    call x%get_value(val)
    if (.not.allocated(val)) then
      call write_fail('test_get_value failed test 3b')
    else
      if (size(val) /= 2) then
        call write_fail('test_get_value failed test 3c')
      else
        select type (val)
        type is (real)
          if (any(val /= [1.0,3.14])) call write_fail('test_get_value failed test 3')
        class default
          call write_fail('test_get_value failed test 4')
        end select
      end if
    end if

    call x%set_value(['biz','bat'])
    call x%get_value(val)
    if (.not.allocated(val)) then
      call write_fail('test_get_value failed test 5b')
    else
      if (size(val) /= 2) then
        call write_fail('test_get_value failed test 5c')
      else
        select type (val)
        type is (character(*))
          if (any(val /= ['biz','bat'])) call write_fail('test_get_value failed test 5')
        class default
          call write_fail('test_get_value failed test 6')
        end select
      end if
    end if

  end subroutine

  subroutine test_value_ptr

    type(any_vector), target :: x
    class(*), pointer :: val(:)

    call x%set_value([1,2,3])
    val => x%value_ptr()
    select type (val)
    type is (integer)
      if (any(val /= [1,2,3])) call write_fail('test_value_ptr failed test 1')
    class default
      call write_fail('test_value_ptr failed test 2')
    end select

    call x%set_value([1.0,3.14])
    val => x%value_ptr()
    select type (val)
    type is (real)
      if (any(val /= [1.0,3.14])) call write_fail('test_value_ptr failed test 3')
    class default
      call write_fail('test_value_ptr failed test 4')
    end select

    call x%set_value(['biz','bat'])
    val => x%value_ptr()
    select type (val)
    type is (character(*))
      if (any(val /= ['biz','bat'])) call write_fail('test_get_value failed test 5')
    class default
      call write_fail('test_get_value failed test 6')
    end select

  end subroutine

  subroutine test_derived_type_value

    type(any_vector) :: x
    class(*), allocatable :: val(:)
    class(*), pointer :: ptr(:)

    type point
      real x, y
    end type

    x = any_vector([point(1.0,2.0),point(3.0,4.0)])
    call x%get_value(val)
    select type (val)
    type is (point)
      if (any(val%x /= [1.0,3.0]) .or. any(val%y /= [2.0,4.0])) call write_fail('test_derived_type_value failed test 1')
    class default
      call write_fail('test_derived_type_value failed test 2')
    end select

    call x%set_value([point(0.0,0.0)])
    ptr => x%value_ptr()
    select type (ptr)
    type is (point)
      if (ptr(1)%x /= 0.0 .or. ptr(1)%y /= 0.0) call write_fail('test_derived_type_value failed test 3')
    class default
      call write_fail('test_derived_type_value failed test 4')
    end select

  end subroutine

  subroutine test_assignment

    type(any_vector) :: x, y
    integer, allocatable :: ival(:)
    real, allocatable :: rval(:)
    character(:), allocatable :: cval(:)
    logical :: errc
    class(*), allocatable :: val(:)

    type point
      real x, y
    end type

    !! Intrinsic type values
    call x%set_value([1,2])
    y = x
    call x%set_value([3.14])
    call y%get_value(ival, errc)
    if (errc) call write_fail('test_assignment failed test 1')
    if (any(ival /= [1,2])) call write_fail('test_assignment failed test 2')
    call x%get_value(rval, errc)
    if (errc) call write_fail('test_assignment failed test 3')
    if (any(rval /= [3.14])) call write_fail('test_assignment failed test 4')

    !! Simple derived type values
    call x%set_value([point(1.0,2.0)])
    y = x
    call x%set_value([1.0,3.14])
    call y%get_value(val)
    select type (val)
    type is (point)
      if (val(1)%x /= 1.0 .or. val(1)%y /= 2.0) call write_fail('test_assignment failed test 5')
    class default
      call write_fail('test_assignment failed test 6')
    end select
    call x%get_value(rval, errc)
    if (errc) call write_fail('test_assignment failed test 7')
    if (any(rval /= [1.0,3.14])) call write_fail('test_assignment failed test 4')

    !! Character type values
    call x%set_value(['foo','bar'])
    y = x
    call x%set_value([3.14])
    call y%get_value(cval, errc)
    if (errc) call write_fail('test_assignment failed test 9')
    if (any(cval /= ['foo','bar'])) call write_fail('test_assignment failed test 10')
    call x%get_value(rval, errc)
    if (errc) call write_fail('test_assignment failed test 11')
    if (any(rval /= [3.14])) call write_fail('test_assignment failed test 12')

  end subroutine

  subroutine test_shallow_assignment

    type(any_vector) :: x, y
    class(*), allocatable :: xval(:), yval(:)
    integer, pointer :: xp1, yp1, xp2, yp2

    type box
      integer, pointer :: n => null()
    end type
    type(box) :: b(2)

    allocate(b(1)%n, b(2)%n)
    b(1)%n = 1
    b(2)%n = 2

    call x%set_value(b) ! x value is shallow copy of b
    y = x ! y value is a shallow copy of x

    call x%get_value(xval)
    select type (xval)
    type is (box)
      xp1 => xval(1)%n
      xp2 => xval(2)%n
    end select

    call y%get_value(yval)
    select type (yval)
    type is (box)
      yp1 => yval(1)%n
      yp2 => yval(2)%n
    end select

    if (.not.associated(xp1,yp1))  call write_fail('test_shallow_assignment test 1 failed')
    if (.not.associated(xp1,b(1)%n)) call write_fail('test_shallow_assignment test 2 failed')
    if (.not.associated(xp2,yp2))  call write_fail('test_shallow_assignment test 3 failed')
    if (.not.associated(xp2,b(2)%n)) call write_fail('test_shallow_assignment test 4 failed')
    
    deallocate(b(1)%n,b(2)%n) ! clean-up in case we use a memory checker

  end subroutine

  subroutine write_fail(errmsg)
    use,intrinsic :: iso_fortran_env, only: error_unit
    character(*), intent(in) :: errmsg
    stat = 1
    write(error_unit,'(a)') errmsg
  end subroutine

end program test_any_vector_type
