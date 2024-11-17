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

program test_parameter_list_type

  use parameter_list_type
  use,intrinsic :: iso_fortran_env, only: int32, int64, real32, real64

  integer :: stat = 0

  call test_basic
  call get_scalar
  call get_vector
  call get_matrix
  call test_get_any
  call test_overwrite
  call test_sublists
  call test_iterator
  call test_path
  call test_assignment

  if (stat /= 0) stop 1

contains

  subroutine write_result(pass, name)
    use,intrinsic :: iso_fortran_env, only: output_unit
    logical, value :: pass
    character(*), intent(in) :: name
    if (pass) then
      write(output_unit,'(a)') 'Passed: ' // name
    else
      stat = 1
      write(output_unit,'(a)') 'FAILED: ' // name
    end if
  end subroutine

 !!
 !! A basic test that we can populate a parameter list with simple parameters.
 !! Tests the count, is_parameter, is_sublist, is_scalar, is_vector, is_matrix
 !! methods.
 !!

  subroutine test_basic

    type(parameter_list) :: p
    integer ::stat
    character(:), allocatable :: errmsg

    !! A derived type
    type point; real x, y; end type

    !! Check that the initial list is empty.
    if (p%count() /= 0) call write_fail('test_basic failed test 1')

    !! Define parameters of various types and ranks,
    !! checking the parameter count each time.
    call p%set('foo', 1)
    if (p%count() /= 1) call write_fail('test_basic failed test 2')
    call p%set('bar', [1.0,3.14])
    if (p%count() /= 2) call write_fail('test_basic failed test 3')
    call p%set('wat', ['biz','bat'])
    if (p%count() /= 3) call write_fail('test_basic failed test 4')
    call p%set('bah', reshape([1,2,3,4],shape=[2,2]))
    if (p%count() /= 4) call write_fail('test_basic failed test 5')

    !! Check that they are recognized as parameters, and of the correct type.
    if (.not.p%is_parameter('foo')) call write_fail('test_basic failed test 6')
    if (p%is_sublist('foo')) call write_fail('test_basic failed test 7')
    if (.not.p%is_scalar('foo')) call write_fail('test_basic failed test 8')
    if (p%is_vector('foo')) call write_fail('test_basic failed test 9')
    if (p%is_matrix('foo')) call write_fail('test_basic failed test 10')
    if (.not.p%is_parameter('bar')) call write_fail('test_basic failed test 1')
    if (p%is_sublist('bar')) call write_fail('test_basic failed test 12')
    if (p%is_scalar('bar')) call write_fail('test_basic failed test 13')
    if (.not.p%is_vector('bar')) call write_fail('test_basic failed test 14')
    if (p%is_matrix('bar')) call write_fail('test_basic failed test 15')
    if (.not.p%is_parameter('wat')) call write_fail('test_basic failed test 16')
    if (p%is_sublist('wat')) call write_fail('test_basic failed test 17')
    if (.not.p%is_parameter('bah')) call write_fail('test_basic failed test 18')
    if (p%is_sublist('bah')) call write_fail('test_basic failed test 19')
    if (p%is_scalar('bah')) call write_fail('test_basic failed test 20')
    if (p%is_vector('bah')) call write_fail('test_basic failed test 21')
    if (.not.p%is_matrix('bah')) call write_fail('test_basic failed test 22')

    !! Replace the value of a parameter; different rank -- should fail
    call p%set('foo', [1,2], stat=stat, errmsg=errmsg)
    if (stat == 0) call write_fail('test_basic failed test 23')
    if (p%count() /= 4) call write_fail('test_basic failed test 24')

    !! Replace the value of a parameter; same rank -- should succeed.
    call p%set('bar', [1,2], stat=stat, errmsg=errmsg)
    if (stat /= 0) call write_fail('test_basic failed test 25')
    if (p%count() /= 4) call write_fail('test_basic failed test 26')

    !! Verify that a non-existant parameter does not exist.
    if (p%is_parameter('dummy')) call write_fail('test_basic failed test 27')
    if (p%is_sublist('dummy')) call write_fail('test_basic failed test 28')
    if (p%is_scalar('dummy')) call write_fail('test_basic failed test 29')
    if (p%is_vector('dummy')) call write_fail('test_basic failed test 30')
    if (p%is_matrix('dummy')) call write_fail('test_basic failed test 31')

  end subroutine

 !!
 !! A thorough test of set/get for all the specific intrinsic types.
 !!

  subroutine get_scalar

    block ! 32-bit integer
      type(parameter_list) :: p
      integer(int32) :: ref, def, x, y
      ref = 1
      def = 2
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(x == ref, 'get_scalar1_int32')
      call p%get('foo', y, default=def)
      call write_result(y == ref, 'get_scalar2_int32')
      call p%get('bar', x, default=def)
      call write_result(x == def, 'get_scalar3_int32')
    end block

    block ! 64-bit integer
      type(parameter_list) :: p
      integer(int64) :: ref, def, x, y
      ref = 1
      def = 2
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(x == ref, 'get_scalar1_int64')
      call p%get('foo', y, default=def)
      call write_result(y == ref, 'get_scalar2_int64')
      call p%get('bar', x, default=def)
      call write_result(x == def, 'get_scalar3_int64')
    end block

    block ! 32-bit real
      type(parameter_list) :: p
      real(real32) :: ref, def, x, y
      ref = 1
      def = 2
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(x == ref, 'get_scalar1_real32')
      call p%get('foo', y, default=def)
      call write_result(y == ref, 'get_scalar2_real32')
      call p%get('bar', x, default=def)
      call write_result(x == def, 'get_scalar3_real32')
    end block

    block ! 64-bit real
      type(parameter_list) :: p
      real(real64) :: ref, def, x, y
      ref = 1
      def = 2
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(x == ref, 'get_scalar1_real64')
      call p%get('foo', y, default=def)
      call write_result(y == ref, 'get_scalar2_real64')
      call p%get('bar', x, default=def)
      call write_result(x == def, 'get_scalar3_real64')
    end block

    block ! 32-bit complex
      type(parameter_list) :: p
      complex(real32) :: ref, def, x, y
      ref = cmplx(1,-1)
      def = cmplx(2,-2)
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(x == ref, 'get_scalar1_complex32')
      call p%get('foo', y, default=def)
      call write_result(y == ref, 'get_scalar2_complex32')
      call p%get('bar', x, default=def)
      call write_result(x == def, 'get_scalar3_complex32')
    end block

    block ! 64-bit complex
      type(parameter_list) :: p
      complex(real64) :: ref, def, x, y
      ref = cmplx(1,-1)
      def = cmplx(2,-2)
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(x == ref, 'get_scalar1_complex64')
      call p%get('foo', y, default=def)
      call write_result(y == ref, 'get_scalar2_complex64')
      call p%get('bar', x, default=def)
      call write_result(x == def, 'get_scalar3_complex64')
    end block
    
    block ! default logical
      type(parameter_list) :: p
      logical :: ref, def, x, y
      ref = .true.
      def = .false.
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(x .eqv. ref, 'get_scalar1_logical')
      call p%get('foo', y, default=def)
      call write_result(y .eqv. ref, 'get_scalar2_logical')
      call p%get('bar', x, default=def)
      call write_result(x .eqv. def, 'get_scalar3_logical')
    end block
    
    block ! default character
      type(parameter_list) :: p
      character(:), allocatable :: ref, def, x, y
      ref = 'biz'
      def = 'bat'
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(x == ref, 'get_scalar1_char')
      call p%get('foo', y, default=def)
      call write_result(y == ref, 'get_scalar2_char')
      call p%get('bar', x, default=def)
      call write_result(x == def, 'get_scalar3_char')
    end block

  end subroutine get_scalar


  subroutine get_vector

    integer, allocatable :: a(:), b(:)

    a = [1, 2]
    b = [3]

    block ! 32-bit integer
      type(parameter_list) :: p
      integer(int32), allocatable, dimension(:) :: ref, def, x, y
      ref = a
      def = b
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(all(x == ref), 'get_vector1_int32')
      call p%get('foo', y, default=def)
      call write_result(all(y == ref), 'get_vector2_int32')
      call p%get('bar', x, default=def)
      call write_result(all(x == def), 'get_vector3_int32')
    end block

    block ! 64-bit integer
      type(parameter_list) :: p
      integer(int64), allocatable, dimension(:) :: ref, def, x, y
      ref = a
      def = b
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(all(x == ref), 'get_vector1_int64')
      call p%get('foo', y, default=def)
      call write_result(all(y == ref), 'get_vector2_int64')
      call p%get('bar', x, default=def)
      call write_result(all(x == def), 'get_vector3_int64')
    end block

    block ! 32-bit real
      type(parameter_list) :: p
      real(real32), allocatable, dimension(:) :: ref, def, x, y
      ref = a
      def = b
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(all(x == ref), 'get_vector1_real32')
      call p%get('foo', y, default=def)
      call write_result(all(y == ref), 'get_vector2_real32')
      call p%get('bar', x, default=def)
      call write_result(all(x == def), 'get_vector3_real32')
    end block

    block ! 64-bit real
      type(parameter_list) :: p
      real(real64), allocatable, dimension(:) :: ref, def, x, y
      ref = a
      def = b
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(all(x == ref), 'get_vector1_real64')
      call p%get('foo', y, default=def)
      call write_result(all(y == ref), 'get_vector2_real64')
      call p%get('bar', x, default=def)
      call write_result(all(x == def), 'get_vector3_real64')
    end block

    block ! 32-bit complex
      type(parameter_list) :: p
      complex(real32), allocatable, dimension(:) :: ref, def, x, y
      ref = a*cmplx(1,-1)
      def = b*cmplx(1,-1)
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(all(x == ref), 'get_vector1_complex32')
      call p%get('foo', y, default=def)
      call write_result(all(y == ref), 'get_vector2_complex32')
      call p%get('bar', x, default=def)
      call write_result(all(x == def), 'get_vector3_complex32')
    end block

    block ! 64-bit complex
      type(parameter_list) :: p
      complex(real64), allocatable, dimension(:) :: ref, def, x, y
      ref = a*cmplx(1,-1)
      def = b*cmplx(1,-1)
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(all(x == ref), 'get_vector1_complex64')
      call p%get('foo', y, default=def)
      call write_result(all(y == ref), 'get_vector2_complex64')
      call p%get('bar', x, default=def)
      call write_result(all(x == def), 'get_vector3_complex64')
    end block
    
    block ! default logical
      type(parameter_list) :: p
      logical, allocatable, dimension(:) :: ref, def, x, y
      ref = [.true., .false.]
      def = [.false.]
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(all(x .eqv. ref), 'get_vector1_logical')
      call p%get('foo', y, default=def)
      call write_result(all(y .eqv. ref), 'get_vector2_logical')
      call p%get('bar', x, default=def)
      call write_result(all(x .eqv. def), 'get_vector3_logical')
    end block
    
    block ! default character
      type(parameter_list) :: p
      character(:), allocatable, dimension(:) :: ref, def, x, y
      ref = ['biz', 'fiz']
      def = ['bat']
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(all(x == ref), 'get_vector1_char')
      call p%get('foo', y, default=def)
      call write_result(all(y == ref), 'get_vector2_char')
      call p%get('bar', x, default=def)
      call write_result(all(x == def), 'get_vector3_char')
    end block

  end subroutine get_vector


  subroutine get_matrix

    integer, allocatable :: a(:,:), b(:,:)

    a = reshape([1, 2, 3, 4, 5, 6], shape=[2, 3])
    b = -transpose(a)

    block ! 32-bit integer
      type(parameter_list) :: p
      integer(int32), allocatable, dimension(:,:) :: ref, def, x, y
      ref = a
      def = b
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(all(x == ref), 'get_matrix1_int32')
      call p%get('foo', y, default=def)
      call write_result(all(y == ref), 'get_matrix2_int32')
      call p%get('bar', x, default=def)
      call write_result(all(x == def), 'get_matrix3_int32')
    end block

    block ! 64-bit integer
      type(parameter_list) :: p
      integer(int64), allocatable, dimension(:,:) :: ref, def, x, y
      ref = a
      def = b
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(all(x == ref), 'get_matrix1_int64')
      call p%get('foo', y, default=def)
      call write_result(all(y == ref), 'get_matrix2_int64')
      call p%get('bar', x, default=def)
      call write_result(all(x == def), 'get_matrix3_int64')
    end block

    block ! 32-bit real
      type(parameter_list) :: p
      real(real32), allocatable, dimension(:,:) :: ref, def, x, y
      ref = a
      def = b
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(all(x == ref), 'get_matrix1_real32')
      call p%get('foo', y, default=def)
      call write_result(all(y == ref), 'get_matrix2_real32')
      call p%get('bar', x, default=def)
      call write_result(all(x == def), 'get_matrix3_real32')
    end block

    block ! 64-bit real
      type(parameter_list) :: p
      real(real64), allocatable, dimension(:,:) :: ref, def, x, y
      ref = a
      def = b
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(all(x == ref), 'get_matrix1_real64')
      call p%get('foo', y, default=def)
      call write_result(all(y == ref), 'get_matrix2_real64')
      call p%get('bar', x, default=def)
      call write_result(all(x == def), 'get_matrix3_real64')
    end block

    block ! 32-bit complex
      type(parameter_list) :: p
      complex(real32), allocatable, dimension(:,:) :: ref, def, x, y
      ref = a*cmplx(1,-1)
      def = b*cmplx(1,-1)
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(all(x == ref), 'get_matrix1_complex32')
      call p%get('foo', y, default=def)
      call write_result(all(y == ref), 'get_matrix2_complex32')
      call p%get('bar', x, default=def)
      call write_result(all(x == def), 'get_matrix3_complex32')
    end block

    block ! 64-bit complex
      type(parameter_list) :: p
      complex(real64), allocatable, dimension(:,:) :: ref, def, x, y
      ref = a*cmplx(1,-1)
      def = b*cmplx(1,-1)
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(all(x == ref), 'get_matrix1_complex64')
      call p%get('foo', y, default=def)
      call write_result(all(y == ref), 'get_matrix2_complex64')
      call p%get('bar', x, default=def)
      call write_result(all(x == def), 'get_matrix3_complex64')
    end block
    
    block ! default logical
      type(parameter_list) :: p
      logical, allocatable, dimension(:,:) :: ref, def, x, y
      ref = reshape([.true., .false.], shape=[1,2])
      def = reshape([.false.], shape=[1,1])
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(all(x .eqv. ref), 'get_matrix1_logical')
      call p%get('foo', y, default=def)
      call write_result(all(y .eqv. ref), 'get_matrix2_logical')
      call p%get('bar', x, default=def)
      call write_result(all(x .eqv. def), 'get_matrix3_logical')
    end block
    
    block ! default character
      type(parameter_list) :: p
      character(:), allocatable, dimension(:,:) :: ref, def, x, y
      ref = reshape(['biz', 'fiz', 'wiz'], shape=[1,2])
      def = reshape(['bat'], shape=[1,1])
      call p%set('foo', ref)
      call p%get('foo', x)
      call write_result(all(x == ref), 'get_matrix1_char')
      call p%get('foo', y, default=def)
      call write_result(all(y == ref), 'get_matrix2_char')
      call p%get('bar', x, default=def)
      call write_result(all(x == def), 'get_matrix3_char')
    end block

  end subroutine get_matrix

 !!
 !! Test the get_any methods
 !!

  subroutine test_get_any

    type(parameter_list) :: p

    type point; real x, y; end type
    class(*), allocatable :: scalar, vector(:), matrix(:,:)

    !! Define parameters using the provided default value.

    call p%get_any('a', scalar, default=1)
    select type (scalar)
    type is (integer)
      if (scalar /= 1) call write_fail('test_get_any failed test 1')
    class default
      call write_fail('test_get_any failed test 2')
    end select

    call p%get_any('b', vector, default=[1.0,2.0])
    select type (vector)
    type is (real)
      if (any(vector /= [1,2])) call write_fail('test_get_any failed test 3')
    class default
      call write_fail('test_get_any failed test 4')
    end select

    call p%get_any('bm', matrix, default=reshape([1.0,2.0],shape=[2,1]))
    select type (matrix)
    type is (real)
      if (any(matrix(:,1) /= [1,2])) call write_fail('test_get_any failed test 3m')
    class default
      call write_fail('test_get_any failed test 4m')
    end select

    call p%get_any('c', scalar, default=point(1.0,2.0))
    select type (scalar)
    type is (point)
      if (scalar%x /= 1 .or. scalar%y /= 2) call write_fail('test_get_any failed test 5')
    class default
      call write_fail('test_get_any failed test 6')
    end select

    call p%get_any('d', vector, default=['foo','bar'])
    select type (vector)
    type is (character(*))
      if (any(vector /= ['foo','bar'])) call write_fail('test_get_any failed test 7')
    class default
      call write_fail('test_get_any failed test 8')
    end select

    call p%get_any('dm', matrix, default=reshape(['foo','bar'],shape=[1,2]))
    select type (matrix)
    type is (character(*))
      if (any(matrix(1,:) /= ['foo','bar'])) call write_fail('test_get_any failed test 7m')
    class default
      call write_fail('test_get_any failed test 8m')
    end select

    !call p%set('e', [point(1.0,2.0)])
    call p%get_any('e', vector, default=[point(1.0,2.0)])
    select type (vector)
    type is (point)
      if (vector(1)%x /= 1.0 .or. vector(1)%y /= 2.0) call write_fail('test_get_any failed test 9')
    class default
      call write_fail('test_get_any failed test 10')
    end select

    call p%get_any('em', matrix, default=reshape([point(1.0,2.0)],shape=[1,1]))
    select type (matrix)
    type is (point)
      if (matrix(1,1)%x /= 1.0 .or. matrix(1,1)%y /= 2.0) call write_fail('test_get_any failed test 9m')
    class default
      call write_fail('test_get_any failed test 10m')
    end select

    !! Get them again with different default values that should be ignored.

    call p%get_any('a', scalar, default=0)
    select type (scalar)
    type is (integer)
      if (scalar /= 1) call write_fail('test_get_any failed test 11')
    class default
      call write_fail('test_get_any failed test 12')
    end select

    call p%get_any('b', vector, default=[0.0])
    select type (vector)
    type is (real)
      if (any(vector /= [1,2])) call write_fail('test_get_any failed test 13')
    class default
      call write_fail('test_get_any failed test 14')
    end select

    call p%get_any('bm', matrix, default=reshape([0.0],shape=[1,1]))
    select type (matrix)
    type is (real)
      if (any(matrix(:,1) /= [1,2])) call write_fail('test_get_any failed test 13m')
    class default
      call write_fail('test_get_any failed test 14m')
    end select

    call p%get_any('c', scalar, default="fubar")
    select type (scalar)
    type is (point)
      if (scalar%x /= 1 .or. scalar%y /= 2) call write_fail('test_get_any failed test 15')
    class default
      call write_fail('test_get_any failed test 16')
    end select

    call p%get_any('d', vector, default=[point(1.0,2.0)])
    select type (vector)
    type is (character(*))
      if (any(vector /= ['foo','bar'])) call write_fail('test_get_any failed test 17')
    class default
      call write_fail('test_get_any failed test 18')
    end select

    call p%get_any('dm', matrix, default=reshape([point(1.0,2.0)],shape=[1,1]))
    select type (matrix)
    type is (character(*))
      if (any(matrix(1,:) /= ['foo','bar'])) call write_fail('test_get_any failed test 17m')
    class default
      call write_fail('test_get_any failed test 18m')
    end select

    call p%get_any('e', vector, default=[13])
    select type (vector)
    type is (point)
      if (vector(1)%x /= 1.0 .or. vector(1)%y /= 2.0) call write_fail('test_get_any failed test 19')
    class default
      call write_fail('test_get_any failed test 20')
    end select

    call p%get_any('em', matrix, default=reshape([13],shape=[1,1]))
    select type (matrix)
    type is (point)
      if (matrix(1,1)%x /= 1.0 .or. matrix(1,1)%y /= 2.0) call write_fail('test_get_any failed test 19m')
    class default
      call write_fail('test_get_any failed test 20m')
    end select

  end subroutine

 !!
 !! Test the overwriting of parameter values
 !!

  subroutine test_overwrite

    type(parameter_list) :: p
    real :: r
    real, allocatable :: rarray(:), rmatrix(:,:)
    character(:), allocatable :: c, carray(:), cmatrix(:,:)
    type point; real x, y; end type
    integer :: stat
    character(:), allocatable :: errmsg
    class(*), allocatable :: scalar, vector(:), matrix(:,:)

    call p%set('foo', 13)

    !! Overwrite with different rank; should fail
    call p%set('foo', [1], stat=stat, errmsg=errmsg)
    if (stat == 0) call write_fail('test_overwrite failed test 1')

    !! Overwrite with different values/types.
    call p%set('foo', 11.0)
    call p%get('foo', r)
    if (r /= 11.0) call write_fail('test_overwrite failed test 2')
    call p%set('foo', 'blah')
    call p%get('foo', c)
    if (c /= 'blah') call write_fail('test_overwrite failed test 3')
    call p%set('foo', point(1.0, 2.0))
    call p%get_any('foo', scalar)
    select type (scalar)
    type is (point)
      if (scalar%x /= 1 .or. scalar%y /= 2) call write_fail('test_overwrite failed test 4')
    class default
      call write_fail('test_overwrite failed test 5')
    end select

    call p%set('bar', [13])

    !! Overwrite with different rank; should fail
    call p%set('bar', 1, stat=stat, errmsg=errmsg)
    if (stat == 0) call write_fail('test_overwrite failed test 6')

    !! Overwrite with different values/types.
    call p%set('bar', [11.0])
    call p%get('bar', rarray)
    if (any(rarray /= [11.0])) call write_fail('test_overwrite failed test 7')
    call p%set('bar', ['blah','blah'])
    call p%get('bar', carray)
    if (any(carray /= ['blah','blah'])) call write_fail('test_overwrite failed test 8')
    call p%set('bar', [point(1.0, 2.0)])
    call p%get_any('bar', vector)
    select type (vector)
    type is (point)
      if (vector(1)%x /= 1 .or. vector(1)%y /= 2) call write_fail('test_overwrite failed test 9')
    class default
      call write_fail('test_overwrite failed test 10')
    end select

    call p%set('biz', reshape([13],shape=[1,1]))

    !! Overwrite with different rank; should fail
    call p%set('biz', [1], stat=stat, errmsg=errmsg)
    if (stat == 0) call write_fail('test_overwrite failed test 11')

    !! Overwrite with different values/types.
    call p%set('biz', reshape([11.0],shape=[1,1]))
    call p%get('biz', rmatrix)
    if (rmatrix(1,1) /= 11.0) call write_fail('test_overwrite failed test 12')
    call p%set('biz', reshape(['blah','blah'],shape=[2,1]))
    call p%get('biz', cmatrix)
    if (any(cmatrix(:,1) /= ['blah','blah'])) call write_fail('test_overwrite failed test 13')
    call p%set('biz', reshape([point(1.0, 2.0)],shape=[1,1]))
    call p%get_any('biz', matrix)
    select type (matrix)
    type is (point)
      if (matrix(1,1)%x /= 1 .or. matrix(1,1)%y /= 2) call write_fail('test_overwrite failed test 14')
    class default
      call write_fail('test_overwrite failed test 15')
    end select

  end subroutine

 !!
 !! Tests the creation and access to sublists
 !!

  subroutine test_sublists

    type(parameter_list) :: p
    type(parameter_list), pointer :: sl, sla, slb
    integer :: stat
    character(:), allocatable :: errmsg

    !! Create a sublist parameter and add a parameter to the sublist.
    sla => p%sublist('A')
    if (.not.associated(sla)) call write_fail('test_sublists failed test 1')
    if (p%count() /= 1) call write_fail('test_sublists failed test 2')
    call sla%set('foo', 42)

    !! 'A' should be recognized as both a parameter and a sublist.
    if (.not.p%is_sublist('A')) call write_fail('test_sublists failed test 3')
    if (.not.p%is_parameter('A')) call write_fail('test_sublists failed test 4')

    !! Try to use sublist with an existing non-sublist parameter; should fail.
    slb => sla%sublist('foo', stat, errmsg=errmsg)
    if (stat == 0) call write_fail('test_sublists failed test 5')

    !! Create a sublist parameter of the sublist.
    slb => sla%sublist('B')
    if (.not.associated(slb)) call write_fail('test_sublists failed test 6')
    if (sla%count() /= 2) call write_fail('test_sublists failed test 7')
    if (.not.sla%is_sublist('B')) call write_fail('test_sublists failed test 8')
    if (.not.sla%is_parameter('B')) call write_fail('test_sublists failed test 9')

    !! Access the 'A' sublist again and verify it is the same.
    sl => p%sublist('A')
    if (.not.associated(sl,sla)) call write_fail('test_sublists failed test 5')

  end subroutine

 !!
 !! Test the parameter list iterator
 !!

  subroutine test_iterator

    type(parameter_list) :: p
    type(parameter_list), pointer :: sl, sl2
    type(parameter_list_iterator) :: piter

    integer :: j
    type point; real x, y; end type
    class(parameter_value), pointer :: pval
    class(*), pointer :: scalar, scalar1, vector(:), vector1(:), matrix(:,:), matrix1(:,:)

    !! Populate a parameter list.
    call p%set('integer', 1)
    call p%set('real', [2.0])
    call p%set('matrix', reshape([1,2,3,4,5,6],shape=[2,3]))
    sl => p%sublist ('sublist')
    call p%set('string', 'hello')
    call p%set('point', point(1.0,2.0))

    !! Walk the list.
    piter = parameter_list_iterator(p)
    if (piter%count() /= 6) call write_fail('test_iterator failed test 1')
    do j = piter%count(), 1, -1
      if (piter%at_end()) call write_fail('test_iterator failed test 2')
      if (piter%count() /= j) call write_fail('test_iterator failed test 3')
      call piter%next()
    end do
    if (piter%count() /= 0) call write_fail('test_iterator failed test 4')
    if (.not.piter%at_end()) call write_fail('test_iterator failed test 5')

    !! Walk the list again and check values this time.
    piter = parameter_list_iterator(p)
    do while (.not.piter%at_end())
      pval => piter%value()
      select type (pval)
      type is (any_scalar)
        if (.not.piter%is_scalar()) call write_fail('test_iterator failed test 6')
        if (piter%is_sublist()) call write_fail('test_iterator failed test 7')
        if (piter%is_vector()) call write_fail('test_iterator failed test 8')
        if (piter%is_matrix()) call write_fail('test_iterator failed test m1')
        scalar1 => piter%scalar()
        scalar => pval%value_ptr()
        if (.not.associated(scalar,scalar1)) call write_fail('test_iterator failed test A')
        select type (scalar)
        type is (integer)
          if (piter%name() /= 'integer') call write_fail('test_iterator failed test 9')
          if (scalar /= 1) call write_fail('test_iterator failed test 10')
        type is (character(*))
          if (piter%name() /= 'string') call write_fail('test_iterator failed test 11')
          if (scalar /= 'hello') call write_fail('test_iterator failed test 12')
        type is (point)
          if (piter%name() /= 'point') call write_fail('test_iterator failed test 13')
          if (scalar%x /= 1 .or. scalar%y /= 2) call write_fail('test_iterator failed test 14')
        class default
          call write_fail('test_iterator failed test 15')
        end select
      type is (any_vector)
        if (.not.piter%is_vector()) call write_fail('test_iterator failed test 16')
        if (piter%is_sublist()) call write_fail('test_iterator failed test 17')
        if (piter%is_scalar()) call write_fail('test_iterator failed test 18')
        if (piter%is_matrix()) call write_fail('test_iterator failed test m2')
        vector => pval%value_ptr()
        vector1 => piter%vector()
        if (.not.associated(vector,vector1)) call write_fail('test_iterator failed test B1')
        select type (vector)
        type is (real)
          if (piter%name() /= 'real') call write_fail('test_iterator failed test 19')
          if (any(vector /= [2.0])) call write_fail('test_iterator failed test 20')
          select type (vector1)
          type is (real)
            if (any(vector1 /= [2.0])) call write_fail('test_iterator failed test B2')
            if (any(vector1 /= vector)) call write_fail('test_iterator failed test B3')
          class default
            call write_fail('test_iterator failed test B4')
          end select
        class default
          call write_fail('test_iterator failed test 21')
        end select
      type is (any_matrix)
        if (.not.piter%is_matrix()) call write_fail('test_iterator failed test 16m')
        if (piter%is_sublist()) call write_fail('test_iterator failed test 17m')
        if (piter%is_scalar()) call write_fail('test_iterator failed test 18m')
        if (piter%is_vector()) call write_fail('test_iterator failed test m3')
        matrix => pval%value_ptr()
        matrix1 => piter%matrix()
        if (.not.associated(matrix,matrix1)) call write_fail('test_iterator failed test B1m')
        select type (matrix)
        type is (integer)
          if (piter%name() /= 'matrix') call write_fail('test_iterator failed test 19m')
          if (any(matrix /= reshape([1,2,3,4,5,6],shape=[2,3]))) call write_fail('test_iterator failed test 20m')
          select type (matrix1)
          type is (integer)
            if (any(matrix1 /= reshape([1,2,3,4,5,6],shape=[2,3]))) call write_fail('test_iterator failed test B2m')
            if (any(matrix1 /= matrix)) call write_fail('test_iterator failed test B3m')
          class default
            call write_fail('test_iterator failed test B4m')
          end select
        class default
          call write_fail('test_iterator failed test 21m')
        end select
      type is (parameter_list)
        if (.not.piter%is_sublist()) call write_fail('test_iterator failed test 22')
        if (piter%is_scalar()) call write_fail('test_iterator failed test 23')
        if (piter%is_vector()) call write_fail('test_iterator failed test 24')
        if (piter%is_matrix()) call write_fail('test_iterator failed test m4')
        sl2 => piter%sublist()
        if (.not. associated(sl,sl2)) call write_fail('test_iterator failed test 25')
        if (.not. associated(sl,pval)) call write_fail('test_iterator failed test 26')
      class default
        call write_fail('test_iterator failed test 27')
      end select
      call piter%next
    end do

    !! Add to the list for the remaining test.
    call p%set('foo', 42)
    sl => p%sublist('sublist2')
    call p%set('bar', 42)

    !! Walk the list, sublists only this time.
    piter = parameter_list_iterator(p, sublists_only=.true.)
    if (piter%count() /= 2) call write_fail('test_iterator failed test 28')
    do j = piter%count(), 1, -1
      if (piter%at_end()) call write_fail('test_iterator failed test 29')
      if (piter%count() /= j) call write_fail('test_iterator failed test 30')
      if (.not.piter%is_sublist()) call write_fail('test_iterator failed test 31')
      call piter%next
    end do
    if (piter%count() /= 0) call write_fail('test_iterator failed test 32')
    if (.not.piter%at_end()) call write_fail('test_iterator failed test 33')

  end subroutine

 !!
 !! Test parameter list path
 !!

  subroutine test_path
    type(parameter_list) :: p
    type(parameter_list), pointer :: sl
    if (p%path() /= '$') call write_fail('test_path failed test 1')
    sl => p%sublist('fiz')
    if (sl%path() /= '$.fiz') call write_fail('test_path failed test 2')
    call p%set_path('foo')
    if (p%path() /= 'foo') call write_fail('test_path failed test 3')
    sl => p%sublist('bar')
    sl => sl%sublist('fubar')
    if (sl%path() /= 'foo.bar.fubar') call write_fail('test_path failed test 4')
    call sl%set_path('biz')
    if (sl%path() /= 'biz') call write_fail('test_path failed test 5')
  end subroutine


  subroutine test_assignment

    type(parameter_list) :: p, q
    type(parameter_list), pointer :: plist
    integer :: i
    real :: r
    character(:), allocatable :: s

    !! Populate a parameter list.
    call p%set('a', 1)
    plist => p%sublist('x')
    call plist%set('b', 2.0)
    plist => plist%sublist('y')
    call plist%set('c', 'foo')

    q = p ! make a (deep) copy

    !! Check that the original is unaltered by the copy (e.g. not deleted)
    call p%get('a', i)
    if (i /= 1) call write_fail('test_assignment failed test 1')
    if (.not.p%is_sublist('x')) call write_fail('test_assignment failed test 2')
    plist => p%sublist('x')
    call plist%get('b', r)
    if (r /= 2.0) call write_fail('test_assignment failed test 3')
    if (.not.plist%is_sublist('y')) call write_fail('test_assignment failed test 4')
    plist => plist%sublist('y')
    call plist%get('c', s)
    if (s /= 'foo') call write_fail('test_assignment failed test 5')

    !! Modify the original
    call p%set('a', -1)
    plist => p%sublist('x')
    call plist%set('b', -2.0)
    plist => plist%sublist('y')
    call plist%set('c', 'bar')

    !! Test the values of the copy
    call q%get('a', i)
    if (i /= 1) call write_fail('test_assignment failed test 6')
    if (.not.q%is_sublist('x')) call write_fail('test_assignment failed test 7')
    plist => q%sublist('x')
    call plist%get('b', r)
    if (r /= 2.0) call write_fail('test_assignment failed test 8')
    if (.not.plist%is_sublist('y')) call write_fail('test_assignment failed test 9')
    plist => plist%sublist('y')
    call plist%get('c', s)
    if (s /= 'foo') call write_fail('test_assignment failed test 10')

    q = p ! copy the original over a now non-empty parameter list

    !! Test the new values
    call q%get('a', i)
    if (i /= -1) call write_fail('test_assignment failed test 11')
    if (.not.q%is_sublist('x')) call write_fail('test_assignment failed test 12')
    plist => q%sublist('x')
    call plist%get('b', r)
    if (r /= -2.0) call write_fail('test_assignment failed test 13')
    if (.not.plist%is_sublist('y')) call write_fail('test_assignment failed test 14')
    plist => plist%sublist('y')
    call plist%get('c', s)
    if (s /= 'bar') call write_fail('test_assignment failed test 15')

  end subroutine


  subroutine write_fail(errmsg)
    use,intrinsic :: iso_fortran_env, only: error_unit
    character(*), intent(in) :: errmsg
    stat = 1
    write(error_unit,'(a)') errmsg
  end subroutine

end program test_parameter_list_type
