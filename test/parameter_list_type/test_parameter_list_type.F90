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
#ifdef NAGFOR
  use,intrinsic :: f90_unix, only: exit
#endif

  integer :: stat = 0

  call test_basic
  call test_get
  call test_get_any
  call test_overwrite
  call test_sublists
  call test_iterator
  call test_name

  call exit (stat)

contains

 !!
 !! A basic test that we can populate a parameter list with simple parameters.
 !! Tests the count, is_parameter, is_sublist, is_scalar, is_vector, is_matrix
 !! methods.
 !!

  subroutine test_basic

    type(parameter_list) :: p
    integer ::stat

    !! A derived type
    type point; real x, y; end type

    !! Check that the initial list is empty.
    if (p%count() /= 0) call write_fail ('test_basic failed test 1')

    !! Define parameters of various types and ranks,
    !! checking the parameter count each time.
    call p%set ('foo', 1)
    if (p%count() /= 1) call write_fail ('test_basic failed test 2')
    call p%set ('bar', [1.0,3.14])
    if (p%count() /= 2) call write_fail ('test_basic failed test 3')
    call p%set ('wat', ['biz','bat'])
    if (p%count() /= 3) call write_fail ('test_basic failed test 4')
    call p%set ('bah', reshape([1,2,3,4],shape=[2,2]))
    if (p%count() /= 4) call write_fail ('test_basic failed test 5')

    !! Check that they are recognized as parameters, and of the correct type.
    if (.not.p%is_parameter('foo')) call write_fail ('test_basic failed test 6')
    if (p%is_sublist('foo')) call write_fail ('test_basic failed test 7')
    if (.not.p%is_scalar('foo')) call write_fail ('test_basic failed test 8')
    if (p%is_vector('foo')) call write_fail ('test_basic failed test 9')
    if (p%is_matrix('foo')) call write_fail ('test_basic failed test 10')
    if (.not.p%is_parameter('bar')) call write_fail ('test_basic failed test 1')
    if (p%is_sublist('bar')) call write_fail ('test_basic failed test 12')
    if (p%is_scalar('bar')) call write_fail ('test_basic failed test 13')
    if (.not.p%is_vector('bar')) call write_fail ('test_basic failed test 14')
    if (p%is_matrix('bar')) call write_fail ('test_basic failed test 15')
    if (.not.p%is_parameter('wat')) call write_fail ('test_basic failed test 16')
    if (p%is_sublist('wat')) call write_fail ('test_basic failed test 17')
    if (.not.p%is_parameter('bah')) call write_fail ('test_basic failed test 18')
    if (p%is_sublist('bah')) call write_fail ('test_basic failed test 19')
    if (p%is_scalar('bah')) call write_fail ('test_basic failed test 20')
    if (p%is_vector('bah')) call write_fail ('test_basic failed test 21')
    if (.not.p%is_matrix('bah')) call write_fail ('test_basic failed test 22')

    !! Replace the value of a parameter; different rank -- should fail
    call p%set ('foo', [1,2], stat=stat)
    if (stat == 0) call write_fail ('test_basic failed test 23')
    if (p%count() /= 4) call write_fail ('test_basic failed test 24')

    !! Replace the value of a parameter; same rank -- should succeed.
    call p%set ('bar', [1,2], stat=stat)
    if (stat /= 0) call write_fail ('test_basic failed test 25')
    if (p%count() /= 4) call write_fail ('test_basic failed test 26')

    !! Verify that a non-existant parameter does not exist.
    if (p%is_parameter('dummy')) call write_fail ('test_basic failed test 27')
    if (p%is_sublist('dummy')) call write_fail ('test_basic failed test 28')
    if (p%is_scalar('dummy')) call write_fail ('test_basic failed test 29')
    if (p%is_vector('dummy')) call write_fail ('test_basic failed test 30')
    if (p%is_matrix('dummy')) call write_fail ('test_basic failed test 31')

  end subroutine

 !!
 !! A thorough test of set/get for all the specific intrinsic types.
 !!

  subroutine test_get

    type(parameter_list) :: p
    integer(int32) :: i32, i32default
    integer(int64) :: i64, i64default
    integer(int32), allocatable :: i32array(:), i32arraydefault(:)
    integer(int64), allocatable :: i64array(:), i64arraydefault(:)
    integer(int32), allocatable :: i32matrix(:,:), i32matrixdefault(:,:), i32matrixref(:,:)
    integer(int64), allocatable :: i64matrix(:,:), i64matrixdefault(:,:), i64matrixref(:,:)
    real(real32) :: r32, r32default
    real(real64) :: r64, r64default
    real(real32), allocatable :: r32array(:), r32arraydefault(:)
    real(real64), allocatable :: r64array(:), r64arraydefault(:)
    real(real32), allocatable :: r32matrix(:,:), r32matrixdefault(:,:), r32matrixref(:,:)
    real(real64), allocatable :: r64matrix(:,:), r64matrixdefault(:,:), r64matrixref(:,:)
    logical :: l, ldefault
    logical, allocatable :: larray(:), larraydefault(:)
    logical, allocatable :: lmatrix(:,:), lmatrixdefault(:,:)
    character(:), allocatable :: c, carray(:), cdefault, carraydefault(:)
    character(:), allocatable :: cmatrix(:,:), cmatrixdefault(:,:)

    call p%set ('i32', 1_int32)
    call p%set ('i64', 2_int64)
    call p%set ('i32array', [3_int32,4_int32])
    call p%set ('i64array', [5_int64,6_int64])
    i32matrixref = reshape([integer(int32) :: 3, 4, 5, 6, 7, 8], shape=[2,3])
    call p%set ('i32matrix', i32matrixref)
    i64matrixref = reshape([integer(int64) :: 3, 4, 5, 6, 7, 8], shape=[2,3])
    call p%set ('i64matrix', i64matrixref)
    call p%set ('r32', 1.0_real32)
    call p%set ('r64', 2.0_real64)
    call p%set ('r32array', [3.0_real32,4.0_real32])
    call p%set ('r64array', [5.0_real64,6.0_real64])
    r32matrixref = reshape([real(real32) :: 3, 4, 5, 6, 7, 8], shape=[3,2])
    call p%set ('r32matrix', r32matrixref)
    r64matrixref = reshape([real(real64) :: 3, 4, 5, 6, 7, 8], shape=[3,2])
    call p%set ('r64matrix', r64matrixref)

    call p%get ('i32', i32)
    call p%get ('i64', i64)
    call p%get ('i32array', i32array)
    call p%get ('i64array', i64array)
    call p%get ('i32matrix', i32matrix)
    call p%get ('i64matrix', i64matrix)
    call p%get ('r32', r32)
    call p%get ('r64', r64)
    call p%get ('r32array', r32array)
    call p%get ('r64array', r64array)
    call p%get ('r32matrix', r32matrix)
    call p%get ('r64matrix', r64matrix)

    if (i32 /= 1) call write_fail ('test_get failed test 1')
    if (i64 /= 2) call write_fail ('test_get failed test 2')
    if (any(i32array /= [3_int32,4_int32])) call write_fail ('test_get failed test 3')
    if (any(i64array /= [5_int64,6_int64])) call write_fail ('test_get failed test 4')
    if (any(i32matrix /= i32matrixref)) call write_fail ('test_get failed test 3m')
    if (any(i64matrix /= i64matrixref)) call write_fail ('test_get failed test 4m')
    if (r32 /= 1.0) call write_fail ('test_get failed test 5')
    if (r64 /= 2.0) call write_fail ('test_get failed test 6')
    if (any(r32array /= [3.0_real32,4.0_real32])) call write_fail ('test_get failed test 7')
    if (any(r64array /= [5.0_real64,6.0_real64])) call write_fail ('test_get failed test 8')
    if (any(r32matrix /= r32matrixref)) call write_fail ('test_get failed test 7m')
    if (any(r64matrix /= r64matrixref)) call write_fail ('test_get failed test 8m')

    call p%set ('l', .true.)
    call p%set ('larray', [.true.,.false.])
    call p%set ('lmatrix', reshape([.true.,.false.],shape=[1,2]))
    call p%set ('c', 'bizbat')
    call p%set ('carray', ['foo','bar'])
    call p%set ('cmatrix', reshape(['foo','bar'],shape=[2,1]))

    call p%get ('l', l)
    call p%get ('larray', larray)
    call p%get ('lmatrix', lmatrix)
    call p%get ('c', c)
    call p%get ('carray', carray)
    call p%get ('cmatrix', cmatrix)

    if (.not.l) call write_fail ('test_get failed test 9')
    if (any(larray .neqv. [.true.,.false.])) call write_fail ('test_get failed test 10')
    if (any(lmatrix(1,:) .neqv. [.true.,.false.])) call write_fail ('test_get failed test 10m')
    if (c /= 'bizbat') call write_fail ('test_get failed test 11')
    if (len(c) /= 6) call write_fail ('test_get failed test 12')
    if (any(carray /= ['foo','bar'])) call write_fail ('test_get failed test 13')
    if (len(carray) /= 3) call write_fail ('test_get failed test 14')
    if (any(cmatrix(:,1) /= ['foo','bar'])) call write_fail ('test_get failed test 13m')
    if (len(cmatrix) /= 3) call write_fail ('test_get failed test 14m')

    !! Verify that the default argument is ignored for these existing parameters.

    call p%get ('i32', i32, default=0_int32)
    call p%get ('i64', i64, default=0_int64)
    call p%get ('i32array', i32array, default=[0_int32])
    call p%get ('i64array', i64array, default=[0_int64])
    call p%get ('i32matrix', i32matrix, default=reshape([0_int32],shape=[1,1]))
    call p%get ('i64matrix', i64matrix, default=reshape([0_int64],shape=[1,1]))
    call p%get ('r32', r32, default=0.0_real32)
    call p%get ('r64', r64, default=0.0_real64)
    call p%get ('r32array', r32array, default=[0.0_real32])
    call p%get ('r64array', r64array, default=[0.0_real64])
    call p%get ('r32matrix', r32matrix, default=reshape([0.0_real32],shape=[1,1]))
    call p%get ('r64matrix', r64matrix, default=reshape([0.0_real64],shape=[1,1]))

    if (i32 /= 1) call write_fail ('test_get failed test 1')
    if (i64 /= 2) call write_fail ('test_get failed test 2')
    if (any(i32array /= [3_int32,4_int32])) call write_fail ('test_get failed test 15')
    if (any(i64array /= [5_int64,6_int64])) call write_fail ('test_get failed test 16')
    if (any(i32matrix /= i32matrixref)) call write_fail ('test_get failed test 15m')
    if (any(i64matrix /= i64matrixref)) call write_fail ('test_get failed test 16m')
    if (r32 /= 1.0) call write_fail ('test_get failed test 17')
    if (r64 /= 2.0) call write_fail ('test_get failed test 18')
    if (any(r32array /= [3.0_real32,4.0_real32])) call write_fail ('test_get failed test 19')
    if (any(r64array /= [5.0_real64,6.0_real64])) call write_fail ('test_get failed test 20')
    if (any(r32matrix /= r32matrixref)) call write_fail ('test_get failed test 19m')
    if (any(r64matrix /= r64matrixref)) call write_fail ('test_get failed test 20m')

    call p%get ('l', l, default=.false.)
    call p%get ('larray', larray, default=[.false.])
    call p%get ('lmatrix', lmatrix, default=reshape([.false.],shape=[1,1]))
    call p%get ('c', c, default='yellow')
    call p%get ('carray', carray, default=['fubar'])
    call p%get ('cmatrix', cmatrix, default=reshape(['fubar'],shape=[1,1]))

    if (.not.l) call write_fail ('test_get failed test 21')
    if (any(larray .neqv. [.true.,.false.])) call write_fail ('test_get failed test 22')
    if (any(lmatrix(1,:) .neqv. [.true.,.false.])) call write_fail ('test_get failed test 22m')
    if (c /= 'bizbat') call write_fail ('test_get failed test 23')
    if (len(c) /= 6) call write_fail ('test_get failed test 24')
    if (any(carray /= ['foo','bar'])) call write_fail ('test_get failed test 25')
    if (len(carray) /= 3) call write_fail ('test_get failed test 26')
    if (any(cmatrix(:,1) /= ['foo','bar'])) call write_fail ('test_get failed test 25m')
    if (len(cmatrix) /= 3) call write_fail ('test_get failed test 25m')

    !! Verify that the default argument is used for these new parameters.

    call p%get ('i32default', i32default, default=10_int32)
    call p%get ('i64default', i64default, default=20_int64)
    call p%get ('i32arraydefault', i32arraydefault, default=[30_int32])
    call p%get ('i64arraydefault', i64arraydefault, default=[40_int64])
    call p%get ('i32matrixdefault', i32matrixdefault, default=reshape([30_int32],shape=[1,1]))
    call p%get ('i64matrixdefault', i64matrixdefault, default=reshape([40_int64],shape=[1,1]))
    call p%get ('r32default', r32default, default=10.0_real32)
    call p%get ('r64default', r64default, default=20.0_real64)
    call p%get ('r32arraydefault', r32arraydefault, default=[30.0_real32])
    call p%get ('r64arraydefault', r64arraydefault, default=[40.0_real64])
    call p%get ('r32matrixdefault', r32matrixdefault, default=reshape([30.0_real32],shape=[1,1]))
    call p%get ('r64matrixdefault', r64matrixdefault, default=reshape([40.0_real64],shape=[1,1]))

    if (i32default /= 10) call write_fail ('test_get failed test 27')
    if (i64default /= 20) call write_fail ('test_get failed test 28')
    if (any(i32arraydefault /= [30_int32])) call write_fail ('test_get failed test 29')
    if (any(i64arraydefault /= [40_int64])) call write_fail ('test_get failed test 30')
    if (i32matrixdefault(1,1) /= 30_int32) call write_fail ('test_get failed test 29m')
    if (i64matrixdefault(1,1) /= 40_int64) call write_fail ('test_get failed test 30m')
    if (r32default /= 10.0) call write_fail ('test_get failed test 31')
    if (r64default /= 20.0) call write_fail ('test_get failed test 32')
    if (any(r32arraydefault /= [30.0])) call write_fail ('test_get failed test 33')
    if (any(r64arraydefault /= [40.0])) call write_fail ('test_get failed test 34')
    if (r32matrixdefault(1,1) /= 30.0) call write_fail ('test_get failed test 33m')
    if (r64matrixdefault(1,1) /= 40.0) call write_fail ('test_get failed test 34m')

    call p%get ('ldefault', ldefault, default=.false.)
    call p%get ('larraydefault', larraydefault, default=[.true.])
    call p%get ('lmatrixdefault', lmatrixdefault, default=reshape([.true.],shape=[1,1]))
    call p%get ('cdefault', cdefault, default='yellow')
    call p%get ('carraydefault', carraydefault, default=['fubar'])
    call p%get ('cmatrixdefault', cmatrixdefault, default=reshape(['fubar'],shape=[1,1]))

    if (ldefault) call write_fail ('test_get failed test 35')
    if (any(larraydefault .neqv. [.true.])) call write_fail ('test_get failed test 36')
    if (lmatrixdefault(1,1) .neqv. .true.) call write_fail ('test_get failed test 36m')
    if (cdefault /= 'yellow') call write_fail ('test_get failed test 37')
    if (len(c) /= 6) call write_fail ('test_get failed test 38')
    if (any(carraydefault /= ['fubar'])) call write_fail ('test_get failed test 39')
    if (len(carraydefault) /= 5) call write_fail ('test_get failed test 40')
    if (cmatrixdefault(1,1) /= 'fubar') call write_fail ('test_get failed test 39m')
    if (len(cmatrixdefault) /= 5) call write_fail ('test_get failed test 40m')

  end subroutine

 !!
 !! Test the get_any methods
 !!

  subroutine test_get_any

    type(parameter_list) :: p

    type point; real x, y; end type
    class(*), allocatable :: scalar, vector(:), matrix(:,:)

    !! Define parameters using the provided default value.

    call p%get_any ('a', scalar, default=1)
    select type (scalar)
    type is (integer)
      if (scalar /= 1) call write_fail ('test_get_any failed test 1')
    class default
      call write_fail ('test_get_any failed test 2')
    end select

    call p%get_any ('b', vector, default=[1.0,2.0])
    select type (vector)
    type is (real)
      if (any(vector /= [1,2])) call write_fail ('test_get_any failed test 3')
    class default
      call write_fail ('test_get_any failed test 4')
    end select

    call p%get_any ('bm', matrix, default=reshape([1.0,2.0],shape=[2,1]))
    select type (matrix)
    type is (real)
      if (any(matrix(:,1) /= [1,2])) call write_fail ('test_get_any failed test 3m')
    class default
      call write_fail ('test_get_any failed test 4m')
    end select

    call p%get_any ('c', scalar, default=point(1.0,2.0))
    select type (scalar)
    type is (point)
      if (scalar%x /= 1 .or. scalar%y /= 2) call write_fail ('test_get_any failed test 5')
    class default
      call write_fail ('test_get_any failed test 6')
    end select

#ifndef GNU_67564
    call p%get_any ('d', vector, default=['foo','bar'])
    select type (vector)
    type is (character(*))
      if (any(vector /= ['foo','bar'])) call write_fail ('test_get_any failed test 7')
    class default
      call write_fail ('test_get_any failed test 8')
    end select

    call p%get_any ('dm', matrix, default=reshape(['foo','bar'],shape=[1,2]))
    select type (matrix)
    type is (character(*))
      if (any(matrix(1,:) /= ['foo','bar'])) call write_fail ('test_get_any failed test 7m')
    class default
      call write_fail ('test_get_any failed test 8m')
    end select
#endif

    !call p%set ('e', [point(1.0,2.0)])
    call p%get_any ('e', vector, default=[point(1.0,2.0)])
    select type (vector)
    type is (point)
      if (vector(1)%x /= 1.0 .or. vector(1)%y /= 2.0) call write_fail ('test_get_any failed test 9')
    class default
      call write_fail ('test_get_any failed test 10')
    end select

    call p%get_any ('em', matrix, default=reshape([point(1.0,2.0)],shape=[1,1]))
    select type (matrix)
    type is (point)
      if (matrix(1,1)%x /= 1.0 .or. matrix(1,1)%y /= 2.0) call write_fail ('test_get_any failed test 9m')
    class default
      call write_fail ('test_get_any failed test 10m')
    end select

    !! Get them again with different default values that should be ignored.

    call p%get_any ('a', scalar, default=0)
    select type (scalar)
    type is (integer)
      if (scalar /= 1) call write_fail ('test_get_any failed test 11')
    class default
      call write_fail ('test_get_any failed test 12')
    end select

    call p%get_any ('b', vector, default=[0.0])
    select type (vector)
    type is (real)
      if (any(vector /= [1,2])) call write_fail ('test_get_any failed test 13')
    class default
      call write_fail ('test_get_any failed test 14')
    end select

    call p%get_any ('bm', matrix, default=reshape([0.0],shape=[1,1]))
    select type (matrix)
    type is (real)
      if (any(matrix(:,1) /= [1,2])) call write_fail ('test_get_any failed test 13m')
    class default
      call write_fail ('test_get_any failed test 14m')
    end select

    call p%get_any ('c', scalar, default="fubar")
    select type (scalar)
    type is (point)
      if (scalar%x /= 1 .or. scalar%y /= 2) call write_fail ('test_get_any failed test 15')
    class default
      call write_fail ('test_get_any failed test 16')
    end select

#ifndef GNU_67564
    call p%get_any ('d', vector, default=[point(1.0,2.0)])
    select type (vector)
    type is (character(*))
      if (any(vector /= ['foo','bar'])) call write_fail ('test_get_any failed test 17')
    class default
      call write_fail ('test_get_any failed test 18')
    end select

    call p%get_any ('dm', matrix, default=reshape([point(1.0,2.0)],shape=[1,1]))
    select type (matrix)
    type is (character(*))
      if (any(matrix(1,:) /= ['foo','bar'])) call write_fail ('test_get_any failed test 17m')
    class default
      call write_fail ('test_get_any failed test 18m')
    end select
#endif

    call p%get_any ('e', vector, default=[13])
    select type (vector)
    type is (point)
      if (vector(1)%x /= 1.0 .or. vector(1)%y /= 2.0) call write_fail ('test_get_any failed test 19')
    class default
      call write_fail ('test_get_any failed test 20')
    end select

    call p%get_any ('em', matrix, default=reshape([13],shape=[1,1]))
    select type (matrix)
    type is (point)
      if (matrix(1,1)%x /= 1.0 .or. matrix(1,1)%y /= 2.0) call write_fail ('test_get_any failed test 19m')
    class default
      call write_fail ('test_get_any failed test 20m')
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
    class(*), allocatable :: scalar, vector(:), matrix(:,:)

    call p%set ('foo', 13)

    !! Overwrite with different rank; should fail
    call p%set ('foo', [1], stat=stat)
    if (stat == 0) call write_fail ('test_overwrite failed test 1')

    !! Overwrite with different values/types.
    call p%set ('foo', 11.0)
    call p%get ('foo', r)
    if (r /= 11.0) call write_fail ('test_overwrite failed test 2')
    call p%set ('foo', 'blah')
    call p%get ('foo', c)
    if (c /= 'blah') call write_fail ('test_overwrite failed test 3')
    call p%set ('foo', point(1.0, 2.0))
    call p%get_any ('foo', scalar)
    select type (scalar)
    type is (point)
      if (scalar%x /= 1 .or. scalar%y /= 2) call write_fail ('test_overwrite failed test 4')
    class default
      call write_fail ('test_overwrite failed test 5')
    end select

    call p%set ('bar', [13])

    !! Overwrite with different rank; should fail
    call p%set ('bar', 1, stat=stat)
    if (stat == 0) call write_fail ('test_overwrite failed test 6')

    !! Overwrite with different values/types.
    call p%set ('bar', [11.0])
    call p%get ('bar', rarray)
    if (any(rarray /= [11.0])) call write_fail ('test_overwrite failed test 7')
    call p%set ('bar', ['blah','blah'])
    call p%get ('bar', carray)
    if (any(carray /= ['blah','blah'])) call write_fail ('test_overwrite failed test 8')
    call p%set ('bar', [point(1.0, 2.0)])
    call p%get_any ('bar', vector)
    select type (vector)
    type is (point)
      if (vector(1)%x /= 1 .or. vector(1)%y /= 2) call write_fail ('test_overwrite failed test 9')
    class default
      call write_fail ('test_overwrite failed test 10')
    end select

    call p%set ('biz', reshape([13],shape=[1,1]))

    !! Overwrite with different rank; should fail
    call p%set ('biz', [1], stat=stat)
    if (stat == 0) call write_fail ('test_overwrite failed test 11')

    !! Overwrite with different values/types.
    call p%set ('biz', reshape([11.0],shape=[1,1]))
    call p%get ('biz', rmatrix)
    if (rmatrix(1,1) /= 11.0) call write_fail ('test_overwrite failed test 12')
    call p%set ('biz', reshape(['blah','blah'],shape=[2,1]))
    call p%get ('biz', cmatrix)
    if (any(cmatrix(:,1) /= ['blah','blah'])) call write_fail ('test_overwrite failed test 13')
    call p%set ('biz', reshape([point(1.0, 2.0)],shape=[1,1]))
    call p%get_any ('biz', matrix)
    select type (matrix)
    type is (point)
      if (matrix(1,1)%x /= 1 .or. matrix(1,1)%y /= 2) call write_fail ('test_overwrite failed test 14')
    class default
      call write_fail ('test_overwrite failed test 15')
    end select

  end subroutine

 !!
 !! Tests the creation and access to sublists
 !!

  subroutine test_sublists

    type(parameter_list) :: p
    type(parameter_list), pointer :: sl, sla, slb
    integer :: stat

    !! Create a sublist parameter and add a parameter to the sublist.
    sla => p%sublist('A')
    if (.not.associated(sla)) call write_fail ('test_sublists failed test 1')
    if (p%count() /= 1) call write_fail ('test_sublists failed test 2')
    call sla%set ('foo', 42)

    !! 'A' should be recognized as both a parameter and a sublist.
    if (.not.p%is_sublist('A')) call write_fail ('test_sublists failed test 3')
    if (.not.p%is_parameter('A')) call write_fail ('test_sublists failed test 4')

    !! Try to use sublist with an existing non-sublist parameter; should fail.
    slb => sla%sublist('foo', stat)
    if (stat == 0) call write_fail ('test_sublists failed test 5')

    !! Create a sublist parameter of the sublist.
    slb => sla%sublist('B')
    if (.not.associated(slb)) call write_fail ('test_sublists failed test 6')
    if (sla%count() /= 2) call write_fail ('test_sublists failed test 7')
    if (.not.sla%is_sublist('B')) call write_fail ('test_sublists failed test 8')
    if (.not.sla%is_parameter('B')) call write_fail ('test_sublists failed test 9')

    !! Access the 'A' sublist again and verify it is the same.
    sl => p%sublist('A')
    if (.not.associated(sl,sla)) call write_fail ('test_sublists failed test 5')

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
    class(parameter_entry), pointer :: pentry
    class(*), pointer :: scalar, scalar1, vector(:), vector1(:), matrix(:,:), matrix1(:,:)

    !! Populate a parameter list.
    call p%set ('integer', 1)
    call p%set ('real', [2.0])
    call p%set ('matrix', reshape([1,2,3,4,5,6],shape=[2,3]))
    sl => p%sublist ('sublist')
    call p%set ('string', 'hello')
    call p%set ('point', point(1.0,2.0))

    !! Walk the list.
    piter = parameter_list_iterator(p)
    if (piter%count() /= 6) call write_fail ('test_iterator failed test 1')
    do j = piter%count(), 1, -1
      if (piter%at_end()) call write_fail ('test_iterator failed test 2')
      if (piter%count() /= j) call write_fail ('test_iterator failed test 3')
      call piter%next()
    end do
    if (piter%count() /= 0) call write_fail ('test_iterator failed test 4')
    if (.not.piter%at_end()) call write_fail ('test_iterator failed test 5')

    !! Walk the list again and check values this time.
    piter = parameter_list_iterator(p)
    do while (.not.piter%at_end())
      pentry => piter%entry()
      select type (pentry)
      type is (any_scalar)
        if (.not.piter%is_scalar()) call write_fail ('test_iterator failed test 6')
        if (piter%is_list()) call write_fail ('test_iterator failed test 7')
        if (piter%is_vector()) call write_fail ('test_iterator failed test 8')
        if (piter%is_matrix()) call write_fail ('test_iterator failed test m1')
        scalar1 => piter%scalar()
        scalar => pentry%value_ptr()
        if (.not.associated(scalar,scalar1)) call write_fail ('test_iterator failed test A')
        select type (scalar)
        type is (integer)
          if (piter%name() /= 'integer') call write_fail ('test_iterator failed test 9')
          if (scalar /= 1) call write_fail ('test_iterator failed test 10')
        type is (character(*))
          if (piter%name() /= 'string') call write_fail ('test_iterator failed test 11')
          if (scalar /= 'hello') call write_fail ('test_iterator failed test 12')
        type is (point)
          if (piter%name() /= 'point') call write_fail ('test_iterator failed test 13')
          if (scalar%x /= 1 .or. scalar%y /= 2) call write_fail ('test_iterator failed test 14')
        class default
          call write_fail ('test_iterator failed test 15')
        end select
      type is (any_vector)
        if (.not.piter%is_vector()) call write_fail ('test_iterator failed test 16')
        if (piter%is_list()) call write_fail ('test_iterator failed test 17')
        if (piter%is_scalar()) call write_fail ('test_iterator failed test 18')
        if (piter%is_matrix()) call write_fail ('test_iterator failed test m2')
        vector => pentry%value_ptr()
        vector1 => piter%vector()
        if (.not.associated(vector,vector1)) call write_fail ('test_iterator failed test B1')
        select type (vector)
        type is (real)
          if (piter%name() /= 'real') call write_fail ('test_iterator failed test 19')
          if (any(vector /= [2.0])) call write_fail ('test_iterator failed test 20')
          select type (vector1)
          type is (real)
            if (any(vector1 /= [2.0])) call write_fail ('test_iterator failed test B2')
            if (any(vector1 /= vector)) call write_fail ('test_iterator failed test B3')
          class default
            call write_fail ('test_iterator failed test B4')
          end select
        class default
          call write_fail ('test_iterator failed test 21')
        end select
      type is (any_matrix)
        if (.not.piter%is_matrix()) call write_fail ('test_iterator failed test 16m')
        if (piter%is_list()) call write_fail ('test_iterator failed test 17m')
        if (piter%is_scalar()) call write_fail ('test_iterator failed test 18m')
        if (piter%is_vector()) call write_fail ('test_iterator failed test m3')
        matrix => pentry%value_ptr()
        matrix1 => piter%matrix()
        if (.not.associated(matrix,matrix1)) call write_fail ('test_iterator failed test B1m')
        select type (matrix)
        type is (integer)
          if (piter%name() /= 'matrix') call write_fail ('test_iterator failed test 19m')
          if (any(matrix /= reshape([1,2,3,4,5,6],shape=[2,3]))) call write_fail ('test_iterator failed test 20m')
          select type (matrix1)
          type is (integer)
            if (any(matrix1 /= reshape([1,2,3,4,5,6],shape=[2,3]))) call write_fail ('test_iterator failed test B2m')
            if (any(matrix1 /= matrix)) call write_fail ('test_iterator failed test B3m')
          class default
            call write_fail ('test_iterator failed test B4m')
          end select
        class default
          call write_fail ('test_iterator failed test 21m')
        end select
      type is (parameter_list)
        if (.not.piter%is_list()) call write_fail ('test_iterator failed test 22')
        if (piter%is_scalar()) call write_fail ('test_iterator failed test 23')
        if (piter%is_vector()) call write_fail ('test_iterator failed test 24')
        if (piter%is_matrix()) call write_fail ('test_iterator failed test m4')
        sl2 => piter%sublist()
        if (.not. associated(sl,sl2)) call write_fail ('test_iterator failed test 25')
        if (.not. associated(sl,pentry)) call write_fail ('test_iterator failed test 26')
      class default
        call write_fail ('test_iterator failed test 27')
      end select
      call piter%next
    end do

  end subroutine

 !!
 !! Test parameter list name
 !!

  subroutine test_name
    type(parameter_list) :: p
    type(parameter_list), pointer :: sl
    if (p%name() /= '$') call write_fail ('test_name failed test 1')
    sl => p%sublist('fiz')
    if (sl%name() /= '$->fiz') call write_fail ('test_name failed test 2')
    call p%set_name('foo')
    if (p%name() /= 'foo') call write_fail ('test_name failed test 3')
    sl => p%sublist('bar')
    sl => sl%sublist('fubar')
    if (sl%name() /= 'foo->bar->fubar') call write_fail ('test_name failed test 4')
    call sl%set_name ('biz')
    if (sl%name() /= 'biz') call write_fail ('test_name failed test 5')
  end subroutine

  subroutine write_fail (errmsg)
    use,intrinsic :: iso_fortran_env, only: error_unit
    character(*), intent(in) :: errmsg
    stat = 1
    write(error_unit,'(a)') errmsg
  end subroutine

end program test_parameter_list_type
