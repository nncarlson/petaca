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

#ifdef __INTEL_COMPILER
#define INTEL_WORKAROUND
#endif

program test_parameter_list_type

  use parameter_list_type
  use,intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
#ifdef NAGFOR
  use,intrinsic :: f90_unix, only: exit
#endif

  integer :: stat = 0
  
  call test_basic
  call test_get
  call test_overwrite
  call test_sublists
  call test_iterator

  call exit (stat)

contains

 !!
 !! A basic test that we can populate a parameter list with simple parameters.
 !! Tests the count, is_parameter, is_sublist methods.
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
    
    !! Check that they are recognized as parameters, but not sublists.   
    if (.not.p%is_parameter('foo')) call write_fail ('test_basic failed test 5')
    if (.not.p%is_parameter('bar')) call write_fail ('test_basic failed test 6')
    if (.not.p%is_parameter('wat')) call write_fail ('test_basic failed test 7')
    if (p%is_sublist('foo')) call write_fail ('test_basic failed test 8')
    if (p%is_sublist('bar')) call write_fail ('test_basic failed test 9')
    if (p%is_sublist('wat')) call write_fail ('test_basic failed test 10')
    
    !! Replace the value of a parameter; different rank -- should fail
    call p%set ('foo', [1,2], stat=stat)
    if (stat == 0) call write_fail ('test_basic failed test 11')
    if (p%count() /= 3) call write_fail ('test_basic failed test 12')
    
    !! Replace the value of a parameter; same rank -- should succeed.
    call p%set ('bar', [1,2], stat=stat)
    if (stat /= 0) call write_fail ('test_basic failed test 13')
    if (p%count() /= 3) call write_fail ('test_basic failed test 14')
    
    !! Verify that a non-existant parameter does not exist.
    if (p%is_parameter('dummy')) call write_fail ('test_basic failed test 15')
    if (p%is_sublist('dummy')) call write_fail ('test_basic failed test 16')
    
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
    real(real32) :: r32, r32default
    real(real64) :: r64, r64default
    real(real32), allocatable :: r32array(:), r32arraydefault(:)
    real(real64), allocatable :: r64array(:), r64arraydefault(:)
    logical :: l, ldefault
    logical, allocatable :: larray(:), larraydefault(:)
    character(:), allocatable :: c, carray(:), cdefault, carraydefault(:)
    
    call p%set ('i32', 1_int32)
    call p%set ('i64', 2_int64)
    call p%set ('i32array', [3_int32,4_int32])
    call p%set ('i64array', [5_int64,6_int64])
    call p%set ('r32', 1.0_real32)
    call p%set ('r64', 2.0_real64)
    call p%set ('r32array', [3.0_real32,4.0_real32])
    call p%set ('r64array', [5.0_real64,6.0_real64])
    
    call p%get ('i32', i32)
    call p%get ('i64', i64)
    call p%get ('i32array', i32array)
    call p%get ('i64array', i64array)
    call p%get ('r32', r32)
    call p%get ('r64', r64)
    call p%get ('r32array', r32array)
    call p%get ('r64array', r64array)
    
    if (i32 /= 1) call write_fail ('test_get failed test 1')
    if (i64 /= 2) call write_fail ('test_get failed test 2')
    if (any(i32array /= [3_int32,4_int32])) call write_fail ('test_get failed test 3')
    if (any(i64array /= [5_int64,6_int64])) call write_fail ('test_get failed test 4')
    if (r32 /= 1.0) call write_fail ('test_get failed test 5')
    if (r64 /= 2.0) call write_fail ('test_get failed test 6')
    if (any(r32array /= [3.0_real32,4.0_real32])) call write_fail ('test_get failed test 7')
    if (any(r64array /= [5.0_real64,6.0_real64])) call write_fail ('test_get failed test 8')

    call p%set ('l', .true.)
    call p%set ('larray', [.true.,.false.])
    call p%set ('c', 'bizbat')
    call p%set ('carray', ['foo','bar'])
    
    call p%get ('l', l)
    call p%get ('larray', larray)
    call p%get ('c', c)
    call p%get ('carray', carray)
    
    if (.not.l) call write_fail ('test_get failed test 9')
    if (any(larray .neqv. [.true.,.false.])) call write_fail ('test_get failed test 10')
    if (c /= 'bizbat') call write_fail ('test_get failed test 11')
    if (len(c) /= 6) call write_fail ('test_get failed test 12')
    if (any(carray /= ['foo','bar'])) call write_fail ('test_get failed test 13')
    if (len(carray) /= 3) call write_fail ('test_get failed test 14')
    
    !! Verify that the default argument is ignored for these existing parameters.
    
    call p%get ('i32', i32, default=0_int32)
    call p%get ('i64', i64, default=0_int64)
    call p%get ('i32array', i32array, default=[0_int32])
    call p%get ('i64array', i64array, default=[0_int64])
    call p%get ('r32', r32, default=0.0_real32)
    call p%get ('r64', r64, default=0.0_real64)
    call p%get ('r32array', r32array, default=[0.0_real32])
    call p%get ('r64array', r64array, default=[0.0_real64])
    
    if (i32 /= 1) call write_fail ('test_get failed test 1')
    if (i64 /= 2) call write_fail ('test_get failed test 2')
    if (any(i32array /= [3_int32,4_int32])) call write_fail ('test_get failed test 15')
    if (any(i64array /= [5_int64,6_int64])) call write_fail ('test_get failed test 16')
    if (r32 /= 1.0) call write_fail ('test_get failed test 17')
    if (r64 /= 2.0) call write_fail ('test_get failed test 18')
    if (any(r32array /= [3.0_real32,4.0_real32])) call write_fail ('test_get failed test 19')
    if (any(r64array /= [5.0_real64,6.0_real64])) call write_fail ('test_get failed test 20')
    
    call p%get ('l', l, default=.false.)
    call p%get ('larray', larray, default=[.false.])
    call p%get ('c', c, default='yellow')
    call p%get ('carray', carray, default=['fubar'])
    
    if (.not.l) call write_fail ('test_get failed test 21')
    if (any(larray .neqv. [.true.,.false.])) call write_fail ('test_get failed test 22')
    if (c /= 'bizbat') call write_fail ('test_get failed test 23')
    if (len(c) /= 6) call write_fail ('test_get failed test 24')
    if (any(carray /= ['foo','bar'])) call write_fail ('test_get failed test 25')
    if (len(carray) /= 3) call write_fail ('test_get failed test 26')
    
    !! Verify that the default argument is used for these new parameters.
    
    call p%get ('i32default', i32default, default=10_int32)
    call p%get ('i64default', i64default, default=20_int64)
    call p%get ('i32arraydefault', i32arraydefault, default=[30_int32])
    call p%get ('i64arraydefault', i64arraydefault, default=[40_int64])
    call p%get ('r32default', r32default, default=10.0_real32)
    call p%get ('r64default', r64default, default=20.0_real64)
    call p%get ('r32arraydefault', r32arraydefault, default=[30.0_real32])
    call p%get ('r64arraydefault', r64arraydefault, default=[40.0_real64])
    
    if (i32default /= 10) call write_fail ('test_get failed test 27')
    if (i64default /= 20) call write_fail ('test_get failed test 28')
    if (any(i32arraydefault /= [30_int32])) call write_fail ('test_get failed test 29')
    if (any(i64arraydefault /= [40_int64])) call write_fail ('test_get failed test 30')
    if (r32default /= 10.0) call write_fail ('test_get failed test 31')
    if (r64default /= 20.0) call write_fail ('test_get failed test 32')
    if (any(r32arraydefault /= [30.0])) call write_fail ('test_get failed test 33')
    if (any(r64arraydefault /= [40.0])) call write_fail ('test_get failed test 34')
    
    call p%get ('ldefault', ldefault, default=.false.)
    call p%get ('larraydefault', larraydefault, default=[.true.])
    call p%get ('cdefault', cdefault, default='yellow')
    call p%get ('carraydefault', carraydefault, default=['fubar'])
    
    if (ldefault) call write_fail ('test_get failed test 35')
    if (any(larraydefault .neqv. [.true.])) call write_fail ('test_get failed test 36')
    if (cdefault /= 'yellow') call write_fail ('test_get failed test 37')
    if (len(c) /= 6) call write_fail ('test_get failed test 38')
    if (any(carraydefault /= ['fubar'])) call write_fail ('test_get failed test 39')
    if (len(carraydefault) /= 5) call write_fail ('test_get failed test 40')
    
  end subroutine
  
 !!
 !! Test the overwriting of parameter values
 !!

  subroutine test_overwrite
  
    type(parameter_list) :: p
    real :: r
    real, allocatable :: rarray(:)
    character(:), allocatable :: c, carray(:)
    type point; real x, y; end type
    integer :: stat
#ifdef INTEL_WORKAROUND
    class(*), pointer :: scalar, vector(:)
#endif
    
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
#ifdef INTEL_WORKAROUND
    scalar => p%get_scalar_ptr('foo')
    select type (scalar)
#else
    select type (scalar => p%get_scalar_ptr('foo'))
#endif
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
    if (any(c /= ['blah','blah'])) call write_fail ('test_overwrite failed test 8')
    call p%set ('bar', [point(1.0, 2.0)])
#ifdef INTEL_WORKAROUND
    vector => p%get_vector_ptr('bar')
    select type (vector)
#else
    select type (vector => p%get_vector_ptr('bar'))
#endif
    type is (point)
      if (vector(1)%x /= 1 .or. vector(1)%y /= 2) call write_fail ('test_overwrite failed test 9')
    class default
      call write_fail ('test_overwrite failed test 10')
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
    
    type point; real x, y; end type
#ifdef INTEL_WORKAROUND
    class(parameter_entry), pointer :: pentry
    class(*), pointer :: scalar, vector(:)
#endif
    
    !! Populate a parameter list.
    call p%set ('integer', 1)
    call p%set ('real', [2.0])
    sl => p%sublist ('sublist')
    call p%set ('string', 'hello')
    call p%set ('point', point(1.0,2.0))
    
    !! Walk the list.
    piter = parameter_list_iterator(p)
    if (piter%count() /= 5) call write_fail ('test_iterator failed test 1')
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
#ifdef INTEL_WORKAROUND
      pentry => piter%value()
      select type (pentry)
#else
      select type (pentry => piter%value())
#endif
      type is (any_scalar)
        if (piter%is_list()) call write_fail ('test_iterator failed test 6')
#ifdef INTEL_WORKAROUND
        scalar => pentry%value_ptr()
        select type (scalar)
#else
        select type (scalar => pentry%value_ptr())
#endif
        type is (integer)
          if (piter%name() /= 'integer') call write_fail ('test_iterator failed test 7')
          if (scalar /= 1) call write_fail ('test_iterator failed test 8')
        type is (character(*))
          if (piter%name() /= 'string') call write_fail ('test_iterator failed test 9')
          if (scalar /= 'hello') call write_fail ('test_iterator failed test 10')
        type is (point)
          if (piter%name() /= 'point') call write_fail ('test_iterator failed test 11')
          if (scalar%x /= 1 .or. scalar%y /= 2) call write_fail ('test_iterator failed test 12')
        class default
          call write_fail ('test_iterator failed test 13')
        end select
      type is (any_vector)
        if (piter%is_list()) call write_fail ('test_iterator failed test 14')
#ifdef INTEL_WORKAROUND
        vector => pentry%value_ptr()
        select type (vector)
#else
        select type (vector => pentry%value_ptr())
#endif
        type is (real)
          if (piter%name() /= 'real') call write_fail ('test_iterator failed test 15')
          if (any(vector /= [2.0])) call write_fail ('test_iterator failed test 16')
        class default
          call write_fail ('test_iterator failed test 17')
        end select
      type is (parameter_list)
        if (.not.piter%is_list()) call write_fail ('test_iterator failed test 18')
        sl2 => piter%sublist()
        if (.not. associated(sl,sl2)) call write_fail ('test_iterator failed test 19')
        if (.not. associated(sl,pentry)) call write_fail ('test_iterator failed test 20')
      class default
        call write_fail ('test_iterator failed test 21')
      end select
      call piter%next
    end do

  end subroutine

  subroutine write_fail (errmsg)
    use,intrinsic :: iso_fortran_env, only: error_unit
    character(*), intent(in) :: errmsg
    stat = 1
    write(error_unit,'(a)') errmsg
  end subroutine

end program test_parameter_list_type
