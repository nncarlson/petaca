!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2022  Neil N. Carlson
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

program perf_test

  use,intrinsic :: iso_fortran_env, only: i8 => int64
  use wavl_tree_type
  implicit none

  integer :: n, p
  integer, allocatable :: seed(:), array1(:), array2(:)
  integer(i8) :: t1, t2, rate
  real :: r
  type(wavl_tree) :: tree
  
  call random_seed(size=n)
  allocate(seed(n))
  seed = -452913
  
  p = 20
  n = 2**p - 1
  allocate(array1(n), array2(n))
  do n = 1, size(array1)
    call random_number(r)
    array1(n) = r * 2**(p+2)
    call random_number(r)
    array2(n) = r * 2**(p+2)
  end do

  call insertion(array1)
  call lookup(array2)
  call deletion(array1)

contains

  subroutine insertion(array)
    integer, intent(in) :: array(:)
    call system_clock(t1)
    do n = 1, size(array)
      call tree%insert(array(n), -array(n))
    end do
    call system_clock(t2, rate)
    write(*,'(a,i0,a,g0,a)') 'insertion of ', size(array), ' random values: ', real(t2-t1)/real(rate), ' sec'
  end subroutine

  subroutine lookup(array)
    integer, intent(in) :: array(:)
    type(rbt_node), pointer :: node
    logical :: dummy
    call system_clock(t1)
    do n = 1, size(array)
      node => tree%lookup(array(n))
    end do
    call system_clock(t2, rate)
    write(*,'(a,i0,a,g0,a)') 'lookup of ', size(array), ' random values:    ', real(t2-t1)/real(rate), ' sec'
  end subroutine

  subroutine deletion(array)
    integer, intent(in) :: array(:)
    call system_clock(t1)
    do n = 1, size(array)
      call tree%delete(array(n))
    end do
    call system_clock(t2, rate)
    write(*,'(a,i0,a,g0,a)') 'deletion of ', size(array), ' random values:  ', real(t2-t1)/real(rate), ' sec'
  end subroutine

end program
