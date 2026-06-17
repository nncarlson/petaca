!!
!! Unit Tests for the CO_BTD_MATRIX_TYPE Module
!!
!! Copyright (c) 2023  Neil N. Carlson
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

program btd_matrix_example

  use,intrinsic :: iso_fortran_env, only: r8 => real64, i8 => int64
  use co_btd_matrix_type
  implicit none

  integer :: nb, ntotal, n, j
  character(16) :: arg
  real(r8), allocatable :: b(:,:)
  type(co_btd_matrix) :: a
  integer(i8) :: t1, t2, t3, rate

  call get_command_argument(1, arg)
  read(arg,*) nb

  call get_command_argument(2, arg)
  read(arg,*) ntotal

  !! Partition the NTOTAL equations into nearly equal blocks
  n = ntotal/num_images()
  if (this_image() <= ntotal - n*num_images()) n = n + 1

  if (this_image() == 1) then
    write(*,'(a,i0,a)') 'Using ', num_images(), ' images'
    write(*,'(2(a,i0))') 'block size ', nb, '; total size ', ntotal
  end if

  allocate(b(nb,n))
  
  if (this_image() == 1) call system_clock(count_rate=rate)

  !! Solve the tridiagonal linear system
  t3 = 0
  do j = 1, 1000
    call a%init(nb, n)
    call matrix_fill(a)
    sync all
    if (this_image() == 1) call system_clock(t1)
    call a%factor
    if (this_image() == 1) then
      call system_clock(t2)
      t3 = t3 + (t2 - t1)
    end if
  end do
  if (this_image() == 1) write(*,'(a,f6.3,a)') 'factor cpu =', real(t3)/real(rate), ' ms'

  t3 = 0
  do j = 1, 1000
    b = 1.0_r8
    sync all
    if (this_image() == 1) call system_clock(t1)
    call a%solve(b)
    if (this_image() == 1) then
      call system_clock(t2)
      t3 = t3 + (t2 - t1)
    end if
  end do
  if (this_image() == 1) write(*,'(a,f6.3,a)') 'solve cpu =', real(t3)/real(rate), ' ms'

contains

  !! Fill with a random-ish M-matrix: negative off-diagonal elements,
  !! positive diagonal elements, and strictly diagonally dominant.

  subroutine matrix_fill(a)
    type(co_btd_matrix), intent(inout) :: a
    integer :: i, j
    call random_number(a%l)
    call random_number(a%d)
    call random_number(a%u)
    a%l = -a%l
    a%d = -a%d
    a%u = -a%u
    do j = 1, a%n
      do i = 1, a%nb
        a%d(i,i,j) = 3*a%nb
      end do
    end do
  end subroutine

end program
