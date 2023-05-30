!!
!! Unit Test for the CO_TD_MATRIX_TYPE Module
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

program main

  use,intrinsic :: iso_fortran_env, only: r8 => real64, int64
  use co_td_matrix_type
  implicit none

  integer, parameter :: ntotal = 100000
  real(r8), allocatable :: x(:), y(:)
  type(co_td_matrix) :: a

  integer  :: n
  real(r8) :: error
  integer(int64) :: t1, t2, rate

  if (this_image() == 1) write(*,'(a,i0,a)') 'Using ', num_images(), ' images'

  !! Partition the NTOTAL equations into nearly equal blocks
  n = ntotal/num_images()
  if (this_image() <= ntotal - n*num_images()) n = n + 1

  !! Initialize the tridiagonal matrix
  call a%init(n)
  call conv_diff_fill(a, 0.5_r8, 0.1_r8)

  !! Initialize the target solution
  allocate(x(n))
  call random_number(x)

  !! Compute the corresponding RHS
  allocate(y(n))
  call a%matvec(x, y)

  !! Solve the tridiagonal linear system; should recover X
  sync all
  if (this_image() == 1) call system_clock(t1)
  call a%factor
  call a%solve(y)
  if (this_image() == 1) call system_clock(t2, rate)

  error = maxval(abs(x-y))
  call co_max(error, 1)
  if (this_image() == 1) &
      write(*,'(2(a,g0),a)') 'error=', error, '; cpu=', real(t2-t1)/real(rate), ' sec'

  sync all
  if (error > 1e-15_r8) error stop

contains

  !! Fill the matrix with values proportional to a finite difference
  !! approximation to a 1D convection-diffusion operator.

  subroutine conv_diff_fill(a, s, t)
    type(co_td_matrix), intent(inout) :: a
    real(r8), intent(in) :: s, t
    a%d = 2.0_r8 + s
    a%l = -1.0_r8 - t
    a%u = -1.0_r8 + t
  end subroutine

end program
