!!
!! Unit Tests for the TD_MATRIX_TYPE Module
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

program td_matrix_test

  use,intrinsic :: iso_fortran_env, only: r8 => real64
  use td_matrix_type
  implicit none

  integer :: stat = 0

  call test_non_periodic_2x2
  call test_non_periodic_gen
  call test_periodic_3x3
  call test_periodic_gen

  if (stat /= 0) stop 1

contains

  subroutine test_non_periodic_2x2
    type(td_matrix) :: a
    real(r8) :: x(2), b(2)
    call a%init(2)
    call conv_diff_fill(a, 0.5_r8, 0.1_r8)
    call random_number(x)
    call a%matvec(x, b)
    call a%factor
    call a%solve(b)
    call report('test_non_periodic_2x2', maxval(abs(x-b)), 1.0e-15_r8)
  end subroutine

  subroutine test_non_periodic_gen
    type(td_matrix) :: a
    integer, parameter :: N = 20
    real(r8) :: x(N), b(N)
    call a%init(N)
    call conv_diff_fill(a, 0.5_r8, 0.1_r8)
    call random_number(x)
    call a%matvec(x, b)
    call a%factor
    call a%solve(b)
    call report('test_non_periodic_gen', maxval(abs(x-b)), 1.0e-15_r8)
  end subroutine

  subroutine test_periodic_3x3
    type(td_matrix) :: a
    real(r8) :: x(3), b(3)
    call a%init(3, periodic=.true.)
    call conv_diff_fill(a, 0.5_r8, 0.1_r8)
    call random_number(x)
    call a%matvec(x, b)
    call a%factor
    call a%solve(b)
    call report('test_periodic_3x3', maxval(abs(x-b)), 1.0e-15_r8)
  end subroutine

  subroutine test_periodic_gen
    type(td_matrix) :: a
    integer, parameter :: N = 20
    real(r8) :: x(N), b(N)
    call a%init(N, periodic=.true.)
    call conv_diff_fill(a, 0.5_r8, 0.1_r8)
    call random_number(x)
    call a%matvec(x, b)
    call a%factor
    call a%solve(b)
    call report('test_periodic_gen', maxval(abs(x-b)), 1.0e-15_r8)
  end subroutine

  !! Fill the matrix with values proportional to a finite difference
  !! approximation to a 1D convection-diffusion operator.

  subroutine conv_diff_fill(a, s, t)
    type(td_matrix), intent(inout) :: a
    real(r8), intent(in) :: s, t
    real(r8) :: rn
    call random_number(rn)
    rn = (2*rn - 1)/10
    a%d =  (1+rn)*2.0_r8 + s
    a%l = -(1+rn)*(1.0_r8 + t)
    a%u = -(1+rn)*(1.0_r8 - t)
  end subroutine

  subroutine report(name, error, tol)
    use,intrinsic :: iso_fortran_env, only: output_unit
    character(*), intent(in) :: name
    real(r8), intent(in) :: error, tol
    character(6) :: pf
    if (error > tol) stat = stat + 1
    pf = merge('PASS: ', 'FAIL: ', error <= tol)
    write(output_unit,'(3a,2(g0,a))') pf, name, ', error=', error, ' (tol=', tol, ')'
  end subroutine

end program
