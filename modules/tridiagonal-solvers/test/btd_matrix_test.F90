!!
!! Unit Tests for the BTD_MATRIX_TYPE Module
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

program btd_matrix_test

  use,intrinsic :: iso_fortran_env, only: r8 => real64
  use btd_matrix_type
  implicit none

  integer :: stat = 0

  call test_non_periodic
  call test_periodic

  if (stat /= 0) stop 1

contains

  subroutine test_non_periodic
    type(btd_matrix) :: a
    integer, parameter :: NB = 4, N = 20
    real(r8) :: x(NB,N), b(NB,N)
    call a%init(NB, N)
    call matrix_fill(a)
    call random_number(x)
    call a%matvec(x, b)
    call a%factor
    call a%solve(b)
    call report('test_non_periodic', maxval(abs(x-b)), 1.0e-15_r8)
  end subroutine

  subroutine test_periodic
    type(btd_matrix) :: a
    integer, parameter :: NB = 4, N = 20
    real(r8) :: x(NB,N), b(NB,N)
    call a%init(NB, N, periodic=.true.)
    call matrix_fill(a)
    call random_number(x)
    call a%matvec(x, b)
    call a%factor
    call a%solve(b)
    call report('test_periodic', maxval(abs(x-b)), 1.0e-15_r8)
  end subroutine

  !! Fill with a random-ish M-matrix: negative off-diagonal elements,
  !! positive diagonal elements, and strictly diagonally dominant.

  subroutine matrix_fill(a)
    type(btd_matrix), intent(inout) :: a
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
