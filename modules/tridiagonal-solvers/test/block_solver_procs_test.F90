!!
!! Unit Tests for the BLOCK_SOLVER_PROCS Module
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

program block_solver_procs_test

  use,intrinsic :: iso_fortran_env, only: r8 => real64
  use block_solver_procs
  implicit none

  integer :: stat = 0

  call test_fct_slv_small
  call test_fct_slv_gen
  call test_mslv_small
  call test_mslv_gen
  call test_ymax
  call test_ypax
  call test_cmab

  if (stat /= 0) stop 1

contains

  subroutine test_fct_slv_gen
    real(r8) :: a(5,5), b(5), x(5)
    call fill_matrix(a)
    call random_number(x)
    b = matmul(a, x)
    call fct(a)
    call slv(a, b)
    call report('test_fct_slv_gen', maxval(abs(x-b)), 1.0e-15_r8)
  end subroutine

  subroutine test_fct_slv_small
    real(r8) :: a(2,2), b(2), x(2)
    call fill_matrix(a)
    call random_number(x)
    b = matmul(a, x)
    call fct(a)
    call slv(a, b)
    call report('test_fct_slv_small', maxval(abs(x-b)), 1.0e-15_r8)
  end subroutine

  subroutine test_mslv_gen
    real(r8) :: a(5,5), b(5,6), x(5,6)
    call fill_matrix(a)
    call random_number(x)
    b = matmul(a, x)
    call fct(a)
    call mslv(a, b)
    call report('test_mslv_gen', maxval(abs(x-b)), 1.0e-15_r8)
  end subroutine

  subroutine test_mslv_small
    real(r8) :: a(2,2), b(2,3), x(2,3)
    call fill_matrix(a)
    call random_number(x)
    b = matmul(a, x)
    call fct(a)
    call mslv(a, b)
    call report('test_mslv_small', maxval(abs(x-b)), 1.0e-15_r8)
  end subroutine

  subroutine test_ymax
    real(r8) :: a(3,3), x(3), y(3), z(3)
    call random_number(a)
    call random_number(x)
    call random_number(y)
    z = y
    call ymax(y, a, x)
    z = z - matmul(a, x)
    call report('test_ymax', maxval(abs(y-z)), 1.0e-15_r8)
  end subroutine

  subroutine test_ypax
    real(r8) :: a(3,3), x(3), y(3), z(3)
    call random_number(a)
    call random_number(x)
    call random_number(y)
    z = y
    call ypax(y, a, x)
    z = z + matmul(a, x)
    call report('test_ypax', maxval(abs(y-z)), 1.0e-15_r8)
  end subroutine

  subroutine test_cmab
    real(r8) :: a(3,4), b(4,2), c(3,2), d(3,2)
    call random_number(a)
    call random_number(b)
    call random_number(c)
    d = c
    call cmab(c, a, b)
    d = d - matmul(a, b)
    call report('test_cmab', maxval(abs(c-d)), 1.0e-15_r8)
  end subroutine

  !! Fill with a random-ish M-matrix: negative off-diagonal elements,
  !! positive diagonal elements, and strictly diagonally dominant.

  subroutine fill_matrix(a)
    real(r8), intent(out) :: a(:,:)
    integer :: i
    call random_number(a)
    a = -a
    do i = 1, size(a,2)
      a(i,i) = size(a,2)
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
