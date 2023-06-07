!!
!! Unit Tests for the CO_TD_MATRIX_TYPE Module
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

program co_td_matrix_test

  use,intrinsic :: iso_fortran_env, only: r8 => real64
  use co_td_matrix_type
  implicit none

  integer :: stat = 0, ntotal
  character(16) :: arg

  call get_command_argument(1, arg)
  read(arg,*) ntotal

  if (this_image() == 1) then
    write(*,'(a,i0,a)') 'Using ', num_images(), ' images'
    write(*,'(a,i0)') 'total size ', ntotal
  end if

  call test_non_periodic
  call test_periodic

  if (stat /= 0) error stop

contains

  subroutine test_non_periodic

    real(r8), allocatable :: x(:), b(:)
    type(co_td_matrix) :: a

    integer  :: n
    real(r8) :: error

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
    allocate(b(n))
    call a%matvec(x, b)

    !! Solve the tridiagonal linear system; should recover X
    call a%factor
    call a%solve(b)
    error = maxval(abs(x-b))
    call co_max(error)
    call report('test_non_periodic', error, 1e-15_r8)

  end subroutine test_non_periodic

  subroutine test_periodic

    real(r8), allocatable :: x(:), b(:)
    type(co_td_matrix) :: a

    integer  :: n
    real(r8) :: error

    !! Partition the NTOTAL equations into nearly equal blocks
    n = ntotal/num_images()
    if (this_image() <= ntotal - n*num_images()) n = n + 1

    !! Initialize the periodic tridiagonal matrix
    call a%init(n, periodic=.true.)
    call conv_diff_fill(a, 0.5_r8, 0.1_r8)

    !! Initialize the target solution
    allocate(x(n))
    call random_number(x)

    !! Compute the corresponding RHS
    allocate(b(n))
    call a%matvec(x, b)

    !! Solve the tridiagonal linear system; should recover X
    call a%factor
    call a%solve(b)
    error = maxval(abs(x-b))
    call co_max(error)
    call report('test_periodic', error, 1e-15_r8)

  end subroutine test_periodic

  !! Fill the matrix with values proportional to a finite difference
  !! approximation to a 1D convection-diffusion operator.

  subroutine conv_diff_fill(a, s, t)
    type(co_td_matrix), intent(inout) :: a
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
    if (this_image() == 1) &
        write(output_unit,'(3a,2(g0,a))') pf, name, ', error=', error, ' (tol=', tol, ')'
    sync all
  end subroutine

end program
