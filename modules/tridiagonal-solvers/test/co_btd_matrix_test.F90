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

program co_btd_matrix_test

  use,intrinsic :: iso_fortran_env, only: r8 => real64
  use co_btd_matrix_type
  implicit none

  integer :: stat = 0, nb, ntotal
  character(16) :: arg

  call get_command_argument(1, arg)
  read(arg,*) nb

  call get_command_argument(2, arg)
  read(arg,*) ntotal

  if (this_image() == 1) then
    write(*,'(a,i0,a)') 'Using ', num_images(), ' images'
    write(*,'(2(a,i0))') 'block size ', nb, '; total size ', ntotal
  end if

  call test_non_periodic
  call test_periodic

  if (stat /= 0) error stop

contains

  subroutine test_non_periodic

    real(r8), allocatable :: x(:,:), b(:,:)
    type(co_btd_matrix) :: a

    integer :: n
    real(r8) :: error

    !! Partition the NTOTAL equations into nearly equal blocks
    n = ntotal/num_images()
    if (this_image() <= ntotal - n*num_images()) n = n + 1

    !! Initialize the tridiagonal matrix
    call a%init(nb, n)
    call matrix_fill(a)
    !call matrix_simple(a)
    
    !! Initialize the target solution
    allocate(x(nb,n))
    call random_number(x)

    !! Compute the corresponding RHS
    allocate(b(nb,n))
    call a%matvec(x, b)

    !! Solve the tridiagonal linear system; should recover X
    call a%factor
    call a%solve(b)
    error = maxval(abs(x-b))
    call co_max(error)
    call report('test_non_periodic', error, 1.0e-15_r8)

  end subroutine

  subroutine test_periodic

    real(r8), allocatable :: x(:,:), b(:,:)
    type(co_btd_matrix) :: a

    integer :: n
    real(r8) :: error

    !! Partition the NTOTAL equations into nearly equal blocks
    n = ntotal/num_images()
    if (this_image() <= ntotal - n*num_images()) n = n + 1

    !! Initialize the tridiagonal matrix
    call a%init(nb, n, periodic=.true.)
    call matrix_fill(a)
    
    !! Initialize the target solution
    allocate(x(nb,n))
    call random_number(x)

    !! Compute the corresponding RHS
    allocate(b(nb,n))
    call a%matvec(x, b)
    !! Solve the tridiagonal linear system; should recover X
    call a%factor
    call a%solve(b)
    error = maxval(abs(x-b))
    call co_max(error)
    call report('test_periodic', error, 1.0e-15_r8)

  end subroutine

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

  subroutine matrix_simple(a)
    type(co_btd_matrix), intent(inout) :: a
    integer :: i, j
    a%l = 0
    a%d = 0
    a%u = 0
    do j = 1, a%n
      do i = 1, a%nb
        a%l(i,i,j) = -1.0_r8
        a%d(i,i,j) =  3.0_r8
        a%u(i,i,j) = -1.0_r8
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
    if (this_image() == 1) &
        write(output_unit,'(3a,2(g0,a))') pf, name, ', error=', error, ' (tol=', tol, ')'
    sync all
  end subroutine

end program
