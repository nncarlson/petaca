!!
!! TD_MATRIX_TYPE
!!
!! A data structure for tridiagonal matrices, with methods for linear equations
!! and matrix-vector products. Supports periodic tridiagonal matrices as well.
!! Linear equation solution uses direct LU factorization without pivoting and
!! is thus only suitable for classes of matrices not requiring pivoting, such
!! as diagonally-dominant matrices.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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

module td_matrix_type

  use,intrinsic :: iso_fortran_env, only: r8 => real64
  implicit none
  private

  type, public :: td_matrix
    integer :: n  ! number of rows/columns
    logical :: periodic = .false.
    real(r8), allocatable :: l(:), d(:), u(:)
    real(r8), allocatable :: q(:) ! factorization fill-in (periodic case)
  contains
    procedure :: init
    procedure :: factor
    procedure :: solve
    procedure :: matvec
    ! auxiliary procedures; available to type extensions
    procedure :: factor_submatrix
    procedure :: solve_submatrix
  end type

contains

  subroutine init(this, n, periodic)
    class(td_matrix), intent(out) :: this
    integer, intent(in) :: n
    logical, intent(in), optional :: periodic
    this%n = n
    if (present(periodic)) this%periodic = periodic
    if (n < 2) error stop 'td_matrix%init: matrix size < 2'
    if (this%periodic .and. n < 3) error stop 'td_matrix%init: periodic matrix size < 3'
    allocate(this%l(n), this%d(n), this%u(n))
  end subroutine

  subroutine factor(this)
    class(td_matrix), intent(inout) :: this
    if (this%periodic) then
      call factor_periodic(this)
    else
      call factor_submatrix(this, 1, this%n)
    end if
  end subroutine

  pure subroutine factor_periodic(this)
    class(td_matrix), intent(inout) :: this
    associate (n => this%n)
      call this%factor_submatrix(1, n-1)
      allocate(this%q(n-1))
      this%q(1) = this%l(1)
      this%q(2:n-2) = 0.0_r8
      this%q(n-1) = this%u(n-1)
      call this%solve_submatrix(1, n-1, this%q)
      this%d(n) = 1.0_r8/(this%d(n) - this%u(n)*this%q(1) - this%l(n)*this%q(n-1))
    end associate
  end subroutine

  !! This auxiliary subroutine computes the usual LU factorization of the
  !! local submatrix composed of rows/columns j1 through j2. The elements
  !! of the local submatrix are overwritten with the elements of L and unit
  !! upper triangular U.

  pure subroutine factor_submatrix(this, j1, j2)
    class(td_matrix), intent(inout) :: this
    integer, intent(in) :: j1, j2
    integer :: j
    this%d(j1) = 1.0_r8/this%d(j1)
    do j = j1+1, j2
      this%u(j-1) = this%d(j-1)*this%u(j-1)
      this%d(j) = 1.0_r8/(this%d(j) - this%l(j)*this%u(j-1))
    end do
  end subroutine

  subroutine solve(this, b)
    class(td_matrix), intent(in) :: this
    real(r8), intent(inout) :: b(:)
    if (this%periodic) then
      call solve_periodic(this, b)
    else
      call solve_submatrix(this, 1, this%n, b)
    end if
  end subroutine

  pure subroutine solve_periodic(this, b)
    class(td_matrix), intent(in) :: this
    real(r8), intent(inout) :: b(:)
    associate (n => this%n)
      call this%solve_submatrix(1, n-1, b)
      b(n) = this%d(n)*(b(n) - this%u(n)*b(1) - this%l(n)*b(n-1))
      b(1:n-1) = b(1:n-1) - b(n)*this%q
    end associate
  end subroutine

  !! This auxiliary subroutine solves the linear system Ax = b where A is
  !! the submatrix composed of rows/columns j1 through j2. The submatrix
  !! must store the LU factorization computed by SERIAL_FACTOR. The RHS b
  !! is the subvector of the passed B composed of elements j1 through j2,
  !! and the computed solution overwrites those elements. Other elements
  !! of B are unmodified.

  pure subroutine solve_submatrix(this, j1, j2, b)
    class(td_matrix), intent(in) :: this
    integer, intent(in) :: j1, j2
    real(r8), intent(inout) :: b(:)
    integer :: j
    b(j1) = this%d(j1)*b(j1)
    do j = j1+1, j2
      b(j) = this%d(j)*(b(j) - this%l(j)*b(j-1))
    end do
    do j = j2-1, j1, -1
      b(j) = b(j) - this%u(j)*b(j+1)
    end do
  end subroutine

  subroutine matvec(this, x, y)
    class(td_matrix), intent(in) :: this
    real(r8), intent(in) :: x(:)
    real(r8), intent(out) :: y(:)
    integer :: j
    y(1) = this%d(1)*x(1) + this%u(1)*x(2)
    if (this%periodic) y(1) = y(1) + this%l(1)*x(this%n)
    do j = 2, this%n-1
      y(j) = this%l(j)*x(j-1) + this%d(j)*x(j) + this%u(j)*x(j+1)
    end do
    y(j) = this%l(j)*x(j-1) + this%d(j)*x(j)
    if (this%periodic) y(j) = y(j) + this%u(j)*x(1)
  end subroutine

end module td_matrix_type
