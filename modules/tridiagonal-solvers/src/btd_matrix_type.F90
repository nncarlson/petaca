!!
!! BTD_MATRIX_TYPE
!!
!! A data structure for block tridiagonal matrices with methods for linear
!! equations and matrix-vector products. Supports periodic block tridiagonal
!! matrices as well. Linear equation solution uses direct LU factorization
!! without pivoting and is thus only suitable for classes of matrices not
!! requiring pivoting, such as diagonally-dominant matrices.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2013, 2023  Neil N. Carlson
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

module btd_matrix_type

  use,intrinsic :: iso_fortran_env, only: r8 => real64
  implicit none
  private

  type, public :: btd_matrix
    integer :: nb ! block size
    integer :: n  ! number of block rows/columns
    logical :: periodic = .false.
    real(r8), allocatable :: l(:,:,:), d(:,:,:), u(:,:,:)
    real(r8), allocatable :: q(:,:,:) ! factorization fill-in (periodic case)
  contains
    procedure :: init
    procedure :: factor
    procedure :: solve
    procedure :: matvec
    ! auxiliary procedures; available to type extensions
    procedure :: factor_submatrix
    procedure :: solve_submatrix
    procedure :: msolve_submatrix
  end type

contains

  subroutine init(this, nb, n, periodic)
    class(btd_matrix), intent(out) :: this
    integer, intent(in) :: nb, n
    logical, intent(in), optional :: periodic
    this%n = n
    this%nb = nb
    if (present(periodic)) this%periodic = periodic
    if (nb < 2) error stop 'btd_matrix%init: block size < 2'
    if (n  < 2) error stop 'btd_matrix%init: matrix size < 2'
    if (this%periodic .and. n < 3) error stop 'btd_matrix%init: periodic matrix < 3'
    allocate(this%l(nb,nb,n), this%d(nb,nb,n), this%u(nb,nb,n))
  end subroutine

  subroutine factor(this)
    class(btd_matrix), intent(inout) :: this
    if (this%periodic) then
      call factor_periodic(this)
    else
      call factor_submatrix(this, 1, this%n)
    end if
  end subroutine

  pure subroutine factor_periodic(this)
    use block_solver_procs, only: fct, cmab
    class(btd_matrix), intent(inout) :: this
    associate (nb => this%nb, n => this%n)
      call factor_submatrix(this, 1, n-1)
      allocate(this%q(nb,nb,n-1))
      this%q(:,:,1) = this%l(:,:,1)
      this%q(:,:,2:n-2) = 0.0_r8
      this%q(:,:,n-1) = this%u(:,:,n-1)
      call msolve_submatrix(this, 1, n-1, this%q)
      call cmab(this%d(:,:,n), this%u(:,:,n), this%q(:,:,1))
      call cmab(this%d(:,:,n), this%l(:,:,n), this%q(:,:,n-1))
      call fct(this%d(:,:,n))
    end associate
  end subroutine

  !! This auxiliary subroutine computes the usual LU factorization of the
  !! local submatrix composed of rows/columns j1 through j2. The elements
  !! of the local submatrix are overwritten with the elements of L and unit
  !! upper triangular U.

  pure subroutine factor_submatrix(this, j1, j2)
    use block_solver_procs, only: fct, mslv, cmab
    class(btd_matrix), intent(inout) :: this
    integer, intent(in) :: j1, j2
    integer :: j
    call fct(this%d(:,:,j1))
    do j = j1+1, j2
      call mslv(this%d(:,:,j-1), this%u(:,:,j-1))
      call cmab(this%d(:,:,j), this%l(:,:,j), this%u(:,:,j-1))
      call fct(this%d(:,:,j))
    end do
  end subroutine

  subroutine solve(this, b)
    class(btd_matrix), intent(in) :: this
    real(r8), intent(inout) :: b(:,:)
    if (this%periodic) then
      call solve_periodic(this, b)
    else
      call solve_submatrix(this, 1, this%n, b)
    end if
  end subroutine

  pure subroutine solve_periodic(this, b)
    use block_solver_procs, only: slv, ymax
    class(btd_matrix), intent(in) :: this
    real(r8), intent(inout) :: b(:,:)
    integer :: j
    associate (n => this%n)
      call this%solve_submatrix(1, n-1, b)
      call ymax(b(:,n), this%u(:,:,n), b(:,1))
      call ymax(b(:,n), this%l(:,:,n), b(:,n-1))
      call slv(this%d(:,:,n), b(:,n))
      do j = 1, n-1
        call ymax(b(:,j), this%q(:,:,j), b(:,n))
      end do
    end associate
  end subroutine

  !! This auxiliary subroutine solves the linear system Ax = b where A is
  !! the submatrix composed of rows/columns j1 through j2. The submatrix
  !! must store the LU factorization computed by SERIAL_FACTOR. The RHS b
  !! is the subvector of the passed B composed of elements j1 through j2,
  !! and the computed solution overwrites those elements. Other elements
  !! of B are unmodified.

  pure subroutine solve_submatrix(this, j1, j2, b)
    use block_solver_procs, only: slv, ymax
    class(btd_matrix), intent(in) :: this
    integer, intent(in) :: j1, j2
    real(r8), intent(inout) :: b(:,:)
    integer :: j
    call slv(this%d(:,:,j1), b(:,j1))
    do j = j1+1, j2
      call ymax(b(:,j), this%l(:,:,j), b(:,j-1))
      call slv(this%d(:,:,j), b(:,j))
    end do
    do j = j2-1, j1, -1
      call ymax(b(:,j), this%u(:,:,j), b(:,j+1))
    end do
  end subroutine

  !! Similar to SOLVE_SUBMATRIX, this auxiliary subroutine solves the linear
  !! system AX = B where A is the submatrix composed of rows/columns j1
  !! through j2. The RHS B is the subvector of the passed block vector B
  !! composed of elements j1 through j2, and the computed solution overwrites
  !! those elements. Other elements of B are unmodified.

  pure subroutine msolve_submatrix(this, j1, j2, b)
    use block_solver_procs, only: mslv, cmab
    class(btd_matrix), intent(in) :: this
    integer, intent(in) :: j1, j2
    real(r8), intent(inout) :: b(:,:,:)
    integer :: j
    call mslv(this%d(:,:,j1), b(:,:,j1))
    do j = j1+1, j2
      call cmab(b(:,:,j), this%l(:,:,j), b(:,:,j-1))
      call mslv(this%d(:,:,j), b(:,:,j))
    end do
    do j = j2-1, j1, -1
      call cmab(b(:,:,j), this%u(:,:,j), b(:,:,j+1))
    end do
  end subroutine

  subroutine matvec(this, x, y)

    use block_solver_procs, only: ypax

    class(btd_matrix), intent(in) :: this
    real(r8), intent(in) :: x(:,:)
    real(r8), intent(out) :: y(:,:)

    integer :: j

    y(:,1) = 0.0_r8
    call ypax(y(:,1), this%d(:,:,1), x(:,1))
    call ypax(y(:,1), this%u(:,:,1), x(:,2))
    if (this%periodic) call ypax(y(:,1), this%l(:,:,1), x(:,this%n))

    do j = 2, this%n-1
      y(:,j) = 0.0_r8
      call ypax(y(:,j), this%l(:,:,j), x(:,j-1))
      call ypax(y(:,j), this%d(:,:,j), x(:,j))
      call ypax(y(:,j), this%u(:,:,j), x(:,j+1))
    end do

    y(:,j) = 0.0_r8
    call ypax(y(:,j), this%l(:,:,j), x(:,j-1))
    call ypax(y(:,j), this%d(:,:,j), x(:,j))
    if (this%periodic) call ypax(y(:,j), this%u(:,:,j), x(:,1))

  end subroutine

end module btd_matrix_type
