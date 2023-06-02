!!
!! BTD_MATRIX_TYPE
!!
!! A data structure for block tridiagonal matrices. Includes linear solver and
!! matrix-vector product methods. Provides support for periodic tridiagonal
!! matrices as well. Uses direct LU factorization WITHOUT PIVOTING and is thus
!! only suitable for classes of matrices not requiring pivoting.
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

module btd_matrix_type

  use,intrinsic :: iso_fortran_env, only: r8 => real64
  implicit none
  private

  type, public :: btd_matrix
    integer :: m, n
    logical :: periodic = .false.
    real(r8), allocatable :: l(:,:,:), d(:,:,:), u(:,:,:)
    real(r8), allocatable, private :: w(:,:,:)
  contains
    procedure :: init
    procedure :: factor
    procedure :: solve
    procedure :: matvec
    procedure :: factor_submatrix
    procedure :: solve_submatrix
  end type

contains

  subroutine init(this, m, n, periodic)
    class(btd_matrix), intent(out) :: this
    integer, intent(in) :: m, n
    logical, intent(in), optional :: periodic
    this%m = m
    this%n = n
    if (present(periodic)) this%periodic = periodic
    if (this%periodic .and. n < 3) &
        error stop 'btd_matrix%init: periodic matrix size must be >= 3'
    allocate(this%l(m,m,n))
    allocate(this%d, this%u, mold=this%l)
  end subroutine

  pure subroutine factor(this)
    class(btd_matrix), intent(inout) :: this
    if (this%periodic) then
      call factor_periodic(this)
    else
      call factor_submatrix(this, 1, this%n)
    end if
  end subroutine

  pure subroutine factor_submatrix(this, j1, j2)
    class(btd_matrix), intent(inout) :: this
    integer, intent(in) :: j1, j2
    integer :: j
    call fct(this%d(:,:,1))
    do j = j1+1, j2
      call mslv(this%d(:,:,j-1), this%u(:,:,j-1))
      call cmab(this%d(:,:,j), this%l(:,:,j), this%u(:,:,j-1))
      call fct(this%d(:,:,j))
    end do
  end subroutine

  pure subroutine factor_periodic(this)
    class(btd_matrix), intent(inout) :: this
    associate (m => this%m, n => this%n)
      call factor_submatrix(this, 1, n-1)
      allocate(this%w(m,m,n-1))
      this%w(:,:,1) = this%l(:,:,1)
      this%w(:,:,2:n-2) = 0.0_r8
      this%w(:,:,n-1) = this%u(:,:,n-1)
      call msolve_submatrix(this, 1, n-1, this%w)
      call cmab(this%d(:,:,n), this%u(:,:,n), this%w(:,:,1))
      call cmab(this%d(:,:,n), this%l(:,:,n), this%w(:,:,n-1))
      call fct(this%d(:,:,n))
    end associate
  end subroutine

  pure subroutine solve(this, b)
    class(btd_matrix), intent(in) :: this
    real(r8), intent(inout) :: b(:,:)
    if (this%periodic) then
      call solve_periodic(this, b)
    else
      call solve_submatrix(this, 1, this%n, b)
    end if
  end subroutine

  pure subroutine solve_submatrix(this, j1, j2, b)
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

  pure subroutine msolve_submatrix(this, j1, j2, b)
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

  pure subroutine solve_periodic(this, b)
    class(btd_matrix), intent(in) :: this
    real(r8), intent(inout) :: b(:,:)
    integer :: j
    associate (n => this%n)
      call this%solve_submatrix(1, n-1, b)
      call ymax(b(:,n), this%u(:,:,n), b(:,1))
      call ymax(b(:,n), this%l(:,:,n), b(:,n-1))
      call slv(this%d(:,:,n), b(:,n))
      do j = 1, n-1
        call ymax(b(:,j), this%w(:,:,j), b(:,n))
      end do
    end associate
  end subroutine

  pure subroutine matvec(this, x, y)

    class(btd_matrix), intent(in) :: this
    real(r8), intent(in) :: x(:,:)
    real(r8), intent(out) :: y(:,:)

    integer :: j

    associate (l => this%l, d => this%d, u => this%u)
      if (this%periodic) then
        call matmul3(this%m, l(:,:,1), x(:,this%n), d(:,:,1), x(:,1), u(:,:,1), x(:,2), y(:,1))
      else
        call matmul2(this%m, d(:,:,1), x(:,1), u(:,:,1), x(:,2), y(:,1))
      end if
      do j = 2, this%n-1
        call matmul3(this%m, l(:,:,j), x(:,j-1), d(:,:,j), x(:,j), u(:,:,j), x(:,j+1), y(:,j))
      end do
      if (this%periodic) then
        call matmul3(this%m, l(:,:,j), x(:,j-1), d(:,:,j), x(:,j), u(:,:,j), x(:,1), y(:,j))
      else
        call matmul2(this%m, l(:,:,j), x(:,j-1), d(:,:,j), x(:,j), y(:,j))
      end if
    end associate

  contains

    pure subroutine matmul2(n, a1, x1, a2, x2, y)
      integer, intent(in) :: n
      real(r8), intent(in)  :: a1(n,n), x1(n), a2(n,n), x2(n)
      real(r8), intent(out) :: y(n)
      integer :: i, j
      real(r8) :: yi
      do i = 1, n
        yi = 0.0_r8
        do j = 1, n
          yi = yi + a1(i,j)*x1(j) + a2(i,j)*x2(j)
        end do
        y(i) = yi
      end do
    end subroutine

    pure subroutine matmul3(n, a1, x1, a2, x2, a3, x3, y)
      integer, intent(in) :: n
      real(r8), intent(in)  :: a1(n,n), x1(n), a2(n,n), x2(n), a3(n,n), x3(n)
      real(r8), intent(out) :: y(n)
      integer :: i, j
      real(r8) :: yi
      do i = 1, n
        yi = 0.0_r8
        do j = 1, n
          yi = yi + a1(i,j)*x1(j) + a2(i,j)*x2(j) + a3(i,j)*x3(j)
        end do
        y(i) = yi
      end do
    end subroutine

  end subroutine

  !! LU factorization of a square matrix.  No pivoting (intentionally).
  !! Unit lower triangular factor; unit diagonal not stored.  Reciprocal
  !! of upper triangular diagonal stored.

  pure subroutine fct(a)

    real(r8), intent(inout) :: a(:,:)

    integer :: n, i, j, k
    real(r8) :: lkk, lkj, ujk

    n = size(a,1)
    select case (n)
    case (2)

      a(1,1) = 1.0_r8 / a(1,1)
      a(2,1) = a(2,1) * a(1,1)
      a(2,2) = 1.0_r8 / (a(2,2) - a(2,1)*a(1,2))

    case default

      a(1,1) = 1.0_r8 / a(1,1)
      do k = 2, n
        lkk = a(k,k)
        do j = 1, k - 1
          lkj = a(k,j)
          ujk = a(j,k)
          do i = 1, j - 1
            lkj = lkj - a(k,i)*a(i,j)
            ujk = ujk - a(j,i)*a(i,k)
          end do
          lkj = lkj * a(j,j)
          lkk = lkk - lkj*ujk
          a(k,j) = lkj
          a(j,k) = ujk
        end do
        a(k,k) = 1.0_r8 / lkk
      end do

    end select

  end subroutine fct

  pure subroutine slv(a, b)

    real(r8), intent(in) :: a(:,:)
    real(r8), intent(inout) :: b(:)

    integer  :: n, i, j
    real(r8) :: bj

    n = size(a,1)
    select case (n)
    case (2)

      b(2) = (b(2) - a(2,1)*b(1))*a(2,2)
      b(1) = (b(1) - a(1,2)*b(2))*a(1,1)

    case default

      do j = 2, n
        bj = b(j)
        do i = 1, j-1
          bj = bj - a(j,i)*b(i)
        end do
        b(j) = bj
      end do
      b(n) = b(n) * a(n,n)
      do j = n-1, 1, -1
        bj = b(j)
        do i = j+1, n
          bj = bj - a(j,i)*b(i)
        end do
        b(j) = bj * a(j,j)
      end do

    end select

  end subroutine slv


  pure subroutine mslv(a, b)

    real(r8), intent(in) :: a(:,:)
    real(r8), intent(inout) :: b(:,:)

    integer  :: n, i, j
    real(r8) :: bj(size(b,2))

    n = size(a,1)
    select case (n)
    case (2)

      b(2,:) = (b(2,:) - a(2,1)*b(1,:)) * a(2,2)
      b(1,:) = (b(1,:) - a(1,2)*b(2,:)) * a(1,1)

    case default

      do j = 2, n
        bj = b(j,:)
        do i = 1, j-1
          bj = bj - a(j,i)*b(i,:)
        end do
        b(j,:) = bj
      end do
      b(n,:) = b(n,:) * a(n,n)
      do j = n-1, 1, -1
        bj = b(j,:)
        do i = j+1, n
          bj = bj - a(j,i)*b(i,:)
        end do
        b(j,:) = bj * a(j,j)
      end do

    end select

  end subroutine mslv

  !! gemv(a, x, y, alpha=-1.0_r8, beta=1.0_r8)
  pure subroutine ymax(y, a, x)
    real(r8), contiguous, intent(inout) :: y(:)
    real(r8), contiguous, intent(in) :: a(:,:), x(:)
    integer :: i, j
    real(r8) :: yi
    do i = 1, size(y)
      yi = y(i)
      do j = 1, size(x)
        yi = yi - a(i,j)*x(j)
      end do
      y(i) = yi
    end do
  end subroutine

  !! gemm(a, b, c, alpha=-1.0_r8, beta=1.0_r8)
  pure subroutine cmab(c, a, b)
    real(r8), contiguous, intent(inout) :: c(:,:)
    real(r8), contiguous, intent(in) :: a(:,:), b(:,:)
    integer :: i, j, k
    real(r8) :: cjk
    do k = 1, size(c,2)
      do j = 1, size(c,1)
        cjk = c(j,k)
        do i = 1, size(a,2)
          cjk = cjk - a(j,i)*b(i,k)
        end do
        c(j,k) = cjk
      end do
    end do
  end subroutine

end module btd_matrix_type
