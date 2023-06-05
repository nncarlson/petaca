!!
!! BLOCK_SOLVER_PROCS
!!
!! This is a collection of low-level linear equation subroutines that are
!! intended for use by the various block linear solvers. The linear equations
!! are expected to be those corresponding to individual block equations: that
!! is, they involve just a few unknowns and have a dense, diagonally-dominant
!! coefficient matrix. There are subroutines to compute the LU factorization
!! of the coefficient matrix and for solving the linear system using the
!! factorization. There are also some matrix-vector and matrix-matrix
!! subroutines that are specialized versions of similar BLAS subroutines.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 1997, 2013. 2023  Neil N. Carlson
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

module block_solver_procs

  use,intrinsic :: iso_fortran_env, only: r8 => real64
  implicit none
  private

  public :: fct, slv, mslv, ymax, ypax, cmab

contains

  !! LU factorization of a square matrix.  No pivoting (intentionally).
  !! Unit lower triangular factor; unit diagonal not stored.  Reciprocal
  !! of upper triangular diagonal stored. Matrix is overwritten with the
  !! elements of L and U. I'm not aware of an equivalent LAPACK routine
  !! (no pivoting!)

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

  !! Solves the system Ax = b, where A stores its LU factorization. The RHS
  !! vector b is overwritten with the solution x.

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

  !! Solves the system AX=B where A stores its LU factorization, and the RHS
  !! B is a matrix (i.e., multiple RHS vectors). B is overwritten with the
  !! solution matrix X.

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

  !! Computes the matrix-vector update y <- y - Ax.
  !! Equivalent to gemv(a, x, y, alpha=-1.0_r8, beta=1.0_r8)

  pure subroutine ymax(y, a, x)
    real(r8), intent(inout) :: y(:)
    real(r8), intent(in) :: a(:,:), x(:)
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

  !! Computes the matrix-vector update y <- y + Ax.
  !! Equivalent to gemv(a, x, y, alpha=1.0_r8, beta=1.0_r8)

  pure subroutine ypax(y, a, x)
    real(r8), intent(inout) :: y(:)
    real(r8), intent(in) :: a(:,:), x(:)
    integer :: i, j
    real(r8) :: yi
    do i = 1, size(y)
      yi = y(i)
      do j = 1, size(x)
        yi = yi + a(i,j)*x(j)
      end do
      y(i) = yi
    end do
  end subroutine

  !! Computes the matrix-matrix update C <- C - AB.
  !! Equivalent to gemm(a, b, c, alpha=-1.0_r8, beta=1.0_r8)

  pure subroutine cmab(c, a, b)
    real(r8), intent(inout) :: c(:,:)
    real(r8), intent(in) :: a(:,:), b(:,:)
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

end module block_solver_procs
