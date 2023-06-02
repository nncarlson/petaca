#include "f90_assert.fpp"

module block_linear_solver

  use,intrinsic :: iso_fortran_env, only: r8 => real64
  implicit none
  private

  public :: vfct, vslv, vmslv
  public :: fct, slv, mslv
  public :: ymax, cmab

contains

  !! LU factorization of a square matrix.  No pivoting (intentionally).
  !! Unit lower triangular factor; unit diagonal not stored.  Reciprocal
  !! of upper triangular diagonal stored.

  subroutine fct(a)

    real(r8), intent(inout) :: a(:,:)

    integer :: n, i, j, k
    real(r8) :: lkk, lkj, ujk

    ASSERT(size(a,1) == size(a,2))

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

  subroutine vfct(a)

    real(r8), intent(inout) :: a(:,:,:)

    integer :: n, i, j, k, p
    real(r8) :: lkk, lkj, ujk

    ASSERT(size(a,1) == size(a,2))

    n = size(a,1)
    select case (n)
    case (2)

      do p = 1, size(a,3)
        a(1,1,p) = 1.0_r8 / a(1,1,p)
        a(2,1,p) = a(2,1,p) * a(1,1,p)
        a(2,2,p) = 1.0_r8 / (a(2,2,p) - a(2,1,p)*a(1,2,p))
      end do

    case default

      do p = 1, size(a,3)
        a(1,1,p) = 1.0_r8 / a(1,1,p)
        do k = 2, n
          lkk = a(k,k,p)
          do j = 1, k - 1
            lkj = a(k,j,p)
            ujk = a(j,k,p)
            do i = 1, j - 1
              lkj = lkj - a(k,i,p)*a(i,j,p)
              ujk = ujk - a(j,i,p)*a(i,k,p)
            end do
            lkj = lkj * a(j,j,p)
            lkk = lkk - lkj*ujk
            a(k,j,p) = lkj
            a(j,k,p) = ujk
          end do
          a(k,k,p) = 1.0_r8 / lkk
        end do
      end do

    end select

  end subroutine vfct


  subroutine slv(a, b)

    real(r8), intent(in) :: a(:,:)
    real(r8), intent(inout) :: b(:)

    integer  :: n, i, j
    real(r8) :: bj

    ASSERT(size(a,1) == size(a,2))
    ASSERT(size(b) == size(a,1))

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


  subroutine mslv(a, b)

    real(r8), intent(in) :: a(:,:)
    real(r8), intent(inout) :: b(:,:)

    integer  :: n, i, j
    real(r8) :: bj(size(b,2))

    ASSERT(size(a,1) == size(a,2))
    ASSERT(size(b,1) == size(a,1))

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


  subroutine vslv(a, b)

    real(r8), intent(in) :: a(:,:,:)
    real(r8), intent(inout) :: b(:,:)

    integer  :: n, i, j, p
    real(r8) :: bj

    ASSERT(size(a,1) == size(a,2))
    ASSERT(size(b,1) == size(a,2))
    ASSERT(size(a,3) == size(b,2))

    n = size(a,1)
    select case (n)
    case (2)

      do p = 1, size(a,3)
        b(2,p) = (b(2,p) - a(2,1,p)*b(1,p))*a(2,2,p)
        b(1,p) = (b(1,p) - a(1,2,p)*b(2,p))*a(1,1,p)
      end do

    case default

      do p = 1, size(a,3)
        do j = 2, n
          bj = b(j,p)
          do i = 1, j-1
            bj = bj - a(j,i,p)*b(i,p)
          end do
          b(j,p) = bj
        end do
        b(n,p) = b(n,p) * a(n,n,p)
        do j = n-1, 1, -1
          bj = b(j,p)
          do i = j+1, n
            bj = bj - a(j,i,p)*b(i,p)
          end do
          b(j,p) = bj * a(j,j,p)
        end do
      end do

    end select

  end subroutine vslv


  subroutine vmslv(a, b)

    real(r8), intent(in) :: a(:,:,:)
    real(r8), intent(inout) :: b(:,:,:)

    integer  :: n, i, j, l
    real(r8) :: bj(size(b,2))

    ASSERT(size(a,1) == size(a,2))
    ASSERT(size(b,1) == size(a,1))
    ASSERT(size(a,3) == size(b,3))

    n = size(a,1)
    select case (n)
    case (2)

      do l = 1, size(a,dim=3)
        b(2,:,l) = (b(2,:,l) - a(2,1,l)*b(1,:,l)) * a(2,2,l)
        b(1,:,l) = (b(1,:,l) - a(1,2,l)*b(2,:,l)) * a(1,1,l)
      end do

    case default

      do l = 1, size(a,dim=3)
        do j = 2, n
          bj = b(j,:,l)
          do i = 1, j-1
            bj = bj - a(j,i,l)*b(i,:,l)
          end do
          b(j,:,l) = bj
        end do
        b(n,:,l) = b(n,:,l) * a(n,n,l)
        do j = n-1, 1, -1
          bj = b(j,:,l)
          do i = j+1, n
            bj = bj - a(j,i,l)*b(i,:,l)
          end do
          b(j,:,l) = bj * a(j,j,l)
        end do
      end do

    end select

  end subroutine vmslv

  !! gemv(a, x, y, alpha=-1.0_r8, beta=1.0_r8)

  subroutine ymax(y, a, x)

    real(r8), intent(inout) :: y(:)
    real(r8), intent(in) :: a(:,:), x(:)

    integer :: i, j
    real(r8) :: yi

    ASSERT(size(y) == size(a,1))
    ASSERT(size(x) == size(a,2))

    do i = 1, size(y)
      yi = y(i)
      do j = 1, size(x)
        yi = yi - a(i,j)*x(j)
      end do
      y(i) = yi
    end do

  end subroutine ymax

  !! gemm(a, b, c, alpha=-1.0_r8, beta=1.0_r8)

  subroutine cmab(c, a, b)

    real(r8), intent(inout) :: c(:,:)
    real(r8), intent(in) :: a(:,:), b(:,:)

    integer :: i, j, k
    real(r8) :: cjk

    ASSERT(size(c,1) == size(a,1))
    ASSERT(size(a,2) == size(b,1))
    ASSERT(size(c,2) == size(b,2))

    do k = 1, size(c,2)
      do j = 1, size(c,1)
        cjk = c(j,k)
        do i = 1, size(a,2)
          cjk = cjk - a(j,i)*b(i,k)
        end do
        c(j,k) = cjk
      end do
    end do

  end subroutine cmab

end module block_linear_solver
