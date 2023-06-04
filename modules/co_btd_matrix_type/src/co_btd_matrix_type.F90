!!
!! CO_BTD_MATRIX_TYPE
!!
!! A parallel data structure for block tridiagonal matrices that is implemented
!! using coarrays. Includes linear solver and matrix-vector product methods.
!! Supports periodic block tridiagonal matrices as well.
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

module co_btd_matrix_type

  use,intrinsic :: iso_fortran_env, only: r8 => real64
  implicit none
  private

  !! This auxiliary type stores the block tridiagonal Schur complement matrix
  !! that results from the local elimination of the interior block rows on
  !! each image. Each block row of the matrix is stored by a different image.

  type, private :: schur_matrix
    integer :: nb, n
    logical :: periodic = .false.
    real(r8), dimension(:,:), allocatable :: l, d, u, w
  contains
    procedure :: init => init_schur
    procedure :: factor => factor_schur
    procedure :: solve => solve_schur
  end type

  type, public :: co_btd_matrix
    private
    integer, public :: nb, n
    logical :: periodic = .false.
    real(r8), allocatable, public :: l(:,:,:), d(:,:,:), u(:,:,:)
    ! LU decomposition fill-in and Schur complement
    real(r8), allocatable, public :: p(:,:,:), q(:,:,:)
    type(schur_matrix), public :: dhat
  contains
    procedure :: init
    procedure :: factor
    procedure :: solve
    procedure :: matvec
  end type

contains

  subroutine init(this, nb, n, periodic)

    class(co_btd_matrix), intent(out) :: this
    integer, intent(in) :: nb, n
    logical, intent(in), optional :: periodic

    integer :: m

    !! Require at least 2 matrix block rows per image
    m = n; call co_min(m)
    if (m < 2) error stop 'co_btd_matrix%init: image matrix size is < 2'

    this%n = n
    this%nb = nb
    if (present(periodic)) this%periodic = periodic

    !! Total size of a periodic matrix must be at least 3
    m = n; call co_sum(m)
    if (this%periodic .and. m < 3) &
        error stop 'co_btd_matrix%init: periodic matrix size must be >= 3'

    allocate(this%l(nb,nb,n), this%d(nb,nb,n), this%u(nb,nb,n))

    m = num_images() - merge(0, 1, this%periodic)
    if (this%periodic .and. num_images() > 2) then
      call this%dhat%init(nb, m, periodic=.true.)
    else if (num_images() > 1) then
      call this%dhat%init(nb, m, periodic=.false.)
    end if

  end subroutine

  subroutine factor(this)
    class(co_btd_matrix), intent(inout) :: this
    if (this%periodic) then
      if (num_images() == 1) then
        call serial_factor_periodic(this)
      else
        call factor_periodic(this)
      end if
    else
      call factor_non_periodic(this)
    end if
  end subroutine

  subroutine factor_non_periodic(this)
    class(co_btd_matrix), intent(inout) :: this
    integer :: m
    real(r8), allocatable :: p1(:,:)[:], q1(:,:)[:]
#if defined(NAG_BUG20230603b) || defined(INTEL_BUG20230604)
    real(r8) :: tmp(this%nb,this%nb)
#endif
    m = merge(this%n-1, this%n, this_image() < num_images())
    associate (nb => this%nb, n => this%n)
      call serial_factor(this, 1, m)
      if (num_images() == 1) return
      allocate(p1(nb,nb)[*], q1(nb,nb)[*])
      if (this_image() < num_images()) then
        allocate(this%q(nb,nb,m))
        this%q = 0.0_r8
        this%q(:,:,m) = this%u(:,:,m)
        call serial_msolve(this, 1, m, this%q)  ! only backward substitution needed
        q1 = this%q(:,:,1)
      end if
      if (this_image() > 1) then
        allocate(this%p(nb,nb,m))
        this%p = 0.0_r8
        this%p(:,:,1) = this%l(:,:,1)
        call serial_msolve(this, 1, m, this%p)
        p1 = this%p(:,:,1)
      end if
      sync all
      if (this_image() < num_images()) then
        !this%dhat%d = this%d(n) - this%l(n)*this%q(m) - this%u(n)*p1[this_image()+1]
        this%dhat%d(:,:) = this%d(:,:,n)
        call cmab(this%dhat%d, this%l(:,:,n), this%q(:,:,m))
#ifdef NAG_BUG20230603b
        tmp = p1(:,:)[this_image()+1]
        call cmab(this%dhat%d, this%u(:,:,n), tmp)
#else
        call cmab(this%dhat%d, this%u(:,:,n), p1(:,:)[this_image()+1])
#endif
        !if (this_image() > 1) this%dhat%l = -this%l(n)*this%p(m)
        if (this_image() > 1) &
            this%dhat%l(:,:) = -matmul(this%l(:,:,n), this%p(:,:,m))
        !if (this_image() < num_images()-1) this%dhat%u = -this%u(n)*q1[this_image()+1]
#if defined(NAG_BUG20230603b) || defined(INTEL_BUG20230604)
        tmp = q1(:,:)[this_image()+1]
        if (this_image() < num_images()-1) this%dhat%u(:,:) = -matmul(this%u(:,:,n), tmp)
#else
        if (this_image() < num_images()-1) &
            this%dhat%u(:,:) = -matmul(this%u(:,:,n), q1(:,:)[this_image()+1])
#endif
      end if
      call this%dhat%factor
      sync all
    end associate
  end subroutine

  subroutine factor_periodic(this)
    class(co_btd_matrix), intent(inout) :: this
    integer :: m, next
    real(r8), allocatable :: p1(:,:)[:], q1(:,:)[:]
#if defined(NAG_BUG20230603b) || defined(INTEL_BUG20230604)
    real(r8) :: tmp(this%nb,this%nb)
#endif
    next = 1 + modulo(this_image(), num_images())
    m = this%n-1
    associate (nb => this%nb, n => this%n)
      call serial_factor(this, 1, m)
      allocate(p1(nb,nb)[*], q1(nb,nb)[*])
      allocate(this%q(nb,nb,m))
      this%q = 0.0_r8
      this%q(:,:,m) = this%u(:,:,m)
      call serial_msolve(this, 1, m, this%q)  ! only backward substitution needed
      q1 = this%q(:,:,1)
      allocate(this%p(nb,nb,m))
      this%p = 0.0_r8
      this%p(:,:,1) = this%l(:,:,1)
      call serial_msolve(this, 1, m, this%p)
      p1 = this%p(:,:,1)
      sync all
      if (num_images() > 2) then
        !this%dhat%l = -this%l(n)*this%p(m)
        this%dhat%l = - matmul(this%l(:,:,n), this%p(:,:,m))
        !this%dhat%d = this%d(n) - this%l(n)*this%q(m) - this%u(n)*p1[next]
        this%dhat%d = this%d(:,:,n)
        call cmab(this%dhat%d, this%l(:,:,n), this%q(:,:,m))
#ifdef NAG_BUG20230603b
        tmp = p1(:,:)[next]
        call cmab(this%dhat%d, this%u(:,:,n), tmp)
#else
        call cmab(this%dhat%d, this%u(:,:,n), p1(:,:)[next])
#endif
        !this%dhat%u = -this%u(n)*q1[next]
#if defined(NAG_BUG20230603b) || defined(INTEL_BUG20230604)
        tmp = q1(:,:)[next]
        this%dhat%u = - matmul(this%u(:,:,n), tmp)
#else
        this%dhat%u = - matmul(this%u(:,:,n), q1(:,:)[next])
#endif
      else
        select case (this_image())
        case (1)
          !this%dhat%d = this%d(n) - this%l(n)*this%q(m) - this%u(n)*p1[2]
          this%dhat%d = this%d(:,:,n)
          call cmab(this%dhat%d, this%l(:,:,n), this%q(:,:,m))
#ifdef NAG_BUG20230603b
          tmp = p1(:,:)[2]
          call cmab(this%dhat%d, this%u(:,:,n), tmp)
#else
          call cmab(this%dhat%d, this%u(:,:,n), p1(:,:)[2])
#endif
          !this%dhat%u =           - this%l(n)*this%p(m) - this%u(n)*q1[2]
          this%dhat%u = - matmul(this%l(:,:,n), this%p(:,:,m))
#ifdef NAG_BUG20230603b
          tmp = q1(:,:)[2]
          call cmab(this%dhat%u, this%u(:,:,n), tmp)
#else
          call cmab(this%dhat%u, this%u(:,:,n), q1(:,:)[2])
#endif
        case (2)
          !this%dhat%l =           - this%l(n)*this%p(m) - this%u(n)*q1[1]
          this%dhat%l = - matmul(this%l(:,:,n), this%p(:,:,m))
#ifdef NAG_BUG20230603b
          tmp = q1(:,:)[1]
          call cmab(this%dhat%l, this%u(:,:,n), tmp)
#else
          call cmab(this%dhat%l, this%u(:,:,n), q1(:,:)[1])
#endif
          !this%dhat%d = this%d(n) - this%l(n)*this%q(m) - this%u(n)*p1[1]
          this%dhat%d = this%d(:,:,n)
          call cmab(this%dhat%d, this%l(:,:,n), this%q(:,:,m))
#ifdef NAG_BUG20230603b
          tmp = p1(:,:)[1]
          call cmab(this%dhat%d, this%u(:,:,n), tmp)
#else
          call cmab(this%dhat%d, this%u(:,:,n), p1(:,:)[1])
#endif
        end select
      end if
      call this%dhat%factor
      sync all
    end associate
  end subroutine

  subroutine solve(this, b)
    class(co_btd_matrix), intent(inout) :: this
    real(r8), intent(inout) :: b(:,:)
    if (this%periodic) then
      if (num_images() == 1) then
        call serial_solve_periodic(this, b)
      else
        call solve_periodic(this, b)
      end if
    else
      call solve_non_periodic(this, b)
    end if
  end subroutine

  subroutine solve_non_periodic(this, b)
    class(co_btd_matrix), intent(inout) :: this
    real(r8), intent(inout) :: b(:,:)
    integer :: m, j
    real(r8), allocatable :: b1(:)[:], bn(:)[:]
    m = merge(this%n-1, this%n, this_image() < num_images())
    associate (nb => this%nb, n => this%n)
      call serial_solve(this, 1, m, b)
      if (num_images() == 1) return
      allocate(b1(nb)[*], bn(nb)[*])
      if (this_image() > 1) b1 = b(:,1)
      sync all
      if (this_image() < num_images()) then
        !b(n) = b(n) - this%l(n)*b(n-1) - this%u(n)*b1[this_image()+1]
        call ymax(b(:,n), this%l(:,:,n), b(:,n-1))
        call ymax(b(:,n), this%u(:,:,n), b1(:)[this_image()+1])
        bn = b(:,n)
      end if
      call this%dhat%solve(bn)
      if (this_image() < num_images()) b(:,n) = bn
      sync all
      if (this_image() > 1) then
        !b(1:m) = b(1:m) - this%p * bn[this_image()-1]
        do j = 1, m
          call ymax(b(:,j), this%p(:,:,j), bn(:)[this_image()-1])
        end do
      end if
      if (this_image() < num_images()) then
        !b(1:n-1) = b(1:n-1) - this%q*b(n)
        do j = 1, m
          call ymax(b(:,j), this%q(:,:,j), b(:,n))
        end do
      end if
      sync all
    end associate
  end subroutine

  subroutine solve_periodic(this, b)
    class(co_btd_matrix), intent(inout) :: this
    real(r8), intent(inout) :: b(:,:)
    integer :: m, next, prev, j
    real(r8), allocatable :: b1(:)[:], bn(:)[:]
    next = 1 + modulo(this_image(), num_images())
    prev = 1 + modulo(this_image()-2, num_images())
    m = this%n - 1
    associate (nb => this%nb, n => this%n)
      call serial_solve(this, 1, m, b)
      allocate(b1(nb)[*], bn(nb)[*])
      b1 = b(:,1)
      sync all
      !b(n) = b(n) - this%l(n)*b(m) - this%u(n)*b1[next]
      call ymax(b(:,n), this%l(:,:,n), b(:,m))
      call ymax(b(:,n), this%u(:,:,n), b1(:)[next])
      bn = b(:,n)
      call this%dhat%solve(bn)
      b(:,n) = bn
      sync all
      !b(1:m) = b(1:m) - this%p*bn[prev] - this%q*b(n)
      do j = 1, m
        call ymax(b(:,j), this%p(:,:,j), bn(:)[prev])
        call ymax(b(:,j), this%q(:,:,j), b(:,n))
      end do
    end associate
  end subroutine

  !! Returns the product of the matrix with the vector X in the vector Y.
  !! X and Y are distributed identically to the rows/columns of the matrix.
  !! This method cannot be used if the internal storage has been overwritten
  !! with the factorization of the matrix computed by FACTOR.

  subroutine matvec(this, x, y)
    class(co_btd_matrix), intent(inout) :: this  ! intent(in) for data
    real(r8), intent(in) :: x(:,:)
    real(r8), intent(out) :: y(:,:)
    integer :: j
    real(r8), allocatable :: xleft(:)[:], xright(:)[:]
    allocate(xleft(this%nb)[*], xright(this%nb)[*])
    xleft = x(:,1)
    xright = x(:,this%n)
    sync all
    associate (l => this%l, d => this%d, u => this%u)
      if (this_image() > 1) then
        !y(1) = this%l(1)*xright[this_image()-1] + this%d(1)*x(1) + this%u(1)*x(2)
        call matmul3(this%nb, l(:,:,1), xright(:)[this_image()-1], d(:,:,1), x(:,1), u(:,:,1), x(:,2), y(:,1))
      else if (this%periodic) then
        !y(1) = this%l(1)*xright[num_images()] + this%d(1)*x(1) + this%u(1)*x(2)
        call matmul3(this%nb, l(:,:,1), xright(:)[num_images()], d(:,:,1), x(:,1), u(:,:,1), x(:,2), y(:,1))
      else
        !y(1) = this%d(1)*x(1) + this%u(1)*x(2)
        call matmul2(this%nb, d(:,:,1), x(:,1), u(:,:,1), x(:,2), y(:,1))
      end if
      do j = 2, this%n-1
        !y(j) = this%l(j)*x(j-1) + this%d(j)*x(j) + this%u(j)*x(j+1)
        call matmul3(this%nb, l(:,:,j), x(:,j-1), d(:,:,j), x(:,j), u(:,:,j), x(:,j+1), y(:,j))
      end do
      if (this_image() < num_images()) then
        !y(j) = this%l(j)*x(j-1) + this%d(j)*x(j) + this%u(j)*xleft[this_image()+1]
        call matmul3(this%nb, l(:,:,j), x(:,j-1), d(:,:,j), x(:,j), u(:,:,j), xleft(:)[this_image()+1], y(:,j))
      else if (this%periodic) then
        !y(j) = this%l(j)*x(j-1) + this%d(j)*x(j) + this%u(j)*xleft[1]
        call matmul3(this%nb, l(:,:,j), x(:,j-1), d(:,:,j), x(:,j), u(:,:,j), xleft(:)[1], y(:,j))
      else
        !y(j) = this%l(j)*x(j-1) + this%d(j)*x(j)
        call matmul2(this%nb, l(:,:,j), x(:,j-1), d(:,:,j), x(:,j), y(:,j))
      end if
    end associate
    sync all

  contains

    subroutine matmul2(n, a1, x1, a2, x2, y)
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

    subroutine matmul3(n, a1, x1, a2, x2, a3, x3, y)
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

!    subroutine matmul2(n, a1, b1, a2, b2, c)
!      integer,  intent(in)  :: n
!      real(r8), intent(in)  :: a1(n,n), b1(n,n), a2(n,n), b2(n,n)
!      real(r8), intent(out) :: c(n,n)
!      integer :: i, j, k
!      real(r8) :: cij
!      do j = 1, n
!        do i = 1, n
!          cij = 0.0_r8
!            do k = 1, n
!              cij = cij + a1(i,k)*b1(k,j) + a2(i,k)*b2(k,j)
!            end do
!          c(i,j) = cij
!        end do
!      end do
!    end subroutine
!
!    subroutine matmul3(n, a1, b1, a2, b2, a3, b3, c)
!      integer,  intent(in)  :: n
!      real(r8), intent(in)  :: a1(n,n), b1(n,n), a2(n,n), b2(n,n), a3(n,n), b3(n,n)
!      real(r8), intent(out) :: c(n,n)
!      integer :: i, j, k
!      real(r8) :: cij
!      do j = 1, n
!        do i = 1, n
!          cij = 0.0_r8
!            do k = 1, n
!              cij = cij + a1(i,k)*b1(k,j) + a2(i,k)*b2(k,j) + a3(i,k)*b3(k,j)
!            end do
!          c(i,j) = cij
!        end do
!      end do
!    end subroutine

  end subroutine

  !! This auxiliary subroutine computes the usual LU factorization of the
  !! local submatrix composed of rows/columns j1 through j2. The elements
  !! of the local submatrix are overwritten with the elements of L and unit
  !! upper triangular U.

  subroutine serial_factor(this, j1, j2)
    class(co_btd_matrix), intent(inout) :: this
    integer, intent(in) :: j1, j2
    integer :: j
    call fct(this%d(:,:,j1))
    do j = j1+1, j2
      !this%u(j-1) = this%u(j-1)/this%d(j-1)
      call mslv(this%d(:,:,j-1), this%u(:,:,j-1))
      !this%d(j) = this%d(j) - this%l(j)*this%u(j-1)
      call cmab(this%d(:,:,j), this%l(:,:,j), this%u(:,:,j-1))
      call fct(this%d(:,:,j))
    end do
  end subroutine

  subroutine serial_factor_periodic(this)
    class(co_btd_matrix), intent(inout) :: this
    associate (nb => this%nb, n => this%n)
      call serial_factor(this, 1, n-1)
      allocate(this%q(nb,nb,n-1))
      this%q(:,:,1) = this%l(:,:,1)
      this%q(:,:,2:n-2) = 0.0_r8
      this%q(:,:,n-1) = this%u(:,:,n-1)
      call serial_msolve(this, 1, n-1, this%q)
      !this%d(n) = this%d(n) - this%u(n)*this%q(1) - this%l(n)*this%q(n-1)
      call cmab(this%d(:,:,n), this%u(:,:,n), this%q(:,:,1))
      call cmab(this%d(:,:,n), this%l(:,:,n), this%q(:,:,n-1))
      call fct(this%d(:,:,n))
    end associate
  end subroutine

  !! This auxiliary subroutine solves the linear system Ax = b where A is
  !! the submatrix composed of rows/columns j1 through j2. The submatrix
  !! must store the LU factorization computed by SERIAL_FACTOR. The RHS b
  !! is the subvector of the passed B composed of elements j1 through j2,
  !! and the computed solution overwrites those elements. Other elements
  !! of B are unmodified.

  subroutine serial_solve(this, j1, j2, b)
    class(co_btd_matrix), intent(in) :: this
    integer, intent(in) :: j1, j2
    real(r8), intent(inout) :: b(:,:)
    integer :: j
    !b(j1) = b(j1)/this%d(j1)
    call slv(this%d(:,:,j1), b(:,j1))
    do j = j1+1, j2
      !b(j) = (b(j) - this%l(j)*b(j-1))/this%d(j)
      call ymax(b(:,j), this%l(:,:,j), b(:,j-1))
      call slv(this%d(:,:,j), b(:,j))
    end do
    do j = j2-1, j1, -1
      !b(j) = b(j) - this%u(j)*b(j+1)
      call ymax(b(:,j), this%u(:,:,j), b(:,j+1))
    end do
  end subroutine

  subroutine serial_msolve(this, j1, j2, b)
    class(co_btd_matrix), intent(in) :: this
    integer, intent(in) :: j1, j2
    real(r8), intent(inout) :: b(:,:,:)
    integer :: j
    !b(j1) = b(j1)/this%d(j1)
    call mslv(this%d(:,:,j1), b(:,:,j1))
    do j = j1+1, j2
      !b(j) = (b(j) - this%l(j)*b(j-1))/this%d(j)
      call cmab(b(:,:,j), this%l(:,:,j), b(:,:,j-1))
      call mslv(this%d(:,:,j), b(:,:,j))
    end do
    do j = j2-1, j1, -1
      !b(j) = b(j) - this%u(j)*b(j+1)
      call cmab(b(:,:,j), this%u(:,:,j), b(:,:,j+1))
    end do
  end subroutine

  subroutine serial_solve_periodic(this, b)
    class(co_btd_matrix), intent(in) :: this
    real(r8), intent(inout) :: b(:,:)
    integer :: j
    associate (n => this%n)
      call serial_solve(this, 1, n-1, b)
      !b(n) = (b(n) - this%u(n)*b(1) - this%l(n)*b(n-1))/this%d(n)
      call ymax(b(:,n), this%u(:,:,n), b(:,1))
      call ymax(b(:,n), this%l(:,:,n), b(:,n-1))
      call slv(this%d(:,:,n), b(:,n))
      !b(1:n-1) = b(1:n-1) - b(n)*this%q
      do j = 1, n-1
        call ymax(b(:,j), this%q(:,:,j), b(:,n))
      end do
    end associate
  end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! This implementation of the distributed tridiagonal Schur complement system
  !! solver is the usual serial algorithm but applied to distributed data. Each
  !! image does the computation for its row, but waits until its neighbor image
  !! has computed the data it depends on.
  !!
  !! Another implementation, which may perform better, is to simply collate the
  !! matrix and RHS elements to a single image, do the computation there and
  !! then scatter the results.

  subroutine init_schur(this, nb, n, periodic)
    class(schur_matrix), intent(out) :: this
    integer, intent(in) :: nb, n ! n <= num_images()
    logical, intent(in), optional :: periodic
    this%n = n
    this%nb = nb
    if (present(periodic)) this%periodic = periodic
    allocate(this%l(nb,nb), this%d(nb,nb), this%u(nb,nb))
  end subroutine

  subroutine factor_schur(this)
    class(schur_matrix), intent(inout) :: this
    if (this%periodic) then
      call factor_schur_periodic(this)
    else
      call factor_schur_submatrix(this, this%n)
    end if
  end subroutine

  subroutine factor_schur_submatrix(this, n)
    class(schur_matrix), intent(inout) :: this
    integer, intent(in) :: n
    real(r8), allocatable :: u(:,:)[:]
#ifdef NAG_BUG20230603b
    real(r8) :: tmp(this%nb,this%nb)
#endif
    allocate(u(this%nb,this%nb)[*])
    if (this_image() > n) return ! this image is idle
    u = this%u
    if (this_image() > 1) then
      sync images (this_image()-1) ! hold until released
      !this%d = this%d - this%l*u[this_image()-1]
#ifdef NAG_BUG20230603b
      tmp = u(:,:)[this_image()-1]
      call cmab(this%d, this%l, tmp)
#else
      call cmab(this%d, this%l, u(:,:)[this_image()-1])
#endif
    end if
    call fct(this%d)
    if (this_image() < n) then
      !u = u/this%d
      call mslv(this%d, u)
      sync images (this_image()+1) ! release
    end if
    this%u = u
  end subroutine

  subroutine factor_schur_periodic(this)
    class(schur_matrix), intent(inout) :: this
    real(r8), allocatable :: w(:,:)[:]
#ifdef NAG_BUG20230603b
    real(r8) :: tmp(this%nb,this%nb)
#endif
    call factor_schur_submatrix(this, this%n-1)
    allocate(w(this%nb,this%nb)[*])
    if (this_image() == 1) then
      w = this%l
    else if (this_image() == this%n-1) then
      w = this%u
    else
      w = 0.0_r8
    end if
    sync all
    call msolve_schur_submatrix(this, this%n-1, w)
    sync all
    !if (this_image() == this%n) this%d = this%d - this%u*w[1] - this%l*w[this%n-1]
    if (this_image() == this%n) then
#ifdef NAG_BUG20230603b
      tmp = w(:,:)[1]
      call cmab(this%d, this%u, tmp)
      tmp = w(:,:)[this%n-1]
      call cmab(this%d, this%l, tmp)
#else
      call cmab(this%d, this%u, w(:,:)[1])
      call cmab(this%d, this%l, w(:,:)[this%n-1])
#endif
      call fct(this%d)
    end if
    this%w = w
  end subroutine

  subroutine solve_schur(this, b)
    class(schur_matrix), intent(in) :: this
    real(r8), intent(inout) :: b(:)[*]
    if (this_image() > this%n) return ! this image is idle
    if (this%periodic) then
      call solve_schur_periodic(this, b)
    else
      call solve_schur_submatrix(this, this%n, b)
    end if
  end subroutine

  subroutine solve_schur_submatrix(this, n, b)
    class(schur_matrix), intent(in) :: this
    integer, intent(in) :: n
    real(r8), intent(inout) :: b(:)[*]
    if (this_image() > n) return ! this image is idle
    if (this_image() > 1) then
      sync images (this_image()-1)  ! hold until released
      !b = (b - this%l*b[this_image()-1])/this%d
      call ymax(b, this%l, b(:)[this_image()-1])
      call slv(this%d, b)
    else
      !b = b/this%d
      call slv(this%d, b)
    end if
    if (this_image() < n) sync images (this_image()+1) ! release
    if (this_image() < n) then
      sync images (this_image()+1)  ! hold until released
      !b = b - this%u * b[this_image()+1]
      call ymax(b, this%u, b(:)[this_image()+1])
    end if
    if (this_image() > 1) sync images (this_image()-1)  ! release
  end subroutine

  subroutine msolve_schur_submatrix(this, n, b)
    class(schur_matrix), intent(in) :: this
    integer, intent(in) :: n
    real(r8), intent(inout) :: b(:,:)[*]
#ifdef NAG_BUG20230603b
    real(r8) :: tmp(this%nb,this%nb)
#endif
    if (this_image() > n) return ! this image is idle
    if (this_image() > 1) then
      sync images (this_image()-1)  ! hold until released
      !b = (b - this%l*b[this_image()-1])/this%d
#ifdef NAG_BUG20230603b
      tmp = b(:,:)[this_image()-1]
      call cmab(b, this%l, tmp)
#else
      call cmab(b, this%l, b(:,:)[this_image()-1])
#endif
      call mslv(this%d, b)
    else
      !b = b/this%d
      call mslv(this%d, b)
    end if
    if (this_image() < n) sync images (this_image()+1) ! release
    if (this_image() < n) then
      sync images (this_image()+1)  ! hold until released
      !b = b - this%u * b[this_image()+1]
#ifdef NAG_BUG20230603b
      tmp = b(:,:)[this_image()+1]
      call cmab(b, this%u, tmp)
#else
      call cmab(b, this%u, b(:,:)[this_image()+1])
#endif
    end if
    if (this_image() > 1) sync images (this_image()-1)  ! release
  end subroutine

  subroutine solve_schur_periodic(this, b)
    class(schur_matrix), intent(in) :: this
    real(r8), intent(inout) :: b(:)[*]
    call solve_schur_submatrix(this, this%n-1, b)
    sync all
    !if (this_image() == this%n) b = (b - this%u*b[1] - this%l*b[this%n-1])/this%d
    if (this_image() == this%n) then
      call ymax(b, this%u, b(:)[1])
      call ymax(b, this%l, b(:)[this%n-1])
      call slv(this%d, b)
    end if
    sync all
    !if (this_image() < this%n) b = b - this%w*b[this%n]
    if (this_image() < this%n) call ymax(b, this%w, b(:)[this%n])
  end subroutine

  !! LU factorization of a square matrix.  No pivoting (intentionally).
  !! Unit lower triangular factor; unit diagonal not stored.  Reciprocal
  !! of upper triangular diagonal stored.

  subroutine fct(a)

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

  subroutine slv(a, b)

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


  subroutine mslv(a, b)

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
  subroutine ymax(y, a, x)
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

  !! gemm(a, b, c, alpha=-1.0_r8, beta=1.0_r8)
  subroutine cmab(c, a, b)
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

end module co_btd_matrix_type
