!!
!! CO_BTD_MATRIX_TYPE
!!
!! A parallel data structure for block tridiagonal matrices, with methods for
!! linear equations and matrix-vector products, that is implemented using
!! coarrays. Supports periodic block tridiagonal matrices as well. Linear
!! equation solution uses direct LU factorization without pivoting and is
!! thus only suitable for classes of matrices not requiring pivoting, such
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

module co_btd_matrix_type

  use,intrinsic :: iso_fortran_env, only: r8 => real64
  use btd_matrix_type
  use block_solver_procs, only: fct, slv, mslv, ymax, cmab
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

  type, extends(btd_matrix), public :: co_btd_matrix
    ! Additional LU decomposition fill-in and Schur complement
    real(r8), allocatable :: p(:,:,:)
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

    if (nb < 2) error stop ' co_btd_matrix%init: block size < 2'

    m = n; call co_min(m) ! require at least 2 block rows per image
    if (m < 2) error stop 'co_btd_matrix%init: image matrix size is < 2'

    this%n = n
    this%nb = nb
    if (present(periodic)) this%periodic = periodic

    m = n; call co_sum(m) ! global matrix size
    if (this%periodic .and. m < 3) error stop 'co_btd_matrix%init: periodic matrix size < 3'

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
    if (num_images() == 1) then
      call this%btd_matrix%factor ! inherited serial method
    else if (this%periodic) then
      call factor_periodic(this)
    else
      call factor_non_periodic(this)
    end if
  end subroutine

  subroutine factor_non_periodic(this)
    use block_solver_procs, only: cmab
    class(co_btd_matrix), intent(inout) :: this
    integer :: m
    real(r8), allocatable :: p1(:,:)[:], q1(:,:)[:]
#if defined(NAG_BUG20230603b) || defined(INTEL_BUG20230604)
    real(r8) :: tmp(this%nb,this%nb)
#endif
    m = merge(this%n-1, this%n, this_image() < num_images())
    associate (nb => this%nb, n => this%n)
      call this%factor_submatrix(1, m)  ! inherited serial method
      if (num_images() == 1) return
      allocate(p1(nb,nb)[*], q1(nb,nb)[*])
      if (this_image() < num_images()) then
        allocate(this%q(nb,nb,m))
        this%q = 0.0_r8
        this%q(:,:,m) = this%u(:,:,m)
        call this%msolve_submatrix(1, m, this%q) ! inherited serial method
        q1 = this%q(:,:,1)
      end if
      if (this_image() > 1) then
        allocate(this%p(nb,nb,m))
        this%p = 0.0_r8
        this%p(:,:,1) = this%l(:,:,1)
        call this%msolve_submatrix(1, m, this%p) ! inherited serial method
        p1 = this%p(:,:,1)
      end if
      sync all
      if (this_image() < num_images()) then
        this%dhat%d(:,:) = this%d(:,:,n)
        call cmab(this%dhat%d, this%l(:,:,n), this%q(:,:,m))
#ifdef NAG_BUG20230603b
        tmp = p1(:,:)[this_image()+1]
        call cmab(this%dhat%d, this%u(:,:,n), tmp)
#else
        call cmab(this%dhat%d, this%u(:,:,n), p1(:,:)[this_image()+1])
#endif
        if (this_image() > 1) &
            this%dhat%l(:,:) = -matmul(this%l(:,:,n), this%p(:,:,m))
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
    use block_solver_procs, only: cmab
    class(co_btd_matrix), intent(inout) :: this
    integer :: m, next
    real(r8), allocatable :: p1(:,:)[:], q1(:,:)[:]
#if defined(NAG_BUG20230603b) || defined(INTEL_BUG20230604)
    real(r8) :: tmp(this%nb,this%nb)
#endif
    next = 1 + modulo(this_image(), num_images())
    m = this%n-1
    associate (nb => this%nb, n => this%n)
      call this%factor_submatrix(1, m) ! inherited serial method
      allocate(p1(nb,nb)[*], q1(nb,nb)[*])
      allocate(this%q(nb,nb,m))
      this%q = 0.0_r8
      this%q(:,:,m) = this%u(:,:,m)
      call this%msolve_submatrix(1, m, this%q) ! inherited serial method
      q1 = this%q(:,:,1)
      allocate(this%p(nb,nb,m))
      this%p = 0.0_r8
      this%p(:,:,1) = this%l(:,:,1)
      call this%msolve_submatrix(1, m, this%p) ! inherited serial method
      p1 = this%p(:,:,1)
      sync all
      if (num_images() > 2) then
        this%dhat%l = - matmul(this%l(:,:,n), this%p(:,:,m))
        this%dhat%d = this%d(:,:,n)
        call cmab(this%dhat%d, this%l(:,:,n), this%q(:,:,m))
#ifdef NAG_BUG20230603b
        tmp = p1(:,:)[next]
        call cmab(this%dhat%d, this%u(:,:,n), tmp)
#else
        call cmab(this%dhat%d, this%u(:,:,n), p1(:,:)[next])
#endif
#if defined(NAG_BUG20230603b) || defined(INTEL_BUG20230604)
        tmp = q1(:,:)[next]
        this%dhat%u = - matmul(this%u(:,:,n), tmp)
#else
        this%dhat%u = - matmul(this%u(:,:,n), q1(:,:)[next])
#endif
      else
        select case (this_image())
        case (1)
          this%dhat%d = this%d(:,:,n)
          call cmab(this%dhat%d, this%l(:,:,n), this%q(:,:,m))
#ifdef NAG_BUG20230603b
          tmp = p1(:,:)[2]
          call cmab(this%dhat%d, this%u(:,:,n), tmp)
#else
          call cmab(this%dhat%d, this%u(:,:,n), p1(:,:)[2])
#endif
          this%dhat%u = - matmul(this%l(:,:,n), this%p(:,:,m))
#ifdef NAG_BUG20230603b
          tmp = q1(:,:)[2]
          call cmab(this%dhat%u, this%u(:,:,n), tmp)
#else
          call cmab(this%dhat%u, this%u(:,:,n), q1(:,:)[2])
#endif
        case (2)
          this%dhat%l = - matmul(this%l(:,:,n), this%p(:,:,m))
#ifdef NAG_BUG20230603b
          tmp = q1(:,:)[1]
          call cmab(this%dhat%l, this%u(:,:,n), tmp)
#else
          call cmab(this%dhat%l, this%u(:,:,n), q1(:,:)[1])
#endif
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
    class(co_btd_matrix), intent(in) :: this
    real(r8), intent(inout) :: b(:,:)
    if (num_images() == 1) then
      call this%btd_matrix%solve(b) ! inherited serial method
    else if (this%periodic) then
      call solve_periodic(this, b)
    else
      call solve_non_periodic(this, b)
    end if
  end subroutine

  subroutine solve_non_periodic(this, b)
    use block_solver_procs, only: ymax
    class(co_btd_matrix), intent(in) :: this
    real(r8), intent(inout) :: b(:,:)
    integer :: m, j
    real(r8), allocatable :: b1(:)[:], bn(:)[:]
    m = merge(this%n-1, this%n, this_image() < num_images())
    associate (nb => this%nb, n => this%n)
      call this%solve_submatrix(1, m, b) ! inherited serial method
      if (num_images() == 1) return
      allocate(b1(nb)[*], bn(nb)[*])
      if (this_image() > 1) b1 = b(:,1)
      sync all
      if (this_image() < num_images()) then
        call ymax(b(:,n), this%l(:,:,n), b(:,n-1))
        call ymax(b(:,n), this%u(:,:,n), b1(:)[this_image()+1])
        bn = b(:,n)
      end if
      call this%dhat%solve(bn)
      if (this_image() < num_images()) b(:,n) = bn
      sync all
      if (this_image() > 1) then
        do j = 1, m
          call ymax(b(:,j), this%p(:,:,j), bn(:)[this_image()-1])
        end do
      end if
      if (this_image() < num_images()) then
        do j = 1, m
          call ymax(b(:,j), this%q(:,:,j), b(:,n))
        end do
      end if
      sync all
    end associate
  end subroutine

  subroutine solve_periodic(this, b)
    use block_solver_procs, only: ymax
    class(co_btd_matrix), intent(in) :: this
    real(r8), intent(inout) :: b(:,:)
    integer :: m, next, prev, j
    real(r8), allocatable :: b1(:)[:], bn(:)[:]
    next = 1 + modulo(this_image(), num_images())
    prev = 1 + modulo(this_image()-2, num_images())
    m = this%n - 1
    associate (nb => this%nb, n => this%n)
      call this%solve_submatrix(1, m, b) ! inherited serial method
      allocate(b1(nb)[*], bn(nb)[*])
      b1 = b(:,1)
      sync all
      call ymax(b(:,n), this%l(:,:,n), b(:,m))
      call ymax(b(:,n), this%u(:,:,n), b1(:)[next])
      bn = b(:,n)
      call this%dhat%solve(bn)
      b(:,n) = bn
      sync all
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

    use block_solver_procs, only: ypax

    class(co_btd_matrix), intent(in) :: this  ! intent(in) for data
    real(r8), intent(in) :: x(:,:)
    real(r8), intent(out) :: y(:,:)

    integer :: j
    real(r8), allocatable :: xleft(:)[:], xright(:)[:]

    allocate(xleft(this%nb)[*], xright(this%nb)[*])
    xleft = x(:,1)
    xright = x(:,this%n)
    sync all

    y(:,1) = 0.0_r8
    call ypax(y(:,1), this%d(:,:,1), x(:,1))
    call ypax(y(:,1), this%u(:,:,1), x(:,2))
    if (this_image() > 1) then
      call ypax(y(:,1), this%l(:,:,1), xright(:)[this_image()-1])
    else if (this%periodic) then
      call ypax(y(:,1), this%l(:,:,1), xright(:)[num_images()])
    end if
    do j = 2, this%n-1
      y(:,j) = 0.0_r8
      call ypax(y(:,j), this%l(:,:,j), x(:,j-1))
      call ypax(y(:,j), this%d(:,:,j), x(:,j))
      call ypax(y(:,j), this%u(:,:,j), x(:,j+1))
    end do
    y(:,j) = 0.0_r8
    call ypax(y(:,j), this%l(:,:,j), x(:,j-1))
    call ypax(y(:,j), this%d(:,:,j), x(:,j))
    call ypax(y(:,j), this%u(:,:,j), x(:,j+1))
    if (this_image() < num_images()) then
      call ypax(y(:,j), this%u(:,:,j), xleft(:)[this_image()+1])
    else if (this%periodic) then
      call ypax(y(:,j), this%u(:,:,j), xleft(:)[1])
    end if
    sync all

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
      call ymax(b, this%l, b(:)[this_image()-1])
      call slv(this%d, b)
    else
      call slv(this%d, b)
    end if
    if (this_image() < n) sync images (this_image()+1) ! release
    if (this_image() < n) then
      sync images (this_image()+1)  ! hold until released
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
#ifdef NAG_BUG20230603b
      tmp = b(:,:)[this_image()-1]
      call cmab(b, this%l, tmp)
#else
      call cmab(b, this%l, b(:,:)[this_image()-1])
#endif
      call mslv(this%d, b)
    else
      call mslv(this%d, b)
    end if
    if (this_image() < n) sync images (this_image()+1) ! release
    if (this_image() < n) then
      sync images (this_image()+1)  ! hold until released
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
    if (this_image() == this%n) then
      call ymax(b, this%u, b(:)[1])
      call ymax(b, this%l, b(:)[this%n-1])
      call slv(this%d, b)
    end if
    sync all
    if (this_image() < this%n) call ymax(b, this%w, b(:)[this%n])
  end subroutine

end module co_btd_matrix_type
