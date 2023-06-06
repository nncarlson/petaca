!!
!! CO_TD_MATRIX_TYPE
!!
!! A parallel data structure for tridiagonal matrices, with methods for linear
!! equations and matrix-vector products, that is implemented using coarrays.
!! Supports periodic tridiagonal matrices as well. Linear equation solution
!! uses direct LU factorization without pivoting and is thus only suitable for
!! classes of matrices not requiring pivoting, such as diagonally-dominant
!! matrices.
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

module co_td_matrix_type

  use,intrinsic :: iso_fortran_env, only: r8 => real64
  use td_matrix_type
  implicit none
  private

  !! This auxiliary type stores the tridiagonal Schur complement matrix
  !! that results from the local elimination of the interior equations on
  !! each image. Each row of the matrix is stored by a different image.

  type, private :: schur_matrix
    integer :: n
    logical :: periodic = .false.
    real(r8) :: l, d, u, w
  contains
    procedure :: init => init_schur
    procedure :: factor => factor_schur
    procedure :: solve => solve_schur
  end type

  type, extends(td_matrix), public :: co_td_matrix
    ! Additional LU factorization fill-in and Schur complement
    real(r8), allocatable :: p(:)
    type(schur_matrix) :: dhat
  contains
    procedure :: init
    procedure :: factor
    procedure :: solve
    procedure :: matvec
  end type

contains

  subroutine init(this, n, periodic)

    class(co_td_matrix), intent(out) :: this
    integer, intent(in) :: n
    logical, intent(in), optional :: periodic

    integer :: m

    m = n; call co_min(m) ! require at least 2 rows per image
    if (m < 2) error stop 'co_td_matrix%init: image matrix size is < 2'

    this%n = n
    if (present(periodic)) this%periodic = periodic

    m = n; call co_sum(m) ! global matrix size
    if (this%periodic .and. m < 3) error stop 'co_td_matrix%init: periodic matrix size < 3'

    allocate(this%l(n), this%d(n), this%u(n))

    m = num_images() - merge(0, 1, this%periodic)
    if (this%periodic .and. num_images() > 2) then
      call this%dhat%init(m, periodic=.true.)
    else if (num_images() > 1) then
      call this%dhat%init(m, periodic=.false.)
    end if

  end subroutine

  subroutine factor(this)
    class(co_td_matrix), intent(inout) :: this
    if (num_images() == 1) then
      call this%td_matrix%factor ! serial method
    else if (this%periodic) then
      call factor_periodic(this)
    else
      call factor_non_periodic(this)
    end if
  end subroutine

  subroutine factor_non_periodic(this)
    class(co_td_matrix), intent(inout) :: this
    integer :: m
    real(r8), allocatable :: p1[:], q1[:]
    m = merge(this%n-1, this%n, this_image() < num_images())
    associate (n => this%n)
      call this%factor_submatrix(1, m) ! inherited serial method
      if (num_images() == 1) return
      allocate(p1[*], q1[*])
      if (this_image() < num_images()) then
        allocate(this%q(m))
        this%q = 0.0_r8
        this%q(m) = this%u(m)
        call this%solve_submatrix(1, m, this%q)  ! only backward substitution needed
        q1 = this%q(1)
      end if
      if (this_image() > 1) then
        allocate(this%p(m))
        this%p = 0.0_r8
        this%p(1) = this%l(1)
        call this%solve_submatrix(1, m, this%p)
        p1 = this%p(1)
      end if
      sync all
      if (this_image() < num_images()) then
        this%dhat%d = this%d(n) - this%l(n)*this%q(m) - this%u(n)*p1[this_image()+1]
        if (this_image() > 1) this%dhat%l = -this%l(n)*this%p(m)
        if (this_image() < num_images()-1) this%dhat%u = -this%u(n)*q1[this_image()+1]
      end if
      call this%dhat%factor
      sync all
    end associate
  end subroutine

  subroutine factor_periodic(this)
    class(co_td_matrix), intent(inout) :: this
    integer :: m, next
    real(r8), allocatable :: p1[:], q1[:]
    next = 1 + modulo(this_image(), num_images())
    m = this%n-1
    associate (n => this%n)
      call this%factor_submatrix(1, m) ! inherited serial method
      allocate(p1[*], q1[*])
      allocate(this%q(m))
      this%q = 0.0_r8
      this%q(m) = this%u(m)
      call this%solve_submatrix(1, m, this%q)  ! only backward substitution needed
      q1 = this%q(1)
      allocate(this%p(m))
      this%p = 0.0_r8
      this%p(1) = this%l(1)
      call this%solve_submatrix(1, m, this%p)
      p1 = this%p(1)
      sync all
      if (num_images() > 2) then
        this%dhat%l = -this%l(n)*this%p(m)
        this%dhat%d = this%d(n) - this%l(n)*this%q(m) - this%u(n)*p1[next]
        this%dhat%u = -this%u(n)*q1[next]
      else
        select case (this_image())
        case (1)
          this%dhat%d = this%d(n) - this%l(n)*this%q(m) - this%u(n)*p1[2]
          this%dhat%u =           - this%l(n)*this%p(m) - this%u(n)*q1[2]
        case (2)
          this%dhat%l =           - this%l(n)*this%p(m) - this%u(n)*q1[1]
          this%dhat%d = this%d(n) - this%l(n)*this%q(m) - this%u(n)*p1[1]
        end select
      end if
      call this%dhat%factor
      sync all
    end associate
  end subroutine

  subroutine solve(this, b)
    class(co_td_matrix), intent(in) :: this
    real(r8), intent(inout) :: b(:)
    if (num_images() == 1) then
      call this%td_matrix%solve(b)
    else if (this%periodic) then
      call solve_periodic(this, b)
    else
      call solve_non_periodic(this, b)
    end if
  end subroutine

  subroutine solve_non_periodic(this, b)
    class(co_td_matrix), intent(in) :: this
    real(r8), intent(inout) :: b(:)
    integer :: m
    real(r8), allocatable :: b1[:], bn[:]
    m = merge(this%n-1, this%n, this_image() < num_images())
    associate (n => this%n)
      call this%solve_submatrix(1, m, b)
      if (num_images() == 1) return
      allocate(b1[*], bn[*])
      if (this_image() > 1) b1 = b(1)
      sync all
      if (this_image() < num_images()) then
        b(n) = b(n) - this%l(n)*b(n-1) - this%u(n)*b1[this_image()+1]
        bn = b(n)
      end if
      call this%dhat%solve(bn)
      if (this_image() < num_images()) b(n) = bn
      sync all
      if (this_image() > 1) then
        b(1:m) = b(1:m) - this%p * bn[this_image()-1]
      end if
      if (this_image() < num_images()) then
        b(1:n-1) = b(1:n-1) - this%q*b(n)
      end if
      sync all
    end associate
  end subroutine

  subroutine solve_periodic(this, b)
    class(co_td_matrix), intent(in) :: this
    real(r8), intent(inout) :: b(:)
    integer :: m, next, prev
    real(r8), allocatable :: b1[:], bn[:]
    next = 1 + modulo(this_image(), num_images())
    prev = 1 + modulo(this_image()-2, num_images())
    m = this%n - 1
    associate (n => this%n)
      call this%solve_submatrix(1, m, b)
      allocate(b1[*], bn[*])
      b1 = b(1)
      sync all
      b(n) = b(n) - this%l(n)*b(m) - this%u(n)*b1[next]
      bn = b(n)
      call this%dhat%solve(bn)
      b(n) = bn
      sync all
      b(1:m) = b(1:m) - this%p*bn[prev] - this%q*b(n)
    end associate
  end subroutine

  !! Returns the product of the matrix with the vector X in the vector Y.
  !! X and Y are distributed identically to the rows/columns of the matrix.
  !! This method cannot be used if the internal storage has been overwritten
  !! with the factorization of the matrix computed by FACTOR.

  subroutine matvec(this, x, y)
    class(co_td_matrix), intent(in) :: this  ! intent(in) for data
    real(r8), intent(in) :: x(:)
    real(r8), intent(out) :: y(:)
    integer :: j
    real(r8), allocatable :: xleft[:], xright[:]
    allocate(xleft[*], xright[*])
    xleft = x(1)
    xright = x(this%n)
    sync all
    y(1) = this%d(1)*x(1) + this%u(1)*x(2)
    if (this_image() > 1) then
      y(1) = y(1) + this%l(1)*xright[this_image()-1]
    else if (this%periodic) then
      y(1) = y(1) + this%l(1)*xright[num_images()]
    end if
    do j = 2, this%n-1
      y(j) = this%l(j)*x(j-1) + this%d(j)*x(j) + this%u(j)*x(j+1)
    end do
    y(j) = this%l(j)*x(j-1) + this%d(j)*x(j)
    if (this_image() < num_images()) then
      y(j) = y(j) + this%u(j)*xleft[this_image()+1]
    else if (this%periodic) then
      y(j) = y(j) + this%u(j)*xleft[1]
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

  subroutine init_schur(this, n, periodic)
    class(schur_matrix), intent(out) :: this
    integer, intent(in) :: n ! matrix order, <= num_images()
    logical, intent(in), optional :: periodic
    this%n = n
    if (present(periodic)) this%periodic = periodic
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
    real(r8), allocatable :: u[:]
    allocate(u[*])
    if (this_image() > n) return ! this image is idle
    u = this%u
    if (this_image() > 1) then
      sync images (this_image()-1) ! hold until released
      this%d = this%d - this%l*u[this_image()-1]
    end if
    if (this_image() < n) then
      u = u/this%d
      sync images (this_image()+1) ! release
    end if
    this%u = u
  end subroutine

  subroutine factor_schur_periodic(this)
    class(schur_matrix), intent(inout) :: this
    real(r8), allocatable :: w[:]
    call factor_schur_submatrix(this, this%n-1)
    allocate(w[*])
    if (this_image() == 1) then
      w = this%l
    else if (this_image() == this%n-1) then
      w = this%u
    else
      w = 0.0_r8
    end if
    sync all
    call solve_schur_submatrix(this, this%n-1, w)
    sync all
    if (this_image() == this%n) this%d = this%d - this%u*w[1] - this%l*w[this%n-1]
    this%w = w
  end subroutine

  subroutine solve_schur(this, b)
    class(schur_matrix), intent(in) :: this
    real(r8), intent(inout) :: b[*]
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
    real(r8), intent(inout) :: b[*]
    if (this_image() > n) return ! this image is idle
    if (this_image() > 1) then
      sync images (this_image()-1)  ! hold until released
      b = (b - this%l*b[this_image()-1])/this%d
    else
      b = b/this%d
    end if
    if (this_image() < n) sync images (this_image()+1) ! release
    if (this_image() < n) then
      sync images (this_image()+1)  ! hold until released
      b = b - this%u * b[this_image()+1]
    end if
    if (this_image() > 1) sync images (this_image()-1)  ! release
  end subroutine

  subroutine solve_schur_periodic(this, b)
    class(schur_matrix), intent(in) :: this
    real(r8), intent(inout) :: b[*]
    call solve_schur_submatrix(this, this%n-1, b)
    sync all
    if (this_image() == this%n) b = (b - this%u*b[1] - this%l*b[this%n-1])/this%d
    sync all
    if (this_image() < this%n) b = b - this%w*b[this%n]
  end subroutine

end module
