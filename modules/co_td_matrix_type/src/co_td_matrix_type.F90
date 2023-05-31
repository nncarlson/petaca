!!
!! CO_TD_MATRIX_TYPE
!!
!! A parallel data structure for tridiagonal matrices that is implemented
!! using coarrays. Includes linear solver and matrix-vector product methods.
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
  implicit none
  private

  !! This auxiliary type stores the tridiagonal Schur complement matrix
  !! that results from the local elimination of the interior equations on
  !! each image. Each row of the matrix is stored by a different image.

  type, private :: schur_matrix
    real(r8) :: l, d
    real(r8), allocatable :: u[:]
    integer :: n
  contains
    procedure :: init => init_schur_matrix
    procedure :: factor => factor_schur_matrix
    procedure :: solve => solve_schur_matrix
  end type

  type, public :: co_td_matrix
    private
    integer, public :: n
    real(r8), allocatable, public :: l(:), d(:), u(:)
    ! LU decomposition fill-in and Schur complement
    real(r8), allocatable :: p(:), q(:)
    type(schur_matrix) :: dhat
    ! Persistent communication buffers
    real(r8), allocatable :: co_tmp1[:], co_tmp2[:]
  contains
    procedure :: init
    procedure :: factor
    procedure :: solve
    procedure :: matvec
  end type

contains

  subroutine init(this, n, stat, errmsg)
    class(co_td_matrix), intent(inout) :: this
    integer, intent(in) :: n
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    integer :: nmin
    nmin = n
    call co_min(nmin)
    if (nmin < 2) then
      if (present(stat)) then
        stat = -1
        if (present(errmsg)) errmsg = 'image matrix size is < 2'
        return
      else
        error stop 'co_td_matrix%init: image matrix size is < 2'
      end if
    end if
    if (present(stat)) stat = 0
    this%n = n
    if (allocated(this%l)) deallocate(this%l)
    if (allocated(this%d)) deallocate(this%d)
    if (allocated(this%u)) deallocate(this%u)
    allocate(this%l(n), this%d(n), this%u(n))
    if (.not.allocated(this%co_tmp1)) allocate(this%co_tmp1[*])
    if (.not.allocated(this%co_tmp2)) allocate(this%co_tmp2[*])
    if (num_images() > 1) call this%dhat%init(num_images()-1)
  end subroutine

  subroutine factor(this)
    class(co_td_matrix), intent(inout) :: this
    integer :: m
    m = merge(this%n-1, this%n, this_image() < num_images())
    associate (n => this%n) !, p1 => this%co_tmp1, q1 => this%co_tmp2)
      call serial_factor(this, 1, m)
      if (num_images() == 1) return
      if (this_image() < num_images()) then
        allocate(this%q(m))
        this%q = 0.0_r8
        this%q(m) = this%u(m)
        call serial_solve(this, 1, m, this%q)  ! only backward substitution needed
        this%co_tmp2 = this%q(1) ! q1 = this%q(1)
      end if
      if (this_image() > 1) then
        allocate(this%p(m))
        this%p = 0.0_r8
        this%p(1) = this%l(1)
        call serial_solve(this, 1, m, this%p)
        this%co_tmp1 = this%p(1) ! p1 = this%p(1)
        sync images (this_image()-1)
      end if
      if (this_image() < num_images()) then
        sync images (this_image()+1)
        this%dhat%d = this%d(n) - this%l(n)*this%q(m) - this%u(n)*this%co_tmp1[this_image()+1]
        if (this_image() > 1) this%dhat%l = -this%l(n)*this%p(m)
        if (this_image() < num_images()-1) this%dhat%u = -this%u(n)*this%co_tmp2[this_image()+1]
      end if
      call this%dhat%factor
      sync all
    end associate
  end subroutine

  subroutine solve(this, x)
    class(co_td_matrix), intent(inout) :: this
    real(r8), intent(inout) :: x(:)
    integer :: m
    m = merge(this%n-1, this%n, this_image() < num_images())
    associate (n => this%n) !, x1 => this%co_tmp1, xn => this%co_tmp2)
      call serial_solve(this, 1, m, x)
      if (num_images() == 1) return
      if (this_image() > 1) then
        this%co_tmp1 = x(1) ! x1 = x(1)
        sync images (this_image()-1)
      end if
      if (this_image() < num_images()) then
        sync images (this_image()+1)
        x(n) = x(n) - this%l(n)*x(n-1) - this%u(n)*this%co_tmp1[this_image()+1]
        this%co_tmp2 = x(n) ! xn = x(n)
      end if
      if (num_images() > 1) call this%dhat%solve(this%co_tmp2)
      if (this_image() < num_images()) then
        x(n) = this%co_tmp2 ! x(n) = xn
        sync images (this_image()+1)
      end if
      if (this_image() > 1) then
        sync images (this_image()-1)
#ifdef NAGFOR
        block
          integer :: j
          do j = 1, m
            x(j) = x(j) - this%p(j) * this%co_tmp2[this_image()-1]
          end do
        end block
#else
        x(1:m) = x(1:m) - this%p * this%co_tmp2[this_image()-1]
#endif
      end if
      if (this_image() < num_images()) then
        x(1:n-1) = x(1:n-1) - this%q*x(n)
      end if
    end associate
  end subroutine

  !! Returns the product of the matrix with the vector X in the vector Y.
  !! X and Y are distributed identically to the rows/columns of the matrix.
  !! This method cannot be used if the internal storage has been overwritten
  !! with the factorization of the matrix computed by FACTOR.

  subroutine matvec(this, x, y)
    class(co_td_matrix), intent(inout) :: this  ! intent(in) for data
    real(r8), intent(in) :: x(:)
    real(r8), intent(out) :: y(:)
    integer :: j
    !associate (xleft => this%co_tmp1, xright => this%co_tmp2)
      this%co_tmp1 = x(1) !xleft = x(1)
      this%co_tmp2 = x(this%n) !xright = x(this%n)
      sync all
      if (this_image() > 1) then
        y(1) = this%l(1)*this%co_tmp2[this_image()-1] + this%d(1)*x(1) + this%u(1)*x(2)
      else
        y(1) = this%d(1)*x(1) + this%u(1)*x(2)
      end if
      do j = 2, this%n-1
        y(j) = this%l(j)*x(j-1) + this%d(j)*x(j) + this%u(j)*x(j+1)
      end do
      if (this_image() < num_images()) then
        y(j) = this%l(j)*x(j-1) + this%d(j)*x(j) + this%u(j)*this%co_tmp1[this_image()+1]
      else
        y(j) = this%l(j)*x(j-1) + this%d(j)*x(j)
      end if
    !end associate
  end subroutine

  !! This auxiliary subroutine computes the usual LU factorization of the
  !! local submatrix composed of rows/columns j1 through j2. The elements
  !! of the local submatrix are overwritten with the elements of L and unit
  !! upper triangular U.

  pure subroutine serial_factor(this, j1, j2)
    class(co_td_matrix), intent(inout) :: this
    integer, intent(in) :: j1, j2
    integer :: j
    do j = j1+1, j2
      this%u(j-1) = this%u(j-1)/this%d(j-1)
      this%d(j) = this%d(j) - this%l(j)*this%u(j-1)
    end do
  end subroutine

  !! This auxiliary subroutine solves the linear system Ax = b where A is
  !! the submatrix composed of rows/columns j1 through j2. The submatrix
  !! must store the LU factorization computed by SERIAL_FACTOR. The RHS b
  !! is the subvector of the passed B composed of elements j1 through j2,
  !! and the computed solution overwrites those elements. Other elements
  !! of B are unmodified.

  pure subroutine serial_solve(this, j1, j2, b)
    class(co_td_matrix), intent(in) :: this
    integer, intent(in) :: j1, j2
    real(r8), intent(inout) :: b(:)
    integer :: j
    b(j1) = b(j1)/this%d(j1)
    do j = j1+1, j2
      b(j) = (b(j) - this%l(j)*b(j-1))/this%d(j)
    end do
    do j = j2-1, j1, -1
      b(j) = b(j) - this%u(j)*b(j+1)
    end do
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

  subroutine init_schur_matrix(this, n)
    class(schur_matrix), intent(inout) :: this
    integer, intent(in) :: n ! matrix order, <= num_images()
    this%n = n
    if (.not.allocated(this%u)) allocate(this%u[*])
  end subroutine

  subroutine factor_schur_matrix(this)
    class(schur_matrix), intent(inout) :: this
    if (this_image() > this%n) return ! this image is idle
    if (this_image() > 1) then
      sync images (this_image()-1) ! hold until released
      this%d = this%d - this%l*this%u[this_image()-1]
    end if
    if (this_image() < this%n) then
      this%u = this%u/this%d
      sync images (this_image()+1) ! release
    end if
  end subroutine

  subroutine solve_schur_matrix(this, x)
    class(schur_matrix), intent(in) :: this
    real(r8), intent(inout) :: x[*]
    if (this_image() > this%n) return ! this image is idle
    if (this_image() > 1) then
      sync images (this_image()-1)  ! hold until released
      x = (x - this%l*x[this_image()-1])/this%d
    else
      x = x/this%d
    end if
    if (this_image() < this%n) sync images (this_image()+1) ! release
    if (this_image() < this%n) then
      sync images (this_image()+1)  ! hold until released
      x = x - this%u * x[this_image()+1]
    end if
    if (this_image() > 1) sync images (this_image()-1)  ! release
  end subroutine

end module
