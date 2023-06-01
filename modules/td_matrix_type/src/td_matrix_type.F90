module td_matrix_type

  use,intrinsic :: iso_fortran_env, only: r8 => real64
  implicit none
  private

  type, public :: td_matrix
    integer :: n
    logical :: periodic = .false.
    real(r8), allocatable :: l(:), d(:), u(:)
    real(r8), allocatable, private :: w(:)
  contains
    procedure :: init
    procedure :: factor
    procedure :: solve
    procedure :: matvec
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
    allocate(this%l(n), this%d(n), this%u(n))
  end subroutine

  pure subroutine factor(this)
    class(td_matrix), intent(inout) :: this
    if (this%periodic) then
      call factor_periodic(this)
    else
      call factor_submatrix(this, 1, this%n)
    end if
  end subroutine

  pure subroutine factor_submatrix(this, j1, j2)
    class(td_matrix), intent(inout) :: this
    integer, intent(in) :: j1, j2
    integer :: j
    do j = j1+1, j2
      this%u(j-1) = this%u(j-1)/this%d(j-1)
      this%d(j) = this%d(j) - this%l(j)*this%u(j-1)
    end do
  end subroutine

  pure subroutine factor_periodic(this)
    class(td_matrix), intent(inout) :: this
    associate (n => this%n)
      call this%factor_submatrix(1, n-1)
      allocate(this%w(n-1))
      this%w(1) = this%l(1)
      this%w(2:n-2) = 0.0_r8
      this%w(n-1) = this%u(n-1)
      call this%solve_submatrix(1, n-1, this%w)
      this%d(n) = this%d(n) - this%u(n)*this%w(1) - this%l(n)*this%w(n-1)
    end associate
  end subroutine

  pure subroutine solve(this, b)
    class(td_matrix), intent(in) :: this
    real(r8), intent(inout) :: b(:)
    if (this%periodic) then
      call solve_periodic(this, b)
    else
      call solve_submatrix(this, 1, this%n, b)
    end if
  end subroutine

  pure subroutine solve_submatrix(this, j1, j2, b)
    class(td_matrix), intent(in) :: this
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

  pure subroutine solve_periodic(this, b)
    class(td_matrix), intent(in) :: this
    real(r8), intent(inout) :: b(:)
    associate (n => this%n)
      call this%solve_submatrix(1, n-1, b)
      b(n) = (b(n) - this%u(n)*b(1) - this%l(n)*b(n-1))/this%d(n)
      b(1:n-1) = b(1:n-1) - b(n)*this%w
    end associate
  end subroutine

  pure subroutine matvec(this, x, y)
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
