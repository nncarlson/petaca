!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2011, 2013  Neil N. Carlson
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

program test_state_history_type

  use,intrinsic :: iso_fortran_env, only: r8 => real64
  use state_history_type
  implicit none

  integer :: status = 0

  call test_finalization
  call test_counts
  call test_record_interp
  call test_record_xdot
  call test_revise
  call test_recent
  call test_deltas
  call test_flush
  call test_defined

  if (status /= 0) stop 1

contains

  subroutine test_finalization
    type(state_history) :: h1
    type(state_history), pointer :: h2(:)
    allocate(h2(2))
    call h2(1)%init(1,1)
    deallocate(h2)
  end subroutine test_finalization

  subroutine test_counts

    logical :: pass
    type(state_history) :: h

    call h%init (3, 1)
    pass = (h%max_depth() == 3) .and. (h%state_size() == 1)
    pass = pass .and. (h%depth() == 0)

    call h%record_state (0.0_r8, [0.0_r8])
    pass = pass .and. (h%depth() == 1)

    call h%record_state (1.0_r8, [0.0_r8], [1.0_r8])
    pass = pass .and. (h%depth() == 3)

    call h%record_state (0.0_r8, [0.0_r8])
    pass = pass .and. (h%depth() == 3)

    call h%flush (0.0_r8, [0.0_r8])
    pass = pass .and. (h%depth() == 1)

    call h%init (1, 2)
    pass = pass .and. (h%max_depth() == 1) .and. (h%state_size() == 2)
    pass = pass .and. (h%depth() == 0)

    call h%record_state (0.0_r8, [0.0_r8, 0.0_r8])
    pass = pass .and. (h%depth() == 1)

    call h%record_state (0.0_r8, [0.0_r8, 0.0_r8])
    pass = pass .and. (h%depth() == 1)

    if (.not.pass) then
      status = 1
      write(0,*) 'test_counts failed'
    end if

  end subroutine test_counts

  subroutine test_record_interp

    integer :: n
    logical :: pass
    real(r8) :: t, dt, x(5), xref(5)
    type(state_history) :: h
    real(r8), parameter :: TREF = 2.0_r8
    integer, parameter :: MVEC = 5

    xref = poly(TREF)
    call h%init(MVEC, size(xref))

    t = -0.625_r8
    dt = 0.375_r8
    do n = 1, MVEC
      call h%record_state(t, poly(t))
      call h%interp_state(TREF, x)
      x = x - xref
      pass = all(x(:n) == 0.0_r8) .and. all(x(n+1:) /= 0.0_r8)
      if (.not.pass) exit
      t = t + dt
    end do

    call h%interp_state (t=1.0_r8, x=x(:3))
    call h%interp_state (t=1.0_r8, x=x(4:5), first=4)
    xref = poly(1.0_r8)
    pass = pass .and. all(xref == x)

    if (.not.pass) then
      status = 1
      write(0,*) 'test_record_interp failed'
    end if

  end subroutine test_record_interp

  subroutine test_record_xdot ()

    logical :: pass
    real(r8) :: x(1)
    type(state_history) :: h

    call h%init (mvec=4, t=0.0_r8, x=[p(0.0_r8)], xdot=[pp(0.0_r8)])
    call h%record_state (t=1.0_r8, x=[p(1.0_r8)], xdot=[pp(1.0_r8)])

    call h%interp_state (t=0.5_r8, x=x)
    pass = (x(1) == p(0.5_r8))

    if (.not.pass) then
      status = 1
      write(0,*) 'test_record_xdot failed'
    end if

  end subroutine test_record_xdot

  real(r8) function p (t)
    real(r8), intent(in) :: t
    p = 2 + t*(t*(t-1)-1)
  end function p

  real(r8) function pp (t)
    real(r8), intent(in) :: t
    pp = (3*t - 2)*t - 1
  end function pp

  subroutine test_revise

    integer :: n
    logical :: pass
    real(r8) :: t, dt, x(5)
    type(state_history) :: h
    integer, parameter :: MVEC = 3

    call h%init(MVEC, size(x))

    t = -0.625_r8
    dt = 0.375_r8
    do n = 1, MVEC
      call h%record_state(t, poly(t))
      t = t + dt
    end do

    call h%revise(2, x=10.0_r8)
    call h%revise(3, x=10.0_r8, xdot=1.0_r8)
    call h%interp_state (t, x(2:3), first=2)

    pass = (x(2) == 10) .and. (x(3) == 10 + dt)

    if (.not.pass) then
      status = 1
      write(0,*) 'test_revise failed'
    end if

  end subroutine test_revise

  subroutine test_recent ()

    logical :: pass
    integer :: n
    real(r8) :: t, x(1)
    type(state_history) :: h
    real(r8), pointer :: view(:)

    call h%init (3, 1)

    pass = .true.
    t = 1.0_r8
    x = 2.0_r8
    do n = 1, 4
      call h%record_state (t, x)
      call h%get_last_state_view (view)
      pass = pass .and. (h%last_time() == t) .and. all(view == x)
      t = 1.25_r8 * t
      x = 1.25_r8 * x
    end do

    call h%record_state (t, x, 2*x)
    call h%get_last_state_view (view)
    pass = pass .and. (h%last_time() == t) .and. all(view == x)

    if (.not.pass) then
      status = 1
      write(0,*) 'test_recent failed'
    end if

  end subroutine test_recent

  subroutine test_deltas ()

    integer :: n
    logical :: pass
    real(r8) :: t(0:4) = [1.0, 1.5, 1.75, 2.125, 3.125]
    type(state_history) :: h

    pass = .true.
    call h%init (4, t(0), [1.0_r8])
    do n = 1, 3
      call h%record_state (t(n), [1.0_r8])
      pass = pass .and. all(t(n)-t(n-1:0:-1) == h%time_deltas())
    end do
    call h%record_state (t(4), [1.0_r8])
    pass = pass .and. all(t(4)-t(3:1:-1) == h%time_deltas())

    if (.not.pass) then
      status = 1
      write(0,*) 'test_deltas failed'
    end if

  end subroutine test_deltas

  subroutine test_flush ()

    logical :: pass
    real(r8) :: x(1), xref(1)
    type(state_history) :: h
    real(r8), pointer :: view(:)

    call h%init (4, 0.0_r8, [0.0_r8], [1.0_r8])

    xref = [-1.0_r8]
    call h%flush (1.0_r8, xref)
    call h%interp_state (2.0_r8, x)
    call h%get_last_state_view (view)

    pass = (h%last_time() == 1.0_r8) .and. &
           all(view == xref) .and. &
           all(x == xref)

    if (.not.pass) then
      status = 1
      write(0,*) 'test_flush failed'
    end if

  end subroutine test_flush

  subroutine test_defined

    logical :: pass
    type(state_history) :: h

    pass = .not. h%defined()

    call h%init (2, 1)
    pass = pass .and. h%defined()

    call h%flush (0.0_r8, [1.0_r8])
    pass = pass .and. h%defined()

    if (.not.pass) then
      status = 1
      write(0,*) 'test_flush failed'
    end if

  end subroutine test_defined

  function poly (t) result (p)
    real(r8), intent(in) :: t
    real(r8) :: p(5)
    p(1) = 1
    p(2) = 1 - t
    p(3) = 1 + t*(1 + t)
    p(4) = 2 - t*(1 - t*t)
    p(5) = 1 + t*t*(1 - t*t)
  end function poly

  function poly_deriv (t) result (pp)
    real(r8), intent(in) :: t
    real(r8) :: pp(5)
    pp(1) = 0
    pp(2) = -1
    pp(3) = 1 + 2*t
    pp(4) = -1 + 3*t*t
    pp(5) = t*(2 - 4*t*t)
  end function poly_deriv

end program test_state_history_type
