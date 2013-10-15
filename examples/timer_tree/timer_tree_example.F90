!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2013  Neil N. Carlson
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

program timer_tree_example

  use timer_tree_type
  use,intrinsic :: iso_fortran_env, only: output_unit

  real :: tspin, x
  integer :: nspin

  call calibrate_spin ! configures spin to do a fixed amount of work

  call start_timer ("A")
    call start_timer ("B")
      call spin ! do some work
    call stop_timer  ("B")
    call spin
    call start_timer ("C")
      call spin
      call start_timer ("B")
        call spin
      call stop_timer  ("B")
    call stop_timer ("C")
  call stop_timer  ("A")
  call start_timer ("B")
    call start_timer ("X")
      call spin
    call stop_timer  ("X")
    call start_timer ("Y")
      call spin
    call stop_timer  ("Y")
    call start_timer ("Z")
      call spin
    call stop_timer  ("Z")
  call stop_timer  ("B")
  call start_timer ("A")
      call spin
  call stop_timer  ("A")

  call write_timer_tree (output_unit, indent=2)

contains

  subroutine spin()
    integer :: j
    x = 0.0
    do j = 1, nspin
      x = (x**2 - 1.0)**2
    end do
  end subroutine spin

  subroutine calibrate_spin ()

    real,    parameter :: TARGET_TIME = 0.01
    integer, parameter :: SAMPLE_SIZE = 1
    integer :: n0, n1
    real :: t0, t1

    nspin = 8192
    tspin = 0.0  !average_spin_time()
    do while (tspin < TARGET_TIME)
      n0 = nspin
      t0 = tspin
      nspin = 4*nspin
      tspin = average_spin_time(SAMPLE_SIZE)
    end do

    n1 = nspin
    t1 = tspin

    nspin = n0 + (n1-n0)*((TARGET_TIME-t0)/(t1-t0))
    tspin = average_spin_time(SAMPLE_SIZE)

  end subroutine calibrate_spin

  real function average_spin_time (n)
    integer, intent(in) :: n
    integer :: j
    real :: t0, t1
    average_spin_time = 0.0
    do j = 1, n
      call cpu_time (t0)
      call spin
      call cpu_time (t1)
      average_spin_time = average_spin_time + (t1 - t0)
    end do
    average_spin_time = average_spin_time / n
  end function average_spin_time

end program
