!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2008, 2013  Neil N. Carlson
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

program test_timer_tree

  use timer_tree_type
  implicit none

  integer :: status = 0
  integer, allocatable :: tree(:)
  character(:), allocatable :: key(:)
  character(15) :: prog = 'TEST_TIMER_TREE'
  real, allocatable :: cpu(:)
  real :: tspin, x = 0.0
  integer :: nspin

  call calibrate_spin ()

  call test_tree_1 ()
  call test_tree_2 ()
  call test_tree_3 ()
  call test_cpu_1 ()
  call test_cpu_2 ()
  call test_handle ()

  if (status /= 0) stop 1

contains

  !!
  !! The main intent here is to verify that the sibling_with_key procedure can
  !! correctly create new nodes if necessary, or find an existing one in
  !! various circumstances.  It suffices to create a 1-deep tree, where we
  !! revisit the children in various orders.  In the process we are testing
  !! the stop_timer routine for proper tree traversal, the initialization
  !! of the root node, and serialize_timer_tree for proper tree traversal.
  !!

  subroutine test_tree_1 ()

    logical :: pass

    call start_timer ('A')
    call stop_timer  ('A')
    call start_timer ('A')
    call stop_timer  ('A')
    call start_timer ('B')
    call stop_timer  ('B')
    call start_timer ('C')
    call stop_timer  ('C')
    call start_timer ('B')
    call stop_timer  ('B')
    call start_timer ('C')
    call stop_timer  ('C')
    call serialize_timer_tree (tree, key, cpu)

    pass = all(key == ['A','B','C']) .and. all(tree==[1,1,2,2,3,3])

    if (.not.pass) then
      status = 1
      write(0,*) prog // ': tree_1 test failed'
    end if

    deallocate(tree, key, cpu)
    call reset_timer_tree ()

  end subroutine test_tree_1

  !!
  !! Here we are testing the creation of a more deeply nested tree with
  !! re-used keys.  Also tests that reset_timer_tree from the previous
  !! test brought us back to a null root (but did it really deallocate
  !! the storage?)
  !!

  subroutine test_tree_2 ()

    logical :: pass

    call start_timer ('A')
    call stop_timer  ('A')
    call start_timer ('B')
    call start_timer ('A')
    call stop_timer  ('A')
    call start_timer ('B')
    call start_timer ('A')
    call stop_timer  ('A')
    call stop_timer  ('B')
    call stop_timer  ('B')
    call start_timer ('C')
    call serialize_timer_tree (tree, key, cpu)

    pass = all(key == ['A','B','A','B','A','C']) .and. all(tree==[1,1,2,3,3,4,5,5,4,2,6,6])

    if (.not.pass) then
      status = 1
      write(0,*) prog // ': tree_2 test failed'
    end if

    deallocate(tree, key, cpu)
    call reset_timer_tree ()

  end subroutine test_tree_2

  !!
  !! Here we verify serialize_timer_tree by creating a tree, flattening it,
  !! using the resulting data to recreate the tree, flatten it again and
  !! compare the flattened data from the two trees which should be identical.
  !!

  subroutine test_tree_3

    logical :: pass
    integer, allocatable :: tree1(:)
    character(:), allocatable :: key1(:)
    real, allocatable :: cpu1(:)

    call start_timer ('A')
      call start_timer ('B')
        call spin
      call stop_timer  ('B')
      call start_timer ('C')
        call start_timer ('D')
          call spin
        call stop_timer  ('D')
        call start_timer ('E')
          call spin
        call stop_timer  ('E')
      call stop_timer  ('C')
      call start_timer ('F')
        call spin
      call stop_timer  ('F')
    call stop_timer  ('A')
    call start_timer ('C')
      call start_timer ('D')
        call spin
      call stop_timer  ('D')
      call start_timer ('E')
        call spin
      call stop_timer  ('E')
    call stop_timer  ('C')

    call serialize_timer_tree (tree, key, cpu)
    call reset_timer_tree ()

    call deserialize_timer_tree (tree, key, cpu)
    call serialize_timer_tree (tree1, key1, cpu1)

    pass = all(tree==tree1) .and. all(key==key1)

    if (.not.pass) then
      status = 1
      write(0,*) prog // ': tree_3 test failed (tree structure)'
    else
      pass = all(cpu == cpu1)
      if (.not.pass) then
        status = 1
        write(0,*) prog // ': tree_3 test failed (cpu values)'
      end if
    end if

    deallocate(tree, key, cpu, tree1, key1, cpu1)
    call reset_timer_tree ()

  end subroutine test_tree_3

  !!
  !! Here we check whether the accumulation of time for a timer is being done
  !! correctly.  We start a timer, spin, stop it, restart it, spin some more,
  !! stop it and check the elapsed time for correctness.
  !!

  subroutine test_cpu_1 ()

    logical :: pass
    real :: error

    call start_timer ('A')
    call spin ()
    call stop_timer ('A')
    call serialize_timer_tree (tree, key, cpu)

    call start_timer ('A')
    call spin ()
    call stop_timer ('A')
    call serialize_timer_tree (tree, key, cpu)

    !! Relative difference from expected
    error = abs(cpu(1)-2*tspin)/cpu(1)
    pass = (error < 0.2)

    if (.not.pass) then
      status = 1
      write(0,*) prog // ': cpu_1 test failed; error =', error
    end if

    deallocate(tree, key, cpu)
    call reset_timer_tree ()

  end subroutine test_cpu_1

  !!
  !! Here we check whether the accumulate_elapsed_time subroutine is doing the
  !! right thing.  We set up a tree with some running timers, spin, and then
  !! stop one, check the reported time is correct, spin some more and check the
  !! reported timers again for correctness.
  !!

  subroutine test_cpu_2 ()

    logical :: pass
    real :: error

    call start_timer ('A')
    call start_timer ('A')
    call start_timer ('A')
    call spin
    call stop_timer  ('A')
    call serialize_timer_tree (tree, key, cpu)

    !! Relative error from expected
    error = maxval(abs(cpu-tspin)/cpu)
    pass = (error < 0.2)

    if (.not.pass) then
      status = 1
      write(0,*) prog // ': cpu_2 test 1 failed; error =', error
    end if

    call spin
    deallocate(tree, key, cpu)
    call serialize_timer_tree (tree, key, cpu)

    !! Relative error from expected
    error = maxval(abs(cpu-[2*tspin,2*tspin,tspin])/cpu)
    pass = (error < 0.2)

    if (.not.pass) then
      status = 1
      write(0,*) prog // ': cpu_2 test 2 failed; error =', error
    end if

    deallocate(tree, key, cpu)
    call reset_timer_tree ()

  end subroutine test_cpu_2

  !!
  !! Here we test the timer handles and read_timer.  Create a small tree
  !! where the timers have different times and verify that we get the
  !! correct times from a stopped timer and from running timers at
  !! different positions in the hierarchy of running timers.
  !!

  subroutine test_handle ()

    logical :: pass
    real :: error, cpu
    integer :: ha, hb, hc

    call start_timer ('A',ha)
    call start_timer ('B',hb)
    call spin
    call spin
    call stop_timer  ('B')
    call start_timer ('C',hc)
    call spin

    call read_timer (hb, cpu)

    !! Relative error from expected
    error = abs(cpu-2*tspin)/cpu
    pass = (error < 0.2)

    if (.not.pass) then
      status = 1
      write(0,*) prog // ': handle test 1 failed; error =', error
    end if

    call read_timer (ha, cpu)

    !! Relative error from expected
    error = abs(cpu-3*tspin)/cpu
    pass = (error < 0.2)

    if (.not.pass) then
      status = 1
      write(0,*) prog // ': handle test 2 failed; error =', error
    end if

    call read_timer (hc, cpu)

    !! Relative error from expected
    error = abs(cpu-tspin)/cpu
    pass = (error < 0.2)

    if (.not.pass) then
      status = 1
      write(0,*) prog // ': handle test 3 failed; error =', error
    end if

    call stop_timer  ('C')
    call stop_timer  ('A')

    call reset_timer_tree ()

  end subroutine test_handle

  subroutine spin()
    integer :: j
    !x = 0.0
    do j = 1, nspin
      x = (x**2 - 1.0)**2
    end do
  end subroutine spin

  subroutine calibrate_spin ()

    real,    parameter :: TARGET_TIME = 0.1
    integer, parameter :: SAMPLE_SIZE = 3
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

end program test_timer_tree
