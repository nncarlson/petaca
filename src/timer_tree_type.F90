!!
!! TIMER_TREE_TYPE
!!
!! This module implements a lightweight method for creating and maintaining
!! a nested set, or tree, of timers.  The tree is automatically generated via
!! the nested starting and stopping of named timers.  Because timers are
!! distinguished by name and position in the tree, a start/stop pair will give
!! rise to different timers when the code containing it is executed within
!! different timer nestings.  As a result, the timing of a portion of shared
!! code can easily, and automatically, be partitioned according to its use.
!!
!! Neil Carlson <neil.n.carlson@gmail.com>
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2007, 2013  Neil N. Carlson
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
!!
!! PROGRAMMING INTERFACE
!!
!! This module defines the derived data type TIMER_TREE and a collection of
!! procedures that operate on a private module variable of that type, the
!! so-called global timer tree.  In most cases, use of the global timer tree
!! via these procedures will provide adequate functionality, but the derived
!! type is also provided should application code want to manage its own timer
!! tree objects.
!!
!! The derived type has the following type bound procedures.
!!
!!  START(NAME [,HANDLE]) starts the timer with the specified character string
!!    name that is a child of the current timer.  If no such child exists, one
!!    is created with this name.  This child timer then becomes the current
!!    timer.  If the optional integer argument handle is specified, it returns
!!    a handle to the timer which can be used as an argument to the WRITE or
!!    READ type bound procedures.
!!
!!  STOP(NAME [,STAT [,ERRMSG]]) stops the current timer.  The current timer's
!!    parent becomes the new current timer.  It is an error if the current
!!    timer does not have the specified name.  If the optional integer argument
!!    STAT is present, it is assigned the value 0 if no error was encountered;
!!    otherwise it is assigned a non-zero value.  In the latter case, the
!!    allocatable deferred-length character string ERRMSG, if present, is
!!    assigned an explanatory message.  If STAT is not present and an error
!!    occurs, the error message is written to the preconnected error unit and
!!    the program is stopped.
!!
!!  WRITE(UNIT, INDENT [,HANDLE]) writes the accumulated time for each timer
!!    to the specified logical unit, using indentation to express the nested
!!    structure of the timer tree.  The incremental number of spaces to
!!    indent for successive tree levels is given by the integer INDENT.  If
!!    an optional integer HANDLE returned by START is specified, only the
!!    accumulated times for that timer and its decendents are written.
!!
!!  READ(HANDLE, CPU) returns, in the default real argument CPU, the elapsed
!!    time for the timer associated with the HANDLE returned by START.  The
!!    timer may be running or stopped.
!!
!!  SERIALIZE(TREE, NAME, CPU) returns the current state of the timer tree
!!    in flat arrays.  Timers may be running or stopped and their state is
!!    unaltered.  The allocatable, deferred-length character array NAME and
!!    allocatable default real array CPU return the timer names and elapsed
!!    cpu times indexed by tree node number.  The allocatable default integer
!!    array TREE returns the structure of the tree as a sequence of node
!!    numbers: node numbers appear in matching pairs, like opening and closing
!!    parentheses, with the intervening sequence describing the trees of its
!!    children, recursively.  The nodes are numbered so that the initial node
!!    of the pairs appear in sequential order.  This enables a simple
!!    reconstruction of the tree; see USING THE OUTPUT OF SERIALIZE below.
!!
!!  DESERIALIZE(TREE, NAME, CPU) defines the state of the timer tree using
!!    the TREE, NAME, and CPU arrays as returned by SERIALIZE.  This can be
!!    used to initialize a timer tree with results from a previous simulation,
!!    for example.  Note that timer handles are not preserved.
!!
!! Objects of timer_tree type should not be used in assignment statements;
!! only the default intrinsic assignment is available and its semantics are
!! almost certainly not what would be desired.  Objects are properly finalized
!! when deallocated or otherwise cease to exist.
!!
!! The following subroutines are provided which operate on the "global timer
!! tree" which is a private timer_tree type module variable.  The subroutines
!! have the same interface and effect as the corresponding type bound
!! procedures
!!
!!  CALL START_TIMER (NAME [,HANDLE])
!!  CALL STOP_TIMER (NAME [,STAT [,ERRMSG]])
!!  CALL WRITE_TIMER_TREE (UNIT, INDENT [,HANDLE])
!!  CALL READ_TIMER (HANDLE, CPU)
!!  CALL SERIALIZE_TIMER_TREE (TREE, NAME, CPU)
!!  CALL DESERIALIZE_TIMER_TREE (TREE, NAME, CPU)
!!
!! In addition there is the subroutine
!!
!!  CALL RESET_TIMER_TREE () which resets the global timer tree to its
!!    initial empty state.
!!
!! Using the timers requires no more that calling START_TIMER and STOP_TIMER
!! in matching pairs with an arbitrary name string.  The only restriction is
!! that the timers be nested; starting "A", then "B", and then stopping "A"
!! is not allowed, for example.  Any incorrect nesting of the timers will be
!! detected by STOP_TIMER.  The same name may be used for multiple start/stop
!! pairs, and stop/start pairs with the same name and nesting are treated as
!! a single timer and their elapsed time accumulated accordingly.  Otherwise
!! they are treated as separate timers.  To obtain the total time associated
!! with a particular key, wherever it occurs in the tree, would be a simple
!! post-processing step.
!!
!! For portability the Fortran intrinsic subroutine CPU_TIME is used to
!! acquire the processor time, and thus the resolution of the timers is
!! limited by the resolution of this subroutine, which varies from one
!! system to another but is typically not very fine.  As a result, these
!! timers are not well suited to timing computationally short bits of code.
!!

#include "f90_assert.fpp"

module timer_tree_type

  implicit none
  private

  type, public :: timer_tree
    private
    type(node), pointer :: root => null()
    type(node), pointer :: current => null()
    integer :: max_name_len = 0
  contains
    procedure :: start => timer_tree_start
    procedure :: stop  => timer_tree_stop
    procedure :: write => timer_tree_write
    procedure :: serialize => timer_tree_serialize
    procedure :: deserialize => timer_tree_deserialize
    procedure :: read => timer_tree_read
    final  :: timer_tree_delete
  end type

  type :: node
    integer :: handle = 0
    character(:), allocatable :: name
    real :: cpu_elapsed  = 0.0
    real :: cpu_start = 0.0
    type(node), pointer :: parent  => null()
    type(node), pointer :: sibling => null()
    type(node), pointer :: child   => null()
  end type node

  !! Single-instance global timer and associated procedures.
  type(timer_tree), save :: global
  public :: start_timer, stop_timer, write_timer_tree
  public :: reset_timer_tree, read_timer
  public :: serialize_timer_tree, deserialize_timer_tree

contains

  subroutine timer_tree_start (this, name, handle)
    class(timer_tree), intent(inout) :: this
    character(*), intent(in) :: name
    integer, intent(out), optional :: handle
    !! Initialization: empty root node.
    if (.not.associated(this%root)) then
      allocate(this%root)
      this%current => this%root
    end if
    ASSERT(associated(this%current))
    this%current => sibling_with_name(this%current%child)
    call cpu_time (this%current%cpu_start)
    if (present(handle)) handle = this%current%handle
    this%max_name_len = max(len(name), this%max_name_len)
  contains
    recursive function sibling_with_name (child) result (timer)
      type(node), pointer :: child, timer
      if (.not.associated(child)) then
        allocate(child)
        child%cpu_elapsed=0.0
        child%name = name
        child%parent => this%current
        this%root%handle = this%root%handle + 1 ! update the number of nodes
        child%handle = this%root%handle
        timer => child
      else if (child%name == name) then
        timer => child
      else
        timer => sibling_with_name (child%sibling)
      end if
    end function sibling_with_name
  end subroutine timer_tree_start

  subroutine timer_tree_stop (this, name, stat, errmsg)
    class(timer_tree), intent(inout) :: this
    character(*), intent(in) :: name
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    character(:), allocatable :: msg
    real :: cpu_stop
    INSIST(associated(this%current))
    if (name /= this%current%name) then
      msg = 'TIMER_TREE%STOP: current timer is ' // this%current%name // '; cannot stop ' // name
      call error_handler (msg, stat, errmsg)
      return
    end if
    if (present(stat)) stat = 0
    call cpu_time (cpu_stop)
    this%current%cpu_elapsed = this%current%cpu_elapsed + (cpu_stop - this%current%cpu_start)
    this%current => this%current%parent
  end subroutine timer_tree_stop

  subroutine timer_tree_delete (this)
    type(timer_tree), intent(inout) :: this
    call node_delete (this%root)
    this%current => null()
  contains
    recursive subroutine node_delete (timer)
      type(node), pointer :: timer
      if (.not.associated(timer)) return
      call node_delete (timer%child)
      call node_delete (timer%sibling)
      deallocate(timer)
    end subroutine node_delete
  end subroutine timer_tree_delete

  subroutine timer_tree_write (this, unit, indent, handle)
    class(timer_tree), intent(in) :: this
    integer, intent(in) :: unit
    integer, intent(in) :: indent
    integer, intent(in), optional :: handle
    type(node), pointer :: child
    ASSERT(associated(this%root))
    ASSERT(indent >= 0)
    call accumulate_elapsed_time (this) ! for any running timers
    if (present(handle)) then
      !! Write the tree rooted at the timer with the given handle.
      ASSERT(handle > 0 .and. handle <= this%root%handle)
      child => timer_with_handle(this, handle)
      ASSERT(associated(child))
      call write_tree (child, tab=0)
    else
      !! Write the trees rooted at each of the root children; the root node
      !! isn't a real timer so we can't just call WRITE_TREE on the root node.
      child => this%root%child
      do while (associated(child))
        call write_tree (child, tab=0)
        child => child%sibling
      end do
    end if
  contains
    recursive subroutine write_tree (timer, tab)
      type(node), intent(in) :: timer
      integer, intent(in) :: tab
      type(node), pointer :: child
      write(unit,fmt='(3a,es12.5)') repeat(' ',tab), timer%name, ':', timer%cpu_elapsed
      child => timer%child
      do while (associated(child))
        call write_tree (child, tab+indent)
        child => child%sibling
      end do
    end subroutine write_tree
  end subroutine timer_tree_write

  subroutine timer_tree_serialize (this, tree, name, cpu)
    class(timer_tree), intent(in) :: this
    integer, allocatable, intent(out) :: tree(:)
    character(:), allocatable, intent(out) :: name(:)
    real, allocatable, intent(out) :: cpu(:)
    type(node), pointer :: child
    integer :: n, m
    ASSERT(associated(this%root))
    n = timer_tree_size(this)
    allocate(tree(2*n), cpu(n))
    allocate(character(this%max_name_len)::name(n))
    call accumulate_elapsed_time (this) ! for any running timers
    !! Global counters referenced by the recursive calls to SERIALIZE.
    n = 0 ! last defined location in name and CPU; also the last assigned node number.
    m = 0 ! last defined location in TREE.
    !! Serialize the trees rooted at each of the children; the root node
    !! isn't a real timer so we can't just call SERIALIZE on the root node.
    child => this%root%child
    do while (associated(child))
      call serialize (child)
      child => child%sibling
    end do
  contains
    recursive subroutine serialize (timer)
      type(node), intent(in) :: timer
      type(node), pointer :: child
      integer :: this_n
      !! Next node; store its data.
      n = n + 1
      ASSERT(n <= size(name))
      name(n) = timer%name
      cpu(n) = timer%cpu_elapsed
      !! Start of nesting for this node.
      this_n = n ! local to this recursive invocation; n is global.
      m = m + 1
      ASSERT(m <= size(tree))
      tree(m) = this_n
      !! Serialize the trees rooted at each of the children.
      child => timer%child
      do while (associated(child))
        call serialize (child)
        child => child%sibling
      end do
      !! End of nesting for this node.
      m = m + 1
      ASSERT(m <= size(tree))
      tree(m) = this_n
    end subroutine serialize
  end subroutine timer_tree_serialize

  subroutine timer_tree_deserialize (this, tree, name, cpu)
    class(timer_tree), intent(out) :: this
    integer, intent(in) :: tree(:)
    character(*), intent(in) :: name(:)
    real, intent(in) :: cpu(:)
    integer :: n, m, j
    type(node), pointer :: timer
    ASSERT(size(name) == size(cpu))
    ASSERT(size(tree) == 2*size(name))
    INSIST(minval(tree) >= 1 .and. maxval(tree) <= size(name))
    INSIST(valid_tree())
    m = 0
    do j = 1, size(tree)
      n = tree(j)
      if (n > m) then
        call this%start (trim(name(n)))
        m = n
      else
        timer => this%current ! save pointer to timer before next stop changes it
        call this%stop (trim(name(n)))
        timer%cpu_elapsed = cpu(n)  ! overwrite with the correct data
      end if
    end do
  contains
    logical function valid_tree ()
      integer, allocatable :: tag(:)
      logical, allocatable :: init(:)
      integer :: j
      !! Each node must appear exactly twice (one pair).
      allocate(tag(size(name)))
      tag = 0
      do j = 1, size(tree)
        tag(tree(j)) = tag(tree(j)) + 1
      end do
      valid_tree = all(tag == 2)
      if (.not.valid_tree) return
      !! The initial nodes of the pairs must be in sequential order.
      allocate(init(size(tree)))
      tag = 0
      init = .false.
      do j = 1, size(tree)
        if (tag(tree(j)) /= 0) cycle
        tag(tree(j)) = 1
        init(j) = .true.
      end do
      valid_tree = all(pack(tree,mask=init) == [(j,j=1,size(name))])
    end function valid_tree
  end subroutine timer_tree_deserialize

  subroutine timer_tree_read (this, handle, cpu)
    class(timer_tree), intent(in) :: this
    integer, intent(in) :: handle
    real, intent(out) :: cpu
    type(node), pointer :: timer
    real :: cpu_now
    ASSERT(associated(this%root))
    ASSERT(handle > 0 .and. handle <= this%root%handle)
    timer => timer_with_handle(this, handle)
    ASSERT(associated(timer))
    cpu = timer%cpu_elapsed
    if (running(this, timer)) then  ! add the time since it was started
      call cpu_time (cpu_now)
      cpu = cpu + (cpu_now - timer%cpu_start)
    end if
  end subroutine timer_tree_read

!!!! AUXILIARY PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! Accumulate the elapsed time for all running timers, which are precisely
  !! those starting with the current one and walking back the parent link to
  !! the root.  Elapsed time is normally accumulated only a timer is stopped.
  subroutine accumulate_elapsed_time (this)
    class(timer_tree), intent(in) :: this
    type(node), pointer :: timer
    real :: cpu_now
    ASSERT(associated(this%current))
    call cpu_time (cpu_now)
    timer => this%current
    do while (associated(timer%parent))
      timer%cpu_elapsed = timer%cpu_elapsed + (cpu_now - timer%cpu_start)
      timer%cpu_start = cpu_now
      timer => timer%parent
    end do
  end subroutine accumulate_elapsed_time

  !! Return a pointer to the node (timer) having the specified handle,
  !! or a null pointer if no such node is found.
  function timer_with_handle (tree, handle) result (timer)
    type(timer_tree), intent(in) :: tree
    integer, intent(in) :: handle
    type(node), pointer :: timer
    timer => search(tree%root%child)
  contains
    recursive function search (timer) result (ptr)
      type(node), pointer :: timer, ptr
      ptr => timer
      if (.not.associated(timer)) return
      if (timer%handle == handle) return
      ptr => search(timer%child)
      if (.not.associated(ptr)) ptr => search(timer%sibling)
    end function search
  end function timer_with_handle

  !! Set optional error return arguments or stop with message.
  subroutine error_handler (message, stat, errmsg)
#ifdef NAGFOR
    use f90_unix, only: exit
#endif
    use,intrinsic :: iso_fortran_env, only: error_unit
    character(*), intent(in) :: message
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    if (present(stat)) then
      stat = -1
      if (present(errmsg)) errmsg = message
      return
    else
      write(error_unit,'(a)') message
      call exit (1)
    end if
  end subroutine error_handler

  !! Number of timers in the tree; maintained in root%handle.
  integer function timer_tree_size (this) result (n)
    class(timer_tree), intent(in) :: this
    n = 0
    if (associated(this%root)) n = this%root%handle
  end function timer_tree_size

  !! Returns true if the target of the timer pointer is running.
  logical function running (this, timer)
    class(timer_tree), intent(in) :: this
    type(node), pointer :: timer, t
    running = .true.
    t => this%current
    do while (associated(t%parent))
      if (associated(t, timer)) return
      t => t%parent
    end do
    running = .false.
  end function running

!!!! PROCEDURES OPERATING ON THE GLOBAL TIMER TREE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine start_timer (name, handle)
    character(*), intent(in) :: name
    integer, intent(out), optional :: handle
    call global%start (name, handle)
  end subroutine start_timer

  subroutine stop_timer (name, stat, errmsg)
    character(*), intent(in) :: name
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    call global%stop (name, stat, errmsg)
  end subroutine stop_timer

  subroutine write_timer_tree (unit, indent, handle)
    integer, intent(in) :: unit
    integer, intent(in) :: indent
    integer, intent(in), optional :: handle
    call global%write (unit, indent, handle)
  end subroutine write_timer_tree

  subroutine serialize_timer_tree (tree, name, cpu)
    integer, allocatable, intent(out) :: tree(:)
    character(:), allocatable, intent(out) :: name(:)
    real, allocatable, intent(out) :: cpu(:)
    call global%serialize (tree, name, cpu)
  end subroutine serialize_timer_tree

  subroutine deserialize_timer_tree (tree, name, cpu)
    integer, intent(in) :: tree(:)
    character(*), intent(in) :: name(:)
    real, intent(in) :: cpu(:)
    call global%deserialize (tree, name, cpu)
  end subroutine deserialize_timer_tree

  subroutine reset_timer_tree ()
    call timer_tree_delete (global)
  end subroutine reset_timer_tree

  subroutine read_timer (handle, cpu)
    integer, intent(in) :: handle
    real, intent(out) :: cpu
    call global%read (handle, cpu)
  end subroutine read_timer

end module timer_tree_type
