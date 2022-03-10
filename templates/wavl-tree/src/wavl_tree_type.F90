!!
!! WAVL_TREE_TYPE
!!
!! This module defines the WAVL_TREE derived type that implements a base
!! reference implementation of a weak AVL (WAVL) binary search tree with
!! bottom-up rebalancing [1]. It uses integer keys and integer values, and
!! includes only the basic methods of insertion, deletion, and lookup. The
!! module also defines a companion WAVL_TREE_ITERATOR derived type for
!! in-order iteration of the nodes of the tree.
!!
!! While this is a compilable and testable module, it is intended to serve
!! *only* as a template, or starting point, for creating custom containers
!! that use a binary search tree as the internal data structure; e.g., set
!! and map. Simply copy the module and modify it to suit the specific need,
!! and do not merely extend the derived type.
!!
!! [1] Haeupler, B., Sen, S., and Tarjan, S. E. 2015. Rank-Balanced Trees.
!! ACM Trans. Algorithms 11, 4. DOI:https://doi.org/10.1145/2689412
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2022  Neil N. Carlson
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

module wavl_tree_type

  implicit none
  private

  type, public :: wavl_tree
    private
    type(rbt_node), pointer :: root => null()
    integer :: tsize = 0
  contains
    procedure :: insert
    procedure :: delete
    procedure :: lookup
    final :: wavl_tree_delete
    !! Procedures for testing/debugging
    procedure :: init => deserialize
    procedure :: serialize
    procedure :: dump
    procedure :: check_ranks
  end type

  !! Ranked binary tree node
  type, public :: rbt_node
    type(rbt_node), pointer :: left => null(), right => null()
    integer :: rank = 0
    integer :: key
    integer :: val
  end type

  !! The iterator does an inorder traversal of the binary tree. This would be
  !! very simply done using recursion and coroutines, but without coroutines
  !! the iterator needs to explicitly manage the path from the root of the
  !! tree to the current node using a stack that would otherwise be handled
  !! by recursion. Note that the binary tree nodes do not contain a pointer
  !! to their parent, hence the need for a stack.

  type :: node_stack
    type(node_stack_item), pointer :: head => null()
  contains
    procedure :: push
    procedure :: pop
    procedure :: top
    procedure :: is_empty
    final :: delete_node_stack
  end type

  type :: node_stack_item
    type(rbt_node), pointer :: node => null()
    type(node_stack_item), pointer :: next => null()
  end type

  type, public :: wavl_tree_iterator
    type(node_stack) :: path
  contains
    procedure :: begin
    procedure :: key
    procedure :: value
    procedure :: next
    procedure :: at_end
  end type

contains

!!!! WAVL_TREE PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! Final subroutine for WAVL_TREE objects
  elemental subroutine wavl_tree_delete(this)
    type(wavl_tree), intent(inout) :: this
    call dealloc_rbt_node(this%root)
  contains
    pure recursive subroutine dealloc_rbt_node(node)
      type(rbt_node), pointer :: node
      if (associated(node)) then
        call dealloc_rbt_node(node%left)
        call dealloc_rbt_node(node%right)
        deallocate(node)
      end if
    end subroutine
  end subroutine

  !! Insert (KEY, VAL) into the tree.
  pure subroutine insert(this, key, val)
    class(wavl_tree), intent(inout) :: this
    integer, intent(in) :: key
    integer, intent(in) :: val
    call rbt_insert(this%root, key, val, this%tsize)
  end subroutine

  !! Delete KEY from the tree.
  pure subroutine delete(this, key)
    class(wavl_tree), intent(inout) :: this
    integer, intent(in) :: key
    call rbt_delete(this%root, key, this%tsize)
  end subroutine

  !! Return a pointer to the NODE with KEY, or NULL if none.
  function lookup(this, key) result(node)
    class(wavl_tree), intent(in) :: this
    integer, intent(in) :: key
    type(rbt_node), pointer :: node
    node => this%root
    do while (associated(node))
      if (key < node%key) then
        node => node%left
      else if (key > node%key) then
        node => node%right
      else  ! key == node%key
        return
      end if
    end do
  end function

  !! Insert (KEY, VAL) into the tree at ROOT. The ROOT pointer may be
  !! modified as a result of the insertion and rebalancing of the tree.

  pure recursive subroutine rbt_insert(root, key, val, tsize)
    type(rbt_node), pointer, intent(inout) :: root
    integer, intent(in) :: key
    integer, intent(in) :: val
    integer, intent(inout) :: tsize
    if (.not.associated(root)) then
      allocate(root)
      root%key = key
      root%val = val
      tsize = tsize + 1
    else if (key < root%key) then
      call rbt_insert(root%left, key, val, tsize)  ! output root%left may be a 0-child
      if (left_rank_diff(root) == 0) call insert_left_rebalance(root)
    else if (key > root%key) then
      call rbt_insert(root%right, key, val, tsize) ! output root%right may be a 0-child
      if (right_rank_diff(root) == 0) call insert_right_rebalance(root)
    else ! key == root%key
      root%val = val
    end if
  end subroutine

  !! Delete KEY from the tree at ROOT. The ROOT pointer may be modified
  !! as a result of the removal and rebalancing of the tree.

  pure recursive subroutine rbt_delete(root, key, tsize)
    type(rbt_node), pointer, intent(inout) :: root
    integer, intent(in) :: key
    integer, intent(inout) :: tsize
    type(rbt_node), pointer :: next, temp
    if (.not.associated(root)) return
    if (key < root%key) then
      call rbt_delete(root%left, key, tsize)
      call delete_left_rebalance(root)
    else if (key > root%key) then
      call rbt_delete(root%right, key, tsize)
      call delete_right_rebalance(root)
    else  ! key == root%key
      if (associated(root%left) .and. associated(root%right)) then
        !! Replace root value with next larger value and then
        !! delete that value from the right subtree.
        next => root%right
        do while (associated(next%left))
          next => next%left
        end do
        root%key = next%key
        call rbt_delete(root%right, next%key, tsize)
        call delete_right_rebalance(root)
      else if (associated(root%left)) then
        !! Replace root node with its left child; new root is a 2 or 3-child
        temp => root
        root => root%left
        deallocate(temp)
        tsize = tsize - 1
      else if (associated(root%right)) then
        !! Replace root node with its right child; new root is a 2 or 3-child
        temp => root
        root => root%right
        deallocate(temp)
        tsize = tsize - 1
      else  ! root is a leaf; just delete it
        deallocate(root)
        tsize = tsize - 1
      end if
    end if
  end subroutine

  !! WAVL procedure for rebalancing the subtree at ROOT after an insertion
  !! in its left subtree resulted in a left 0-child (assumed).

  pure subroutine insert_left_rebalance(root)
    type(rbt_node), pointer, intent(inout) :: root
    if (right_rank_diff(root) == 1) then ! root is 0,1
      root%rank = root%rank + 1 ! makes root 1,2
    else ! root is 0,2
      if (right_rank_diff(root%left) == 2) then
        call rotate_right(root) ! new root is 1,1
        root%right%rank = root%right%rank - 1
      else
        call rotate_left(root%left)
        call rotate_right(root) ! new root is 1,1
        root%rank = root%rank + 1
        root%left%rank = root%left%rank - 1
        root%right%rank = root%right%rank - 1
      end if
    end if
  end subroutine

  !! WAVL procedure for rebalancing the subtree at ROOT after an insertion
  !! in its right subtree resulted in a right 0-child (assumed).

  pure subroutine insert_right_rebalance(root)
    type(rbt_node), pointer, intent(inout) :: root
    if (left_rank_diff(root) == 1) then ! root is 1,0
      root%rank = root%rank + 1 ! makes root 2,1
    else ! root is 2,0
      if (left_rank_diff(root%right) == 2) then
        call rotate_left(root)
        root%left%rank = root%left%rank - 1
      else
        call rotate_right(root%right)
        call rotate_left(root)
        root%rank = root%rank + 1
        root%left%rank = root%left%rank - 1
        root%right%rank = root%right%rank - 1
      end if
    end if
  end subroutine

  !! WAVL procedure for rebalancing the subtree at ROOT after a removal
  !! in its left subtree resulted in it becoming a 2,2 leaf or with a
  !! possible left 3-child.

  pure subroutine delete_left_rebalance(root)
    type(rbt_node), pointer, intent(inout) :: root
    if (is_leaf(root)) then
      root%rank = 0 ! makes root 1,1
    else if (left_rank_diff(root) == 3) then
      if (right_rank_diff(root) == 2) then ! root is 3,2
        root%rank = root%rank - 1 ! makes root 2,1 and a 2 or 3-child
      else if (right_rank_diff(root%right) == 1) then ! 3,1 root with 1,1 or 2,1 right child
        call rotate_left(root)  ! new root is 1,2 and a 1 or 2-child
        root%rank = root%rank + 1
        root%left%rank = root%left%rank - 1
        if (is_leaf(root%left)) root%left%rank = root%left%rank - 1
      else if (left_rank_diff(root%right) == 1) then ! 3,1 root with 1,2 right child
        call rotate_right(root%right)
        call rotate_left(root)  ! new root is 2,2 and a 1 or 2-child
        root%rank = root%rank + 2
        root%left%rank = root%left%rank - 2
        root%right%rank = root%right%rank - 1
      else  ! 3,1 root with a 2,2 right child
        root%right%rank = root%right%rank - 1  ! root%right is 1,1
        root%rank = root%rank - 1 ! root is 2,1 and a 2 or 3-child
      end if
    end if
  end subroutine

  !! WAVL procedure for rebalancing the subtree at ROOT after a removal
  !! in its right subtree resulted in it becoming a 2,2 leaf or with a
  !! possible right 3-child.

  pure subroutine delete_right_rebalance(root)
    type(rbt_node), pointer, intent(inout) :: root
    if (is_leaf(root)) then
      root%rank = 0 ! makes root 1,1
    else if (right_rank_diff(root) == 3) then
      if (left_rank_diff(root) == 2) then ! root is 2,3
        root%rank = root%rank - 1 ! root is 1,2 and a 2 or 3-child
      else if (left_rank_diff(root%left) == 1) then ! 1,3 root with 1,1 or 1,2 left child
        call rotate_right(root)  ! new root is 2,1 and a 1 or 2-child
        root%rank = root%rank + 1
        root%right%rank = root%right%rank - 1
        if (is_leaf(root%right)) root%right%rank = root%right%rank - 1
      else if (right_rank_diff(root%left) == 1) then ! 1,3 root with 2,1 left child
        call rotate_left(root%left)
        call rotate_right(root)  ! new root is 2,2 and a 1 or 2-child
        root%rank = root%rank + 2
        root%right%rank = root%right%rank - 2
        root%left%rank = root%left%rank - 1
      else
        root%left%rank = root%left%rank - 1  ! root%left is 1,1
        root%rank = root%rank - 1 ! root is 1,2 and a 2 or 3-child
      end if
    end if

  end subroutine

  !! Return the rank difference of the left child.
  !! The rank of a null child is -1 by convention.
  pure integer function left_rank_diff(this)
    type(rbt_node), intent(in) :: this
    if (associated(this%left)) then
      left_rank_diff = this%rank - this%left%rank
    else
      left_rank_diff = this%rank + 1
    end if
  end function

  !! Return the rank difference of the right child
  !! The rank of a null child is -1 by convention.
  pure integer function right_rank_diff(this)
    type(rbt_node), intent(in) :: this
    if (associated(this%right)) then
      right_rank_diff = this%rank - this%right%rank
    else
      right_rank_diff = this%rank + 1
    end if
  end function

  !! Return true if the node THIS is a leaf
  pure logical function is_leaf(this)
    type(rbt_node), intent(in) :: this
    is_leaf = .not.(associated(this%left) .or. associated(this%right))
  end function

  !! Right-rotate subtree at ROOT; left child becomes root.
  pure subroutine rotate_right(root)
    type(rbt_node), pointer, intent(inout) :: root
    type(rbt_node), pointer :: pivot
    pivot => root%left
    root%left => pivot%right
    pivot%right => root
    root => pivot
  end subroutine

  !! Left-rotate subtree at ROOT; right child becomes root.
  pure subroutine rotate_left(root)
    type(rbt_node), pointer, intent(inout) :: root
    type(rbt_node), pointer :: pivot
    pivot => root%right
    root%right => pivot%left
    pivot%left => root
    root => pivot
  end subroutine

!!!! PROCEDURES USEFUL FOR TESTING/DEBUGGING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! Serialize the ranked binary tree data. The data is written sequentially
  !! into ARRAY using pre-order traversal of the binary tree, with the node
  !! value followed by its rank. The size of ARRAY must be at least twice
  !! the size of the tree.

  subroutine serialize(this, array)
    class(wavl_tree), intent(in) :: this
    integer, intent(out) :: array(:)
    integer :: pos
    !ASSERT(size(array) >= 2*this%size())
    pos = 1
    call serialize_rbt(this%root)
  contains
    recursive subroutine serialize_rbt(root)
      type(rbt_node), pointer, intent(in) :: root
      if (associated(root)) then
        array(pos)   = root%key
        array(pos+1) = root%rank
        pos = pos + 2
        call serialize_rbt(root%left)
        call serialize_rbt(root%right)
      end if
    end subroutine
  end subroutine

  !! Deserialize data that describes a ranked binary tree. ARRAY contains
  !! the data according to a pre-order traversal of the binary tree, with
  !! node value followed by its rank, such as generated by the corresponding
  !! serialize procedure.

  subroutine deserialize(this, array)
    class(wavl_tree), intent(out) :: this
    integer, intent(in) :: array(:)
    integer :: pos
    pos = 1
    call deserialize_rbt(this%root, huge(array))
  contains
    recursive subroutine deserialize_rbt(root, hi)
      type(rbt_node), pointer, intent(out) :: root
      integer, intent(in), optional :: hi
      root => null()
      if (pos > ubound(array,1)) return
      if (array(pos) > hi) return
      allocate(root)
      root%key  = array(pos)
      root%rank = array(pos+1)
      pos = pos + 2
      call deserialize_rbt(root%left, root%key)
      call deserialize_rbt(root%right, hi)
    end subroutine
  end subroutine

  !! Print the internal ranked binary tree to stdout
  subroutine dump(this)
    class(wavl_tree), intent(in) :: this
    print *, '------------------'
    call rbt_dump(this%root, 0)
    print *, '------------------'
  contains
    recursive subroutine rbt_dump(root, level)
      type(rbt_node), pointer, intent(in) :: root
      integer, intent(in) :: level
      if (associated(root)) then
        call rbt_dump(root%left, level+1)
        print '(a,"(",i0,",",i0,")[",i0,":",i0,",",i0,"]")', repeat('  ', level), &
            root%key, root%val, root%rank, left_rank_diff(root), right_rank_diff(root)
        call rbt_dump(root%right, level+1)
      end if
    end subroutine
  end subroutine

  !! Returns true if all the ranks satisfy the WAVL rank rules
  logical function check_ranks(this)
    class(wavl_tree), intent(in) :: this
    check_ranks = check_ranks_(this%root)
  contains
    recursive logical function check_ranks_(root) result(okay)
      type(rbt_node), pointer, intent(in) :: root
      integer :: d(2)
      if (associated(root)) then
        d = [left_rank_diff(root), right_rank_diff(root)]
        if (minval(d) >= 1 .and. maxval(d) <= 2) then
          okay = check_ranks_(root%left) .and. check_ranks_(root%right)
        else
          okay = .false.
        end if
        if (is_leaf(root)) okay = okay .and. root%rank == 0
      else
        okay = .true.
      end if
    end function
  end function

!!!! NODE_STACK TYPE BOUND PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! Push a pointer to NODE onto the stack
  subroutine push(this, node)
    class(node_stack), intent(inout) :: this
    type(rbt_node), pointer, intent(in) :: node
    type(node_stack_item), pointer :: rest
    rest => this%head
    allocate(this%head)
    this%head%node => node
    this%head%next => rest
  end subroutine

  !! Pop the top node pointer off the stack
  pure subroutine pop(this)
    class(node_stack), intent(inout) :: this
    type(node_stack_item), pointer :: head
    if (associated(this%head)) then
      head => this%head
      this%head => this%head%next
      deallocate(head)
    end if
  end subroutine

  !! Return a pointer to the top node on the stack
  function top(this) result(node)
    class(node_stack), intent(in) :: this
    type(rbt_node), pointer :: node
    if (associated(this%head)) then
      node => this%head%node
    else
      node => null()
    end if
  end function

  !! Return true if the stack is empty
  pure logical function is_empty(this)
    class(node_stack), intent(in) :: this
    is_empty = .not.associated(this%head)
  end function

  !! Final subroutine for NODE_STACK objects
  elemental subroutine delete_node_stack(this)
    type(node_stack), intent(inout) :: this
    do while (.not.is_empty(this))
      call pop(this)
    end do
  end subroutine

!!!! WAVL_TREE_ITERATOR PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine begin(this, set)
    class(wavl_tree_iterator), intent(out) :: this
    type(wavl_tree), intent(in) :: set
    type(rbt_node), pointer :: node
    node => set%root
    do while (associated(node))
      call this%path%push(node)
      node => node%left
    end do
  end subroutine

  logical function at_end(this)
    class(wavl_tree_iterator), intent(in) :: this
    at_end = this%path%is_empty()
  end function

  integer function key(this)
    class(wavl_tree_iterator), intent(in) :: this
    type(rbt_node), pointer :: curr
    curr => this%path%top()
    key = curr%key
  end function

  integer function value(this)
    class(wavl_tree_iterator), intent(in) :: this
    type(rbt_node), pointer :: curr
    curr => this%path%top()
    value = curr%val
  end function

  subroutine next(this)
    class(wavl_tree_iterator), intent(inout) :: this
    type(rbt_node), pointer :: curr, prev
    if (at_end(this)) return  ! no-op
    curr => this%path%top()
    if (associated(curr%right)) then ! advance to next left-most node
      curr => curr%right
      do while (associated(curr))
        call this%path%push(curr)
        curr => curr%left
      end do
    else
      do
        prev => curr
        call this%path%pop
        if (this%path%is_empty()) return ! done
        curr => this%path%top()
        if (associated(prev, curr%left)) return
      end do
    end if
  end subroutine

end module wavl_tree_type
