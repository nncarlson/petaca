!!
!! MAP_ANY_TYPE
!!
!! This module defines a map data structure (or associative array) which stores
!! (key, value) pairs as the elements of the structure. The keys are unique and
!! are regarded as mapping (or indexing) to the value associated with the key.
!! In this implementation keys are character strings but the values may be a
!! scalar value of any intrinsic or derived type.  An associated iterator data
!! structure is also defined which gives a sequential access to all elements of
!! the map structure.
!!
!! Neil Carlson <neil.n.carlson@gmail.com>
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2011  Neil N. Carlson
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
!! The derived types MAP_ANY and MAP_ANY_ITERATOR are defined.
!!  * Scalar assignment is defined for both types with the expected semantics.
!!  * Finalization is defined for MAP_ANY objects so that when they are
!!    deallocated, explicitly or automatically, all allocated data associated
!!    with the map is deallocated.
!!  * The structure constructor MAP_ANY() evaluates to an empty map.
!!  * The structure constructor MAP_ANY_ITERATOR(MAP) evaluates to an iterator
!!    positioned at the beginning element of the specified map.
!! The types have the following type-bound procedures.
!!
!! MAP_ANY TYPE-BOUND PROCEDURES
!!
!!  INSERT(KEY,VALUE) adds the specified key and associated value to the map.
!!    If the mapping already exists, its value is replaced with the specifed one.
!!
!!  REMOVE(KEY) removes the specified key from the map and deallocates the
!!    associated value.  If the mapping does not exist, the map is unchanged.
!!
!!  MAPPED(KEY) returns the value .TRUE. if a mapping for the specified key
!!    exists; otherwise it returns .FALSE.
!!
!!  VALUE(KEY) returns a CLASS(*) pointer to the mapped value for the specified
!!    key, or a null pointer if the map does not contain the key.
!!
!!  CLEAR() removes all elements from the map, leaving it with a size of 0.
!!
!!  EMPTY() returns .TRUE. if the map contains no elements (i.e., has size 0).
!!
!!  SIZE() returns the number of elements in the map.
!!
!! MAP_ANY_ITERATOR TYPE-BOUND PROCEDURES
!!
!!  NEXT() advances the iterator to the next element in the map.
!!
!!  AT_END() returns .TRUE. if the iterator has reached the end; that is,
!!    it has gone past the last element of the map.
!!
!!  KEY() returns the character string key for the current element.
!!
!!  VALUE() returns a CLASS(*) pointer to the value for the current element.
!!
!! A CAUTION ABOUT VALUES.  The values contained in a map are copies of the
!! values passed to the INSERT method, but they are shallow copies (or clones)
!! as created by sourced allocation.  For intrinsic types these are genuine
!! copies but for derived type values the literal contents of the object are
!! copied.  For a pointer component this means that a copy of the pointer is
!! made but not a copy of its target; the original pointer and its copy will
!! have the same target.  This also applies to the assignment of maps where
!! the values in the lhs map are sourced-allocation clones of the values in
!! the rhs map.
!!

module map_any_type

  implicit none
  private

  public :: dump

  type :: list_item
    character(:), allocatable :: key
    class(*), allocatable :: value
    type(list_item), pointer :: next => null(), prev => null()
  contains
    final :: dealloc_list_item
  end type list_item

  type, public :: map_any
    private
    type(list_item), pointer :: first => null()
  contains
    procedure :: insert
    procedure :: remove
    procedure :: value
    procedure :: mapped
    procedure :: size => map_size
    procedure :: empty
    procedure :: clear
    procedure, private :: copy
    generic :: assignment(=) => copy
    final :: dealloc_map_any
  end type map_any

  type, public :: map_any_iterator
    class(list_item), pointer, private :: item => null()
  contains
    procedure :: next => iter_next
    procedure :: at_end => iter_at_end
    procedure :: key => iter_key
    procedure :: value => iter_value
  end type map_any_iterator

  !! User-defined MAP_ANY_ITERATOR structure constructor
  interface map_any_iterator
    procedure map_any_begin
  end interface

contains

  !! Final procedure for MAP_ANY objects.  This is recursive because the
  !! CLASS(*) values in the map may themselves be MAP_ANY objects or be
  !! derived data types having a MAP_ANY component.
  recursive subroutine dealloc_map_any (this)
    type(map_any), intent(inout) :: this
    if (associated(this%first)) deallocate(this%first)
  end subroutine dealloc_map_any

  !! Final procedure for LIST_ITEM objects.  This recursively follows the
  !! NEXT pointer.  When deallocating a linked-list structure only the root
  !! needs to be explicitly deallocated.  When the desire is to deallocate a
  !! single LIST_ITEM object, first nullify the NEXT point to prevent the
  !! recursive finalization from possibly deallocating more than it should.
  recursive subroutine dealloc_list_item (this)
    type(list_item), intent(inout) :: this
    if (associated(this%next)) deallocate(this%next)
  end subroutine dealloc_list_item

  !!!! AUXILLARY ROUTINES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! Returns a pointer to a new initialized (but unlinked) LIST_ITEM.
  function new_list_item (key, value)
    character(*), intent(in) :: key
    class(*), intent(in) :: value
    type(list_item), pointer :: new_list_item
    allocate(new_list_item)
    new_list_item%key = key
    allocate(new_list_item%value, source=value)
    new_list_item%prev => new_list_item
  end function new_list_item

  !! Returns a pointer to the LIST_ITEM having the specified key,
  !! or a null pointer of none was found.
  function find_list_item (this, key) result (item)
    class(map_any), intent(in) :: this
    character(*), intent(in) :: key
    type(list_item), pointer :: item
    item => this%first
    do while (associated(item))
      if (item%key == key) exit
      item => item%next
    end do
  end function find_list_item

  !! Blindly links the given LIST_ITEM (as made by NEW_LIST_ITEM) to the end
  !! of the list; it does not check that the key is unique (someone else must).
  subroutine append_list_item (this, item)
    class(map_any), intent(inout) :: this
    type(list_item), pointer, intent(in) :: item
    type(list_item), pointer :: tail
    if (associated(this%first)) then
      tail => this%first%prev
      tail%next => item
      item%prev => tail
      this%first%prev => item
    else
      item%prev => item
      this%first => item
    end if
  end subroutine append_list_item

  !! Creates a (recursive) copy of the (forward) linked-list structure rooted
  !! with the given LIST_ITEM object.  A pointer to the copy root is returned
  !! by the function.  All the PREV pointers in the copy are properly defined,
  !! including that of the root, which points to the end of the list, and so
  !! the result is appropriate as the target of MAP_ANY%FIRST.
  recursive function item_copy (item) result (copy)
    type(list_item), intent(in) :: item
    type(list_item), pointer :: copy
    allocate(copy, source=item)
    if (associated(item%next)) then
      copy%next => item_copy(item%next)
      copy%prev => copy%next%prev
      copy%next%prev => copy
    else
      copy%prev => copy
    end if
  end function item_copy

  !!!! MAP_ANY TYPE-BOUND PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! Returns a CLASS(*) pointer to the mapped value for KEY,
  !! or a null pointer if KEY is not mapped.
  function value (this, key)
    class(map_any), intent(in) :: this
    character(*), intent(in) :: key
    class(*), pointer :: value
    type(list_item), pointer :: item
    item => find_list_item(this, key)
    if (associated(item)) then
      value => item%value
    else
      value => null()
    end if
  end function value

  !! Inserts the (KEY, VALUE) pair into the map.  If the mapping already
  !! exists its value is replaced with the specified value.
  subroutine insert (this, key, value)
    class(map_any), intent(inout) :: this
    character(*), intent(in) :: key
    class(*), intent(in) :: value
    type(list_item), pointer :: item
    item => find_list_item(this, key)
    if (associated(item)) then
      !! Replace existing value with the given one.
      !! F2008 alternative: item%value = value
      if (allocated(item%value)) deallocate(item%value)
      allocate(item%value, source=value)
    else
      call append_list_item(this, new_list_item(key, value))
    end if
  end subroutine insert

  !! Removes KEY from the map and deallocates the mapped value.
  !! If the mapping does not exist the map is unchanged.
  subroutine remove (this, key)
    class(map_any), intent(inout) :: this
    character(*), intent(in) :: key
    type(list_item), pointer :: item
    item => find_list_item(this, key)
    if (associated(item)) then
      if (associated(item%prev, item)) then ! single item list
        this%first => null()
      else if (associated(this%first, item)) then ! first item of multiple
        this%first => item%next
        item%next%prev => item%prev
      else if (.not.associated(item%next)) then ! last item of multiple
        item%prev%next => item%next
        this%first%prev => item%prev
      else ! interior item of multiple
        item%prev%next => item%next
        item%next%prev => item%prev
      end if
      item%next => null() ! stop recursive finalization when item is deallocated
      deallocate(item)
    end if
  end subroutine remove

  !! All elements of the map are removed, leaving it with a size of 0.
  subroutine clear (this)
    class(map_any), intent(inout) :: this
    if (associated(this%first)) deallocate(this%first)
  end subroutine clear

  !! Returns true if a mapping for KEY exists; otherwise returns false.
  logical function mapped (this, key)
    class(map_any), intent(in) :: this
    character(*), intent(in) :: key
    mapped = associated(find_list_item(this, key))
  end function mapped

  !! Returns true if the map contains no elements (size is 0).
  logical function empty (this)
    class(map_any), intent(in) :: this
    empty = .not.associated(this%first)
  end function empty

  !! Returns the number of elements in the map.
  integer function map_size (this)
    class(map_any), intent(in) :: this
    type(list_item), pointer :: item
    map_size = 0
    item => this%first
    do while (associated(item))
      map_size = map_size + 1
      item => item%next
    end do
  end function map_size

  !! Defined assignment operator for MAP_ANY objects.
  recursive subroutine copy (lmap, rmap)
    class(map_any), intent(inout) :: lmap ! inout in case lmap is rmap
    class(map_any), intent(in) :: rmap
    if (associated(rmap%first,lmap%first)) return ! lmap and rmap are the same
    if (associated(lmap%first)) deallocate(lmap%first)
    if (associated(rmap%first)) lmap%first => item_copy(rmap%first)
  end subroutine copy

  !!!! MAP_ANY_ITERATOR TYPE-BOUND PROCEDURES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! Defined MAP_ANY_ITERATOR constructor that is positioned
  !! to the beginning element of the specified MAP_ANY map.
  function map_any_begin (map) result (iter)
    class(map_any), intent(in) :: map
    type(map_any_iterator) :: iter
    iter%item => map%first
  end function map_any_begin

  !! Advances the iterator to the next element in the map.
  subroutine iter_next (this)
    class(map_any_iterator), intent(inout) :: this
    if(associated(this%item)) this%item => this%item%next
  end subroutine iter_next

  !! Returns true if the iterator has reached the end; that is, it has
  !! gone past the last element of the map.
  pure logical function iter_at_end (this)
    class(map_any_iterator), intent(in) :: this
    iter_at_end = .not.associated(this%item)
  end function iter_at_end

  !! Returns the key for the current element.
  function iter_key (this)
    class(map_any_iterator), intent(in) :: this
    character(:), allocatable :: iter_key
    iter_key = this%item%key
  end function iter_key

  !! Returns a CLASS(*) pointer to the value of the current element.
  function iter_value (this)
    class(map_any_iterator), intent(in) :: this
    class(*), pointer :: iter_value
    iter_value => this%item%value
  end function iter_value

  subroutine dump (this)
    type(map_any), intent(in) :: this
    type(list_item), pointer :: item, last, tail
    item => this%first
    if (associated(item)) then
      write(*,'(a)',advance='no') trim(item%key)
      tail => item%prev
      last => item
      item => item%next
      do while (associated(item))
        if (.not.associated(last,item%prev)) write(*,'(1x,a)',advance='no') 'X<'
        write(*,'(1x,a)',advance='no') trim(item%key)
        last => item
        item => item%next
      end do
      if (.not.associated(last,tail)) write(*,'(1x,a)',advance='no') 'XT'
      write(*,*)
    end if
  end subroutine

end module map_any_type
