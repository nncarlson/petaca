program test_map_any_type

  use map_any_type
#ifdef NAGFOR
  use f90_unix, only: exit
#endif

  integer :: stat = 0

  call test_insertion_removal
  call test_value_retrieval
  call test_map_overwrite
  call test_iterator
  call test_clear
  call test_assignment

  call exit (stat)

contains

  subroutine test_insertion_removal

    type(map_any) :: map
    class(*), pointer :: value

    if (map%size() /= 0) call write_fail ('size not 0')
    if (.not.map%empty()) call write_fail ('map not empty')

    call map%insert ('first item', 1)
    if (map%size() /= 1) call write_fail ('size not 1')
    if (map%empty()) call write_fail ('map is empty')
    value => map%value('first item')
    if (.not.associated(value)) call write_fail ('first item not found')

    call map%insert ('middle item', 2)
    if (map%size() /= 2) call write_fail ('size not 2')
    value => map%value('middle item')
    if (.not.associated(value)) call write_fail ('middle item not found')

    call map%insert ('another item', 3)
    if (map%size() /= 3) call write_fail ('size not 3')
    value => map%value('another item')
    if (.not.associated(value)) call write_fail ('another item not found')

    call map%insert ('last item', 4)
    if (map%size() /= 4) call write_fail ('size not 4')
    value => map%value('last item')
    if (.not.associated(value)) call write_fail ('last item not found')

    call map%remove ('last item')
    if (map%size() /= 3) call write_fail ('size not 3')
    value => map%value('last item')
    if (associated(value)) call write_fail ('last item found')

    value => map%value('middle item')
    call map%remove ('middle item')
    if (.not.associated(value)) call write_fail ('middle item not found')
    if (map%size() /= 2) call write_fail ('size not 2')
    value => map%value('middle item')
    if (associated(value)) call write_fail ('middle item found')

    value => map%value('first item')
    if (.not.associated(value)) call write_fail ('first item not found')
    call map%remove ('first item')
    if (map%size() /= 1) call write_fail ('size not 1')
    value => map%value('first item')
    if (associated(value)) call write_fail ('first item found')

    value => map%value('another item')
    if (.not.associated(value)) call write_fail ('another item not found')
    call map%remove ('another item')
    if (map%size() /= 0) call write_fail ('size not 0')
    value => map%value('another item')
    if (associated(value)) call write_fail ('another item found')
    if (.not.map%empty()) call write_fail ('map not empty')

  end subroutine test_insertion_removal

  subroutine test_value_retrieval

    type(map_any) :: map
    class(*), pointer :: value

    type foo
      integer n
    end type
    type(foo) :: adt

    call map%insert ('item 1', 1)
    value => map%value('item 1')
    select type (value)
    type is (integer)
      if (value /= 1) call write_fail ('value not 1')
    class default
      call write_fail ('not an integer value')
    end select

    call map%insert ('item 2', 1.0)
    value => map%value('item 2')
    select type (value)
    type is (real)
      if (value /= 1.0) call write_fail ('value not 1.0')
    class default
      call write_fail ('not an real value')
    end select

    call map%insert ('item 3', 1.0d0)
    value => map%value('item 3')
    select type (value)
    type is (double precision)
      if (value /= 1.0d0) call write_fail ('value not 1.0d0')
    class default
      call write_fail ('not an double precision value')
    end select

    call map%insert ('item 4', .true.)
    value => map%value('item 4')
    select type (value)
    type is (logical)
      if (value .neqv. .true.) call write_fail ('value not T')
    class default
      call write_fail ('not an logical value')
    end select

    call map%insert ('item 5', 'foobar')
    value => map%value('item 5')
    select type (value)
    type is (character(*))
      if (value /= 'foobar' .or. len(value) /= len('foobar')) call write_fail ('strings don''t match')
    class default
      call write_fail ('not an character value')
    end select

    adt%n = 1
    call map%insert ('item 6', adt)
    value => map%value('item 6')
    select type (value)
    type is (foo)
      if (value%n /= adt%n) call write_fail ('wrong derived type value')
    class default
      call write_fail ('not a type(foo) value')
    end select

  end subroutine test_value_retrieval

  subroutine test_map_overwrite

    type(map_any) :: map
    class(*), pointer :: value

    call map%insert ('foo', 1)
    call map%insert ('foo', 'lobster')
    value => map%value('foo')
    if (.not.associated(value)) call write_fail ('foo not found')
    select type (value)
    type is (character(*))
      if (value /= 'lobster') call write_fail ('wrong string value')
    class default
      call write_fail ('not a character value')
    end select

  end subroutine test_map_overwrite

  subroutine test_iterator

    type(map_any) :: map
    type(map_any_iterator) :: mapit

    character(1) :: keys(4) = ['A', 'B', 'C', 'D']
    logical :: tag(size(keys)) = .false.
    integer :: j, n
    class(*), pointer :: value

    do j = 1, size(keys)
      call map%insert (keys(j), j)
    end do

    mapit = map_any_iterator(map)
    do j = 1, size(keys)
      if (mapit%at_end()) then
        call write_fail ('iterator at end prematurely')
        exit
      end if
      value => mapit%value()
      select type (value)
      type is (integer)
        n = value
      end select
      if (mapit%key() /= keys(n)) call write_fail ('key doesn''t match value')
      if (tag(n)) call write_fail ('element already visited')
      tag(n) = .true.
      call mapit%next
    end do
    if (.not.mapit%at_end()) call write_fail ('iterator not at end')
    if (any(.not.tag)) call write_fail ('not all elements visited')

  end subroutine test_iterator

  subroutine test_clear
    type(map_any) :: map
    call map%insert ('a', 1)
    call map%insert ('b', 2)
    call map%clear
    if (map%size() /= 0) call write_fail ('map not empty')
  end subroutine test_clear

  subroutine test_assignment

    type(map_any) :: map1, map2
    class(*), pointer :: value

    !! Create a map, which will be the lhs of the assignment.
    call map1%insert ('a', 1)
    call map1%insert ('b', 2)

    !! Create another map which will be the rhs of the assignment.
    call map2%insert ('x', 1.0)
    call map2%insert ('y', 2.0)
    call map2%insert ('z', 3.0)

    map1 = map2

    !! Check that map1 == map2
    if (map1%size() /= map2%size()) call write_fail ('map wrong size')
    value => map1%value('x')
    if (.not.associated(value)) then
      call write_fail ('x not found')
    else
      select type (value)
      type is (real)
        if (value /= 1.0) call write_fail ('wrong value')
      class default
        call write_fail ('wrong value type')
      end select
    end if
    value => map1%value('y')
    if (.not.associated(value)) then
      call write_fail ('y not found')
    else
      select type (value)
      type is (real)
        if (value /= 2.0) call write_fail ('wrong value')
      class default
        call write_fail ('wrong value type')
      end select
    end if
    value => map1%value('z')
    if (.not.associated(value)) then
      call write_fail ('z not found')
    else
      select type (value)
      type is (real)
        if (value /= 3.0) call write_fail ('wrong value')
      class default
        call write_fail ('wrong value type')
      end select
    end if

    !! Delete map2 and check that map1 is unchanged.
    call map2%clear
    value => map1%value('x')
    if (.not.associated(value)) then
      call write_fail ('x not found')
    else
      select type (value)
      type is (real)
        if (value /= 1.0) call write_fail ('wrong value')
      class default
        call write_fail ('wrong value type')
      end select
    end if
    value => map1%value('y')
    if (.not.associated(value)) then
      call write_fail ('y not found')
    else
      select type (value)
      type is (real)
        if (value /= 2.0) call write_fail ('wrong value')
      class default
        call write_fail ('wrong value type')
      end select
    end if
    value => map1%value('z')
    if (.not.associated(value)) then
      call write_fail ('z not found')
    else
      select type (value)
      type is (real)
        if (value /= 3.0) call write_fail ('wrong value')
      class default
        call write_fail ('wrong value type')
      end select
    end if

  end subroutine test_assignment


  subroutine write_fail (errmsg)
    character(*), intent(in) :: errmsg
    stat = 1
    write(0,*) errmsg
  end subroutine

end program
