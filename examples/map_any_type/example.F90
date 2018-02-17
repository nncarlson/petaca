program example

  use map_any_type

  type(map_any) :: map, map_copy
  type(map_any_iterator) :: iter
  class(*), pointer :: value
#if defined(INTEL_BUG20180115)
  class(*), pointer :: uptr
#endif

  type point
    real x, y
  end type

  if (.not.map%empty())  print *, 'error: map is not empty!'

  !! Insert some elements into the map; note the different types.
  call map%insert ('page', 3)
  call map%insert ('size', 1.4)
  call map%insert ('color', 'black')
  call map%insert ('origin', point(1.0, 2.0))

  !! Replace existing mapping with a different value of different type.
  call map%insert ('size', 'default')

  !! Remove a mapping.
  call map%remove ('color')
  if (map%mapped('color')) print *, 'error: mapping not removed!'

  !! Retrieve a specific value.
  value => map%value('origin')

  !! Write the contents.
  iter = map_any_iterator(map)
  do while (.not.iter%at_end())
#if defined(INTEL_BUG20180115)
    uptr => iter%value()
    select type (uptr)
#else
    select type (uptr => iter%value())
#endif
    type is (integer)
      print *, iter%key(), ' = ', uptr
    type is (real)
      print *, iter%key(), ' = ', uptr
    type is (character(*))
      print *, iter%key(), ' = ', uptr
    type is (point)
      print *, iter%key(), ' = ', uptr
    end select
    call iter%next
  end do

  !! Make a copy of the map.
  map_copy = map

  !! Delete the contents of map; map_copy is unchanged.
  call map%clear
  if (map%size() /= 0) print *, 'error: map size is not 0!'
  if (map_copy%empty()) print *, 'error: map_copy is empty!'

  call map_copy%clear

end program
