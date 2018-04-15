.. _map_any-module:

=======================
The map_any_type module
=======================
The ``map_any_type`` module defines a map data structure or associative
array which stores (key, value) pairs as the elements of the structure.
The keys are unique and are regarded as mapping or indexing to the value
associated with the key.  In this implementation keys are character strings
but the values may be a scalar value of any intrinsic or derived type.  An
associated iterator derived type is also defined which gives sequential
access to all elements of the map structure.

Synopsis
========

.. code-block:: fortran

    use map_any_type
    type(map_any) :: map
    type(map_any_iterator) :: iter

The map_any derived type
========================
The derived type ``map_any`` defines a map data structure which stores
(key, value) pairs.  The keys are unique character strings that map (or index)
to the value associated with the key. Values may be a scalar of any intrinsic
or derived type. There are limitations, however, with some derived type values;
see the caution_ below. The derived type has the following properties:

* ``map_any`` objects are properly finalized when deallocated or when they
  otherwise cease to exist.

* Scalar assignment is defined for ``map_any`` objects with the expected
  semantics.  The contents of the lhs map are first cleared, and then the lhs
  map defined with the same (key, value) pairs as the rhs map, becoming an
  independent copy of the rhs map; but see the caution_ below for derived
  type values.

* The structure constructor ``map_any()`` evaluates to an empty map,
  and ``map_any`` variables come into existence as empty maps.

Type bound subroutines
----------------------

``insert(key, value)``
    Add the specified key and associated value to the map.  If the mapping
    already exists, its value is replaced with the specified one. ``key`` is
    a character string and ``value`` may be a scalar of any intrinsic or
    derived type. A copy of ``value`` is stored in the map; see the caution_
    below for derived type values.

``remove(key)``
    Remove the specified key from the map and deallocate the associated value.
    If the mapping does not exist, the map is unchanged.

``clear()``
    Remove all elements from the map.

Type bound functions
--------------------

``mapped(key)``
    Return true if a mapping for the specified key exists.

``value(key)``
    Return a ``class(*)`` pointer to the mapped value for the specified
    key, or a null pointer if the map does not contain the key.

``size()``
    Return the number of elements in the map.

``empty()``
    Return true if the map contains no elements; otherwise false.

.. caution::
   :name: caution

   Derived type values with pointer components, direct or indirect, should
   only be used advisedly. The map values are sourced-allocation copies of
   the values passed to the ``insert`` procedure, and this only makes a
   *shallow* copy of any pointer component. The original pointer and its
   copy will have the same target; no copy of the target is made. This also
   applies to map assignment where the values in the lhs map are
   sourced-allocation copies of the values in the rhs map.

The map_any_iterator derived type
=================================
The derived type ``map_any_iterator`` provides a means of iterating through
the elements of a ``map_any`` object, sequentially visiting each element of
a map once and only once. A defined ``map_any_iterator`` object is positioned
at a particular element of its associated map, or at a pseudo-position
*the-end*, and can be queried for the key and value of that element.

The structure constructor ``map_any_iterator(map)`` evaluates to an iterator
positioned at the the initial element of the specified ``map``, or the-end if
``map`` is empty, and ``map_any_iterator`` variables are initialized by
assignment from such structure constructor expressions; for example,

.. code-block:: fortran

   type(map_any) :: map
   type(map_any_iterator) :: iter
   iter = map_any_iterator(map)

More generally, scalar assignment is defined for ``map_any_iterator`` objects.
The lhs iterator becomes associated with the same map as the rhs iterator, and
is positioned at the same element. Subsequent changes to one iterator do not
affect the other.

Type bound subroutine
---------------------

``next()``
    Advance the iterator to the next element in the map, or to the-end if
    there are no more elements remaining to be visited.  This call has no
    effect if the iterator is already positioned at the-end.

Type bound functions
--------------------

``at_end()``
    Return true if the iterator is positioned at the-end; otherwise false.

``key()``
    Return the character string key for the current map element.
    The iterator must not be positioned at the-end.

``value()``
    Return a ``class(*)`` pointer to the value of the current map element.
    The iterator must not be positioned at the-end.

An example
==========

.. code-block:: fortran

    use map_any_type

    type(map_any) :: map, map_copy
    type(map_any_iterator) :: iter
    class(*), pointer :: value

    type point
      real x, y
    end type

    !! Maps come into existence well-defined and empty.
    if (.not.map%empty()) print *, 'error: map is not empty!'

    !! Insert some elements into the map; note the different types.
    call map%insert('page', 3)
    call map%insert('size', 1.4)
    call map%insert('color', 'black')
    call map%insert('origin', point(1.0, 2.0))

    !! Replace an existing mapping with a new value of different type.
    call map%insert('size', 'default')

    !! Remove a mapping.
    call map%remove('color')
    if (map%mapped('color')) print *, 'error: mapping not removed!'

    !! Retrieve a specific value.
    value => map%value('origin')

    !! Write the contents, using an iterator to access all elements.
    iter = map_any_iterator(map)
    do while (.not.iter%at_end())
      select type (uptr => iter%value())
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
