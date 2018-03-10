.. _json-module:

===============
The json module
===============

The ``json`` module defines derived data types for representing arbitrary
JSON data, and procedures for instantiating objects of those types from JSON
text read from a file or string.

This module uses :ref:`yajl_fort <yajl_fort>` for parsing the JSON input data.

.. note::

   This module is a work-in-progress. While it provides the ability to read
   arbitrary JSON data and represent it in memory, it lacks many convenient
   methods for working with the data. Needed in particular, are methods for
   direct access to values using a "path" type of indexing.

Usage
=====

Refer to http://www.json.org for a detailed description of the JSON syntax.
The derived types and terminology used here adhere closely to that description.

The abstract type ``json_value`` represents a JSON `value`. The dynamic type
of a polymorphic instance of this class will be one of these extended types:

:``json_integer``:  stores a JSON *number* without fractional part (P)
:``json_real``:     stores a JSON *number* with fractional part (P)
:``json_string``:   stores a JSON *string* (P)
:``json_boolean``:  stores a logical for the JSON literals ``true``
                    and ``false`` (P)
:``json_null``:     represents the JSON literal ``null`` (P)
:``json_object``:   stores a JSON *object* (S)
:``json_array``:    stores a JSON *array* (S)

The primitive types (P) have a public component ``%value`` that stores the
corresponding value (except for ``json_null``). The content of the structure
types (S) are accessed via iterator objects.  For ``json_object`` values:

.. code-block:: fortran

   type(json_object), target  :: value
   type(json_object_iterator) :: iter
   iter = json_object_iterator(value)
   do while (.not.iter%at_end()) ! order of object members is insignificant
     ! iter%name() is the name of the member
     ! iter%value() is a class(json_value) pointer to the value of the member
     call iter%next
   end do

For ``json_array`` values:

.. code-block:: fortran

   type(json_array), target  :: value
   type(json_array_iterator) :: iter
   iter = json_array_iterator(value)
   do while (.not.iter%at_end()) ! order of array elements *is* significant
     ! iter%value() is a class(json_value) pointer to the value of the element
     call iter%next
   end do

The following subroutines allocate and define an allocatable
``class(json_value)`` variable with JSON text read from a string or logical
unit opened for unformatted stream input.

.. code-block:: fortran

   call json_from_string(string, value, stat, errmsg)
   call json_from_stream(unit,   value, stat, errmsg)
     character(*), intent(in) :: string
     integer, intent(in) :: unit
     class(json_value), allocatable, intent(out) :: value
     integer, intent(out) :: stat
     character(:), allocatable, intent(out) :: errmsg

The argument ``stat`` returns a nonzero value if an error occurs, and in that
case ``errmsg`` is assigned an explanatory error message.

Examples
========

Here are some examples that use ``json_from_string``. Examples using
``json_from_stream`` would be essentially the same. Note that the ``stop``
statements identify things that should not occur.

Reading primitive JSON values:

.. code-block:: fortran

   use json

   class(json_value), allocatable :: val
   character(:), allocatable :: errmsg
   integer :: stat

   call json_from_string('42', val, stat, errmsg)
   select type (val)
   type is (json_integer)
     if (val%value /= 42) stop 1
   class default
     stop 2
   end select

   call json_from_string('"foo"', val, stat, errmsg)
   select type (val)
   type is (json_string)
     if (val%value /= 'foo') stop 3
   class default
     stop 4
   end select

   call json_from_string('false', val, stat, errmsg)
   if (stat /= 0) stop 51
   select type (val)
   type is (json_boolean)
     if (val%value) stop 5
   class default
     stop 6
   end select

   call json_from_string('null', val, stat, errmsg)
   select type (val)
   type is (json_null)
   class default
     stop 7
   end select

Reading a JSON array value and iterating through its elements:

.. code-block:: fortran

   use json

   class(json_value), allocatable :: val
   type(json_array_iterator) :: iter
   character(:), allocatable :: errmsg
   integer :: stat, n

   call json_from_string('[42,"foo",false,null]', val, stat, errmsg)

   select type (val)
   type is (json_array)
     n = 0
     iter = json_array_iterator(val)
     do while (.not.iter%at_end())
       n = n + 1
       select type (ival => iter%value())
       type is (json_integer)
         if (n /= 1) stop 1
         if (ival%value /= 42) stop 2
       type is (json_string)
         if (n /= 2) stop 3
         if (ival%value /= 'foo') stop 4
       type is (json_boolean)
         if (n /= 4) stop 5
         if (ival%value) stop 6
       type is (json_null)
         if (n /= 5) stop 7
       class default
         stop 8
       end select
       call iter%next
     end do
   class default
     stop 9
   end select

Reading a JSON object value and iterating through its members:

.. code-block:: fortran

   use json

   class(json_value), allocatable :: val
   type(json_object_iterator) :: iter
   character(:), allocatable :: errmsg
   integer :: stat

   call json_from_string('{"a":42,"b":"foo","c":false}', val, stat, errmsg)

   select type (val)
   type is (json_object)
     iter = json_object_iterator(val)
     do while (.not.iter%at_end())
       select type (ival => iter%value())
       type is (json_integer)
         if (iter%name() /= 'a') stop 1
         if (ival%value /= 42) stop 2
       type is (json_string)
         if (iter%name() /= 'b') stop 3
         if (ival%value /= 'foo') stop 4
       type is (json_boolean)
         if (iter%name() /= 'y') stop 6
         if (ival%value) stop 6
       class default
         stop 7
       end select
       call iter%next
     end do
   class default
     stop 8
   end select

Error handling with invalid JSON:

.. code-block:: fortran

   use json

   class(json_value), allocatable :: val
   integer :: stat
   character(:), allocatable :: errmsg

   call json_from_string('[1,2,foo,3]', val, stat, errmsg)
   if (stat == 0) stop 1 ! should have been an error
   write(*,*) errmsg

This produces this error output when run:

.. code-block:: none

   lexical error: invalid string in json text.
                                    [1,2,foo,3]
                       (right here) ------^
