===============
Parameter Lists
===============

A **parameter list** is a hierarchical data structure consisting of parameter name/value pairs. It provides a structured way to pass data between program units. This is particularly useful when long argument lists would otherwise be required or when heterogeneous arguments must be encapsulated within a single interface type. The implementation is modeled after the ``Teuchos::ParameterList`` C++ class. It is intended for lightweight data such as scalars and small arrays. It is not designed as a general-purpose container for large data.

The module ``parameter_list_type`` defines the data structure and associated procedures. The module ``parameter_list_json`` provides additional procedures for input/output using JSON-formatted text.

Core Capabilities
=================
* **Value Types**: Values may be scalars, rank-1 arrays, or rank-2 arrays of arbitrary type and kind. Values may also be parameter lists (sublists), enabling hierarchical organization of data.

  .. caution::

     Values of arbitrary derived type may be stored in a parameter list.
     The ``set`` and ``get`` procedures use shallow copy semantics, so
     derived types containing allocatable or pointer components require
     careful handling. Such values are not representable in JSON input/output.

* **Polymorphic Interfaces**: Parameter lists provide a single interface type for procedures in abstract base classes, allowing structured arguments to be passed through polymorphic variables without knowledge of the dynamic type.

* **JSON Support**: Parameter lists may be constructed programmatically via ``set`` calls or populated from JSON-formatted input text.

.. admonition:: Relationship to JSON

   The parameter list structure resembles JSON in its representation of
   hierarchical name/value data. JSON permits jagged arrays and
   heterogeneous element types. While this flexibility is appropriate
   for data interchange, it does not align with Fortran's strongly typed,
   regular array model. Parameter lists therefore represent arrays as
   regular, single-type Fortran arrays, enabling direct use without
   additional structural or type checks.

   At the same time, parameter lists can represent values that JSON
   cannot express natively, including complex numbers and other
   Fortran-specific types. Within a Fortran application, parameter
   lists provide a representation that is both consistent with the
   language's type system and capable of expressing values outside
   JSON's native model.

A simple example
================
The following example illustrates basic usage.

.. code-block:: fortran

   use parameter_list_type

   type(parameter_list) :: plist
   type(parameter_list), pointer :: sublist
   integer :: p
   character(:), allocatable :: f

   !! Parameter lists come into existence well-defined and empty.
   !! Define some parameters; note the different types and ranks.
   call plist%set('page', 3)
   call plist%set('size', 1.4)
   call plist%set('color', 'blue')
   call plist%set('boundingbox', [10,10,90,90])
   call plist%set('crop', .true.)

   !! Replace an existing parameter value with a different value
   !! of different type, but same rank.
   call plist%set('size', 'default')

   !! Retrieve a specific parameter value; its type must match P.
   call plist%get('page', p)

   !! Retrieve a parameter that is not defined;
   !! it is created with the specified default value
   call plist%get('font', f, default='courier')

   !! Create a sublist parameter named 'picture'
   sublist => plist%get('picture')

   !! Define a parameter in the sublist
   call sublist%set('origin', [1.0, 2.0])

Instead of populating a parameter list through set calls, the parameters
can be read from a JSON format text file. Suppose the file ``input.json``
contains the following text:

.. code-block:: json

   {
     "page": 3, "size": 1.4, "color": "blue",
     "boundingbox": [10,10,90,90], "crop": true,
     "picture": { "origin": [1.0, 2.0] }
   }

Then this code will populate a parameter list with the same values.

.. code-block:: fortran

   use parameter_list_type
   use parameter_list_json

   type(parameter_list), pointer :: plist
   character(:), allocatable :: errmsg
   integer :: unit

   open(newunit=unit,file='input.json',status='old',action='read',access='stream')
   call parameter_list_from_json_stream(unit, plist, errmsg)

The two approaches can be combined: a parameter list read from a file may
be modified using ``set``, and additional parameters may be added from JSON
input.

The parameter_list derived type
===============================
The derived type ``parameter_list`` implements the parameter list data
structure.  It has the following properties.

* Scalar assignment is defined for ``parameter_list`` variables with the
  expected semantics. The left-hand side is first deleted, and then defined
  with the same parameters and values as the right-hand side, becoming an
  independent copy; see the caution_ below on derived type values.
* The structure constructor ``parameter_list()`` evaluates to an empty
  parameter list, and ``parameter_list`` variables come into existence as
  empty parameter lists.
* ``parameter_list`` objects are properly finalized when they are deallocated
  or otherwise cease to exist.

Type bound subroutines
----------------------

Many of the following subroutines provide the optional intent-out arguments
``stat`` and ``errmsg``. If ``stat`` is present, it is assigned 0 on success
and a non-zero value on error. If an error occurs and ``errmsg`` is present,
it is assigned an explanatory message. If ``stat`` is not present and an error
occurs, the error message is written to the preconnected error unit and
execution is terminated.

``set(name, value [,stat [,errmsg]])``
    Define a parameter with the specified ``name`` and assign it the specified
    ``value``, which may be a scalar or rank-1 or rank-2 array of any type.
    A copy of the value, created by sourced allocation, is stored
    in the parameter list; see the caution_ below for derived type values.
    If the parameter already exists, it must not be a sublist parameter and
    its existing value must have the same rank as ``value``, but not
    necessarily the same type; its value is overwritten with ``value``.

``get(name, value [,stat [,errmsg]] [,default])``
    Retrieve the value of the parameter ``name``. A copy of the value is
    returned in ``value``, which may be a scalar, or rank-1 or rank-2 array
    of the following intrinsic types: ``integer(int32)``, ``integer(int64)``,
    ``real(real32)``, ``real(real64)``, ``complex(real32)``, ``complex(real64)``,
    default ``logical``, and default ``character``.
    The kind parameters are those from the intrinsic module
    ``iso_fortran_env``, and should cover the default integer and real kinds,
    as well as double precision. An array ``value`` must be allocatable and
    a character ``value`` must be deferred-length allocatable. In these latter
    cases, ``value`` is allocated with the proper size/length to hold the
    parameter value. If present, the optional argument ``default`` must have
    the same type, kind, and rank as ``value``. If the named parameter does
    not exist, it is created with the value prescribed by ``default``, and
    that value is returned in ``value``. It is an error if the named parameter
    does not exist and ``default`` is not present, if the named parameter is a
    sublist, or if the type, kind, and rank of ``value`` does not match the
    stored value. Use ``get_any`` when the type of the parameter value is not
    one of those handled by this method.

``get_any(name, value [,stat [,errmsg]] [,default])``
    Retrieves the value of the parameter ``name``.  A copy of the value is
    returned in ``value``, which is an allocatable ``class(*)`` variable or
    rank-1 or rank-2 array.  This is a more general version of ``get`` that
    can retrieve any type of parameter value. The drawback of ``get_any`` is
    that application code must use a ``select type`` construct to access the
    returned value, making it more cumbersome. If present, the optional
    argument ``default`` must have the same rank as ``value``. If the named
    parameter does not exist, it is created with the value prescribed by
    ``default``, and that value is returned in ``value``.  It is an error if
    the named parameter does not exist and ``default`` is not present. It is
    an error if the named parameter is a sublist. It is an error if the rank
    of ``value`` does not match that of the stored value of the named
    parameter.

.. note::
   Arrays returned by ``get`` and ``get_any`` will have the default index
   lower bounds of 1 and not the lower bounds of the array passed to ``set``.
   This is an unfortunate consequence of the semantics of array passing in
   Fortran.

``set_path(path)``
    Sets the path of the parameter list to ``path``. This subroutine is not
    normally needed, because the path of a parameter list has an automatically
    defined value which follows the `JSONPath <https://goessner.net/articles/JsonPath/>`_ specification: a local
    ``parameter_list`` variable has a default path of "``$``" (the root), and
    the default path of a parameter list created by ``sublist`` is the
    concatenation of the path of the parent parameter list, the character
    "``.``", and the sublist parameter name.

Type bound functions
--------------------

``sublist(name [,stat [,errmsg]])``
    Returns a ``type(parameter_list)`` pointer to the named parameter
    sublist.  The parameter is created with an empty sublist value if it
    does not already exist. It is an error if the parameter exists but is
    not a sublist.

``is_parameter(name)``
    Returns true if there is a parameter with the given ``name``;
    otherwise false.

``is_sublist(name)``
    Returns true if there is a sublist parameter with the given ``name``;
    otherwise false.

``is_scalar(name)``
    Returns true if there is a scalar-valued parameter with the given ``name``;
    otherwise false.

``is_vector(name)``
  Returns true if there is a vector-valued parameter with the given ``name``;
  otherwise false.

``is_matrix(name)``
  Returns true if there is a matrix-valued parameter with the given ``name``;
  otherwise false.

``count()``
  Returns the number of parameters stored in the parameter list.

``path()``
  Returns the path of the parameter list; see ``set_path``.

.. caution::
  :name: caution

  Derived type values with pointer components, direct or indirect, require
  careful handling. Values are stored as sourced-allocation copies of those
  passed to ``set``. This makes a *shallow* copy of any direct or indirect
  pointer component: the original pointer and its copy share the same target,
  and no copy of the target is made. The same applies to ``parameter_list``
  assignment, whose values in the left-hand side are sourced-allocation copies
  of those in the right-hand side.


The parameter_list_iterator derived type
========================================
Parameter values in a ``parameter_list`` can be accessed directly when their
names are known. The ``parameter_list_iterator`` type provides an iterator for
traversing the parameters, visiting each parameter exactly once. A
``parameter_list_iterator`` is positioned either at a specific parameter of its
associated list, or at a distinguished end position (*the-end*).

Scalar assignment is defined for
``parameter_list_iterator`` objects. After assignment, the left-hand side
refers to the same parameter list and position as the right-hand side.
Subsequent changes to one iterator do not affect the
other. An iterator object is normally defined by assignment from a structure
constructor expression; see below.

An iterator is associated with a particular ``parameter_list``. If the
underlying parameter list is modified (for example, by adding or removing
parameters), any associated iterators become invalid. Further use of such
iterators results in undefined behavior.

The order in which parameters are visited is unspecified. Applications
must not rely on any particular iteration order.

Constructor
-----------

``parameter_list_iterator(plist [,sublists_only])``
  Returns an iterator positioned at the initial parameter of the parameter
  list ``plist``, or the-end if the parameter list is empty. If the optional
  logical argument ``sublists_only`` is present with value true, parameters
  other than sublists are skipped by the iterator.

Constructor expressions are used to initialize iterator objects:

.. code-block:: fortran

   type(parameter_list) :: plist
   type(parameter_list_iterator) :: iter
   iter = parameter_list_iterator(plist)

Type bound subroutine
---------------------

``next()``
  Advances the iterator to the next parameter in the list, or to the-end if
  there are no more parameters remaining to be visited. If the iterator is
  already at *the-end*, the call has no effect.

Type bound functions
--------------------

Unless otherwise noted, the following functions require that the iterator
not be positioned at *the-end*.

``at_end()``
  Returns true if the iterator is positioned at the-end; otherwise false.

``name()``
  Returns the name of the current parameter.

``is_sublist()``
  Returns true if the current parameter value is a sublist; otherwise false.

``is_scalar()``
  Returns true if the current parameter has a scalar value; otherwise
  false.

``is_vector()``
  Returns true if the current parameter has a rank-1 array value; otherwise
  false.

``is_matrix()``
  Returns true if the current parameter has a rank-2 array value; otherwise
  false.

``sublist()``
  Returns a ``parameter_list`` pointer associated with the current parameter
  value if it is a sublist; otherwise it returns a ``null()`` pointer.

``scalar()``
  Returns a ``class(*)`` pointer to the current parameter value if it is a
  scalar value; otherwise it returns a ``null()`` pointer.

``vector()``
  Returns a ``class(*)`` rank-1 array pointer to the current parameter value
  if it is a vector value; otherwise it returns a ``null()`` pointer.

``matrix()``
  Returns a ``class(*)`` rank-2 array pointer to the current parameter value
  if it is a matrix value; otherwise it returns a ``null()`` pointer.

``count()``
  Returns the number of remaining parameters, including the current one.

Parameter list values are stored internally in objects of class
``parameter_value``. There are four different concrete extensions of this
abstract type: ``any_scalar``, which stores a scalar value of any intrinsic
or derived type; ``any_vector``, which stores a rank-1 array value of any
intrinsic or derived type; ``any_matrix``, which stores a rank-2 array value
of any intrinsic or derived type; and ``parameter_list`` itself. This internal
implementation detail is typically not relevant to users; all of the procedures
described so far hide this detail, for example. The following procedure is
the exception.

``value()``
  Returns a ``class(parameter_value)`` pointer to an object that holds
  the value of the current parameter. The iterator must not be positioned
  at the-end. A ``select type`` construct with type-guard blocks for each
  possible dynamic type is required to access the value. In most cases, the
  preceding functions are more convenient. For example, with sublists it is
  easier to use the ``is_sublist`` method to identify whether the current
  parameter is a sublist, and if so use the ``sublist`` method to access the
  sublist.


Parameter list input/output using JSON
======================================

This section describes the JSON subset supported by ``parameter_list_json``.
JSON is a widely used data interchange format. A parameter list whose primitive
values are of intrinsic types (integer, real, character, logical) can be
represented quite naturally as JSON text, subject to the following restrictions:

* A parameter list is represented by a JSON *object*, which is an unordered
  list of comma-separated *name* : *value* pairs enclosed in braces
  (``{`` and ``}``).

* A parameter name and value are represented by a *name* : *value* pair
  of the object:

  * A *name* is a string enclosed in double quotes.
  * A *value* may be a string (in double quotes), an integer, a real
    number, or a boolean (the tokens ``true`` or ``false``).
  * A *value* may also be a JSON *array*, which is an ordered list of
    comma-separated *values* enclosed in brackets (``[`` and ``]``). To
    represent an array parameter value, the values in a JSON array are
    restricted to scalars of the same primitive type or such JSON arrays
    themselves. Nesting, however, is limited to 1 level (rank-2 arrays)
    and the sub-arrays must all have the same length. The values are listed
    in Fortran array element order. In other words, JSON arrays are limited
    to structures that correspond exactly to a rank-1 or rank-2 Fortran array
    of intrinsic type. JSON generally allows jagged arrays of any JSON values,
    possibly of differing types.
  * A *value* may also be a JSON object that represents a parameter sublist.
  * Null values (the token ``null``) are not allowed.
  * 0-sized arrays are not allowed.

* Comments (text from ``//`` to the end of the line) are allowed. This is an
  extension to the JSON standard supported by the YAJL library used for parsing.


The ``parameter_list_json`` module provides the following procedures for
creating a parameter list object from JSON text and for producing a JSON
text representation of a parameter list object.

.. note::

   When reading JSON text, booleans are converted to logical values of default
   kind, integer numbers are converted to default-kind integers, and real
   numbers to real(real64).. For numbers, this reflects
   the behavior of the YAJL parser. A future enhancement could allow for these
   values to be converted to user-specified kinds before being added to the
   parameter list.

``call parameter_list_from_json_stream(unit, plist, errmsg)``
  Reads JSON text from the given logical ``unit``, which must be connected for
  unformatted stream access, and creates the corresponding parameter list, as
  described above. The intent-out ``type(parameter_list)`` pointer argument
  ``plist`` returns the created parameter list. An unassociated return value
  indicates an error, in which case errmsg is assigned an explanatory message.

``call parameter_list_from_json_stream(unit, name, plist, errmsg)``
  Does exactly the same thing as the preceding subroutine except that ``plist``
  is assigned the given ``name`` instead of the default "$".  Although the
  name can be reset after the fact, this would not be reflected in the names
  of any sublists created by the stream, whose names are automatically
  generated from the name of their parent parameter lists.

``call parameter_list_from_json_string(string, plist, errmsg)``
  Equivalent to parameter_list_from_json_stream except that the JSON text is
  read from the character variable ``string``.

``call parameters_from_json_stream(unit, plist, stat, errmsg)``
  This differs from ``parameter_list_from_json_stream`` in that ``plist``
  is an intent-inout variable and the parameters read from the stream are
  *added* to ``plist``. In the event of an error, the integer ``stat``
  returns a non-zero value, and the allocatable deferred-length character
  argument ``errmsg`` is assigned an explanatory error message.

``call parameters_from_json_string(string, plist, stat, errmsg)``
  Does exactly the same thing as ``parameters_from_json_stream`` except that
  the JSON text is read from the character variable ``string``.

``call parameter_list_to_json(plist, unit [,real_format] [,compact])``
  Writes the JSON text representation of the parameter list ``plist``
  to ``unit``, which must be connected for formatted write access.
  Values other than sublists must be intrinsic types representable in JSON:
  logical, integer, real, or character.
  The edit descriptor to use for writing real values can be specified by
  ``real_format``; the default is ``"es12.5"``. The output is formatted with
  indentation and line breaks by default, using white space, multiple lines,
  and indentation to express the hierarchical structure of the parameter list.
  However, if ``compact`` is
  specified with value true, the output is a single line without any white
  space (mostly). This may be more useful for piping into downstream utilities.

  .. note::
     JSON is very strict about the syntax of a real number. In particular, the
     decimal point must be preceded by and followed by a digit. For some real
     edit descriptors (depending on compiler) the output will not be strictly
     valid JSON. For example, outputting ``1.`` or ``.1`` instead of ``1.0``
     or ``0.1``.


Version History & Updates
=========================
New in v24.12

* Expanded Complex Support: Support for 32 and 64-bit complex arguments has been added to the generic ``get`` subroutine.

* Set Subroutine: The ``set`` subroutine has always supported arguments of any type.

New in v23.12

* Argument Reordering: The optional default argument for ``get`` and ``get_any`` has been moved to the last position.

  - Update Requirement: Applications passing this argument without the ``default=`` keyword will need to be updated.

  - Benefit: This allows the use of ``stat`` and ``errmsg`` arguments without needing keywords, making usage less verbose.

* Renamed Procedures: To more accurately reflect their meaning, ``name`` and ``set_name`` have been renamed to ``path`` and ``set_path``.

* JSON Enhancements: Optional arguments ``real_format`` and ``compact`` were added to the ``parameter_list_to_json`` subroutine.

