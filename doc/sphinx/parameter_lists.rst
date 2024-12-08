===============
Parameter Lists
===============
A *parameter list* is a hierarchical data structure consisting of a collection
of parameter name/value pairs. A value can be a scalar or array of arbitrary
type, or a parameter list itself. Parameter lists are intended as a convenient
way to pass information between different program units. The module
``parameter_list_type`` implements the data structure and basic methods for
working with it, and the module ``parameter_list_json`` provides additional
procedures for parameter list input/output using JSON-format text.

.. admonition:: New in v24.12

   * Support for 32 and 64-bit arguments has been added to the generic ``get``
     subroutine. Note that the ``set`` subroutine has always supported
     arguments of any type.

.. admonition:: New in v23.12

   * The optional ``default`` argument of the ``get`` and ``get_any``
     subroutines has been moved to the last position. Applications that
     passed that argument without using its ``default=`` keyword will
     need to be updated. This change allows applications that use the
     optional ``stat`` and ``errmsg`` arguments without the default
     argument to dispense using their keywords, making usage less verbose.
   * The ``name`` and ``set_name`` procedures have been renamed ``path``
     and ``set_path`` to more accurately reflect their meaning.
   * Optional arguments ``real_format`` and ``compact`` have been added
     to the ``parameter_list_to_json`` subroutine.

Introduction
============
A parameter list is a hierarchical data structure consisting of a collection
of parameter name/value pairs. A value can be a scalar or rank-1 or 2 array
of arbitrary type and kind, or a parameter list itself, which is called a
*sublist* parameter. Parameter lists are meant for passing information between
different program units. Long argument lists can be shortened by gathering
some of the arguments into a parameter list and passing it instead. Sublists
can be used for arguments intended to be passed to lower-level procedures,
and so forth. While a parameter list can be manually populated through a
sequence of calls, it is more easily defined from JSON input text. When used
in this way, a parameter list provides a powerful and flexible means for
distributing program input to the different components of a program.

Parameter lists are also extremely useful in object oriented programming.
Consider an abstract base class that has multiple concrete implementations
(extended types). Each has an initialization procedure, but with differing
arguments specific to the implementation. Because of the differing argument
lists, the initialization procedure cannot be a deferred procedure of the
base class, so the different extended types must be exposed to application
code in order to invoke the initialization procedure. The differing
initialization arguments can instead be bundled into a parameter list
argument, to yield a common interface for the initialization procedures,
which can then be elevated to a base class procedure. Application code can
then invoke it through a base class polymorphic variable without needing to
know the specific dynamic type of the variable.

Parameter lists are intended to pass lightweight data: scalars or (very)
small arrays, typically of intrinsic types. Values of arbitrary derived
type can be used but take great caution when doing so because the set/get
methods make shallow copies of the value; see the caution_ on values below.
Also such values cannot be input/output using JSON text.

This implementation is inspired by, and modeled after, the
``Teuchos::ParameterList`` C++ class from `Trilinos <http://trilinos.org>`_.

.. admonition:: Why not use a generic JSON data structure?

   The parameter list data structure is similar to that of JSON, but there
   are some important differences that make parameter lists far better suited
   to Fortran use than a generic JSON data structure. Chief among these is
   that parameter list array values are Fortran arrays, whereas JSON allows
   jagged arrays with values of differing types. This makes array access
   natural and far simpler with parameter lists than it would be with JSON.

A simple example
----------------
A simple example will illustrate some basic usage.

.. code-block:: fortran

   use parameter_list_type

   type(parameter_list) :: plist
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

   open(newunit=unit,file='input.json',action='read',access='stream')
   call parameter_list_from_json_stream(unit, plist, errmsg)

The two methods can also be combined: a parameter list read from a file can
be modified with set methods, and an existing parameter list can be added to
with parameters read from a file.

The parameter_list derived type
===============================
The derived type ``parameter_list`` implements the parameter list data
structure.  It has the following properties.

* Scalar assignment is defined for ``parameter_list`` variables with the
  expected semantics. The lhs parameter list is first deleted, and then
  defined with the same parameters and values as the rhs parameter list,
  becoming an independent copy of the rhs parameter list; but see the
  caution_ below on derived type values.
* The structure constructor ``parameter_list()`` evaluates to an empty
  parameter list, and ``parameter_list`` variables come into existence as
  empty parameter lists.
* ``parameter_list`` objects are properly finalized when they are deallocated
  or otherwise cease to exist.

Type bound subroutines
----------------------

Many of the following subroutines have the optional intent-out arguments
``stat`` and ``errmsg``. If the integer ``stat`` is present, it is assigned
the value 0 if no error occurs; otherwise it is assigned a non-zero value
and the allocatable deferred-length character string ``errmsg``, if present,
is assigned an explanatory message. If ``stat`` is not present and an error
occurs, the error message is written to the preconnected error unit and
program execution is terminated.

``set(name, value [,stat [,errmsg]])``
    Define a parameter with the specified ``name`` and assign it the specified
    ``value``, which may be a scalar, or rank-1 or rank-2 array of any type.
    A copy of the passed value, as created by sourced allocation, is stored
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
    does not exist and ``default`` is not present. It is an error if the
    named parameter is a sublist. It is an error if the type, kind, and rank
    of ``value`` does not match the stored value of the named parameter. Use
    ``get_any`` when the type of the parameter value is not one of those
    handled by this method.

``get_any(name, value [,stat [,errmsg]] [,default])``
    Retrieves the value of the parameter ``name``.  A copy of the value is
    returned in ``value``, which is an allocatable ``class(*)`` variable or
    rank-1 or rank-2 array.  This is a more general version of ``get`` that
    can retrieve any type of parameter value. The drawback of ``get_any`` is
    that application code must use a select-type construct in order to use the
    returned value, making it more cumbersome to use. If present, the optional
    argument ``default`` must have the same rank as ``value``. If the named
    parameter does not exist, it is created with the value prescribed by
    ``default``, and that value is returned in ``value``.  It is an error if
    the named parameter does not exist and ``default`` is not present. It is
    an error if the named parameter is a sublist. It is an error if the rank
    of ``value`` does not match that of the stored value of the named
    parameter.

.. note::
   Arrays returned by ``get`` and ``get_any`` will have the default index
   lower bounds of 1 and not the lower bounds of the array passed to `set`.
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
    returns true if there is a parameter with the given ``name``;
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

  Derived type values with pointer components, direct or indirect, should be
  used advisedly. The values are sourced-allocation copies of the values passed
  to the ``set`` method. This makes a *shallow* copy of any direct or indirect
  pointer component. The original pointer and its copy will have the same
  target; no copy of the target is made. This also applies to parameter list
  assignment, whose values in the lhs are sourced-allocation copies of those
  in the rhs.


The parameter_list_iterator derived type
========================================
Parameter values can be accessed in a parameter list directly, but only if
the parameter names are known. The derived type ``parameter_list_iterator``
provides a means of iterating through the parameters in a ``parameter_list``
object, sequentially visiting each parameter in the list once and only once.
A defined ``parameter_list_iterator`` object is positioned at a particular
parameter of its associated parameter list, or at a pseudo-position *the-end*,
and can be queried for the name and value of that parameter. Scalar assignment
is defined for ``parameter_list_iterator`` objects. The lhs iterator becomes
associated with the same parameter list as the rhs iterator and is positioned
at the same parameter. Subsequent changes to one iterator do not affect the
other. An iterator object is normally defined by assignment from a structure
constructor expression; see below.

Constructor
-----------

``parameter_list_iterator(plist [,sublists_only])``
  Returns an iterator positioned at the initial parameter of the parameter
  list``plist``, or the-end if the parameter list is empty. If the optional
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
  there are no more parameters remaining to be visited. This call has no
  effect if the iterator is already positioned at the-end.

Type bound functions
--------------------

``at_end()``
  Returns true if the iterator is positioned at the-end; otherwise false.

``name()``
  Returns the name of the current parameter. The iterator must not be
  positioned at the-end.

``is_sublist()``
  Returns true if the current parameter value is a sublist; otherwise false.
  The iterator must not be positioned at the-end.

``is_scalar()``
  Returns true if the current parameter has a scalar value; otherwise
  false. The iterator must not be positioned at the-end.

``is_vector()``
  Returns true if the current parameter has a rank-1 array value; otherwise
  false. The iterator must not be positioned at the-end.

``is_matrix()``
  Returns true if the current parameter has a rank-2 array value; otherwise
  false. The iterator must not be positioned at the-end.

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
of any intrinsic or derived type; and ``parameter_list`` itself. This
internal implementation detail can mostly be ignored; all of the procedures
described so far hide this detail, for example. The following procedure is
the exception.

``value()``
  Returns a ``class(parameter_value)`` pointer to an object that holds
  the value of the current parameter. The iterator must not be positioned
  at the-end. A select-type construct with stanzas for each of the four
  possible dynamic types is required to access the value. It is generally
  easier to use the preceding functions instead. For example, with sublists
  it is easier to use the ``is_sublist`` method to identify whether the current
  parameter is a sublist, and if so use the ``sublist`` method to acess the
  sublist.


Parameter list input/output using JSON
======================================

JSON is a widely-used data interchange format (http://www.json.org).
A parameter list whose primitive values are of intrinsic types (integer,
real, character, logical) can be represented quite naturally as JSON text
that conforms to a subset of the JSON format:

* A parameter list is represented by a JSON *object*, which is an unordered
  list of comma-separated *name* : *value* pairs enclosed in braces
  (``{`` and ``}``).

* A parameter name and value are represented by a *name* : *value* pair
  of the object:

  * A *name* is a string enclosed in double quotes.
  * A *value* may be a string (in double quotes), an integer, a real
    number, or a boolean (the tokens ``true`` or ``false``).
  * A *value* may be also be a JSON *array*, which is an ordered list of
    comma-separated *values* enclosed in brackets (``[`` and ``]``). To
    represent an array parameter value, the values in a JSON array are
    restricted to scalars of the same primitive type or such JSON arrays
    themselves. Nesting, however, is limited to 1 level (rank-2 arrays)
    and the sub-arrays must all have the same length. The values are listed
    in Fortran array element order. Simply stated, JSON arrays are limited to
    things that exactly correspond to a rank-1 or 2 Fortran array of intrinsic
    type. JSON generally allows jagged arrays of any JSON values, possibly of
    differing types.
  * A *value* may also be a JSON object that represents a parameter sublist.
  * Null values (the token ``null``) are not allowed.
  * 0-sized arrays are not allowed.

* Comments (text starting from ``//`` to the end of the line) are allowed;
  this is an extention to the JSON standard that is allowed by the YAJL
  library that performs the actual parsing of the JSON text.

The ``parameter_list_json`` module provides the following procedures for
creating a parameter list object from JSON text and for producing a JSON
text representation of a parameter list object.

.. note::

   When reading JSON text, booleans are converted to logical values of default
   kind, integer numbers converted to integer values of default kind, and real
   numbers converted to ``real(real64)`` values. For numbers, this reflects
   the behavior of the YAJL parser. A future enhancement could allow for these
   values to be converted to user-specified kinds before being added to the
   parameter list.

``call parameter_list_from_json_stream(unit, plist, errmsg)``
  Reads JSON text from the given logical ``unit``, which must be connected for
  unformatted stream access, and creates the corresponding parameter list, as
  described above. The intent-out ``type(parameter_list)`` pointer argument
  ``plist`` returns the created parameter list. An unassociated return value
  indicates an error condition, in which case the allocatable deferred-length
  character argument ``errmsg`` is assigned an explanatory error message.

``call parameter_list_from_json_stream(unit, name, plist, errmsg)``
  Does exactly the same thing as the preceding subroutine except that ``plist``
  is assigned the given ``name`` instead of the default "$".  Although the
  name can be reset after the fact, this would not be reflected in the names
  of any sublists created by the stream, whose names are automatically
  generated from the name of their parent parameter lists.

``call parameter_list_from_json_string(string, plist, errmsg)``
  Does exactly the same thing as ``parameter_list_from_json_stream`` except
  that the JSON text is read from the character variable ``string``.

``call parameters_from json_stream(unit, plist, stat, errmsg)``
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
  The parameter list values other than sublists must be of intrinsic primitive
  types that are representable in JSON: logical, integer, real, character.
  The edit descriptor to use for writing real values can be specified by
  ``real_format``; the default is ``"es12.5"``. The output is "pretty" by
  default, using white space, multiple lines, and indentation to express the
  hierarchical structure of the parameter list. However, if ``compact`` is
  specified with value true, the output is a single line without any white
  space (mostly). This may be more useful for piping into downstream utilities.

  .. note::
     JSON is very strict about the syntax of a real number. In particular, the
     decimal point must be preceded by and followed by a digit. For some real
     edit descriptors (depending on compiler) the output will not be strictly
     valid JSON. For example, outputting "1." or ".1" instead of "1.0" or "0.1".
