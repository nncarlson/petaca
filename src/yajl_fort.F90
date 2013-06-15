!!
!! YAJL_FORT -- Fortran bindings to the YAJL library
!!
!! YAJL (Yet Another JSON Library) is a parser for JSON (JavaScript Object
!! Notation) format data streams.  This module provides a Fortran interface
!! to the YAJL library.
!!
!! Neil Carlson <neil.n.carlson@gmail.com> 4 July 2011
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2011, 2013, Neil N. Carlson
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
!! IMPLEMENTATION NOTES
!!
!!  The C yajl parser operates by calling application-defined callback
!!  functions in response to the various events encountered while parsing
!!  the input stream.  The callback functions communicate with each other
!!  through a common, application-defined, context data struct, and a void
!!  pointer to that data struct is passed to each of the callbacks.
!!  In the Fortran interface, this application-defined code/data is naturally
!!  implemented with an abstract derived type with deferred type-bound
!!  procedures that are the required callback functions.  Application code
!!  extends this type, adding the desired context data components and
!!  providing concrete implementations for the callback functions.
!!
!!  Several related problems are encountered when attempting to interface
!!  this implementation to the C yajl parser.  Creating the parser requires
!!  the yajl_callbacks struct, whose fields are function pointers to the
!!  callbacks, and a void pointer to the context data, which in this case
!!  would be a CLASS(FYAJL_CALLBACKS) variable.  First, the C_FUNLOC
!!  intrinsic cannot be applied to type-bound procedures, so the Fortran
!!  callbacks cannot be used directly by the parser.  Second, the C_LOC
!!  intrinsic cannot be applied to a polymorphic variable, so the Fortran
!!  context cannot be directly passed to the parser.
!!
!!  These obstacles are finessed in the following way.  Fixed module procedures
!!  defined here are passed to the parser as the callbacks.  These are thin
!!  wrapper functions that delegate to the actual application-defined callbacks.
!!  In addition, the CLASS(FYAJL_CALLBACKS) variable is boxed in a simple type,
!!  and the address of this variable is passed instead as the context.  The
!!  callbacks can then unbox this received context to recover the actual
!!  polymorphic context (through which the actual callbacks are accessed too).
!!  One function of the fixed callback wrappers is to perform this unboxing.
!!

#include "f90_assert.fpp"

module yajl_fort

  use,intrinsic :: iso_c_binding
  implicit none
  private

  !! Application code should extend this type with context data components
  !! and concrete implementations of the callback functions.
  type, abstract, public :: fyajl_callbacks
  contains
    procedure(cb_no_args), deferred :: start_map
    procedure(cb_no_args), deferred :: end_map
    procedure(cb_string),  deferred :: map_key
    procedure(cb_no_args), deferred :: null_value
    procedure(cb_logical), deferred :: logical_value
    procedure(cb_integer), deferred :: integer_value
    procedure(cb_double),  deferred :: double_value
    procedure(cb_string),  deferred :: string_value
    procedure(cb_no_args), deferred :: start_array
    procedure(cb_no_args), deferred :: end_array
  end type fyajl_callbacks

  !! Callback return values.
  integer, parameter, public :: FYAJL_TERMINATE_PARSING = 0
  integer, parameter, public :: FYAJL_CONTINUE_PARSING  = 1

  !! Integer and real kinds of the data values passed to the callbacks.
  integer, parameter, public :: FYAJL_INTEGER_KIND = c_long_long
  integer, parameter, public :: FYAJL_REAL_KIND    = c_double

  !! The required interfaces for the callback functions.  All callbacks return
  !! an integer status result (above): FYAJL_TERMINATE_PARSING when an error
  !! occurs that requires the parser to terminate, or FYAJL_CONTINUE_PARSING
  !! to indicate that parsing should continue normally.
  abstract interface
    integer function cb_no_args (this)
      import fyajl_callbacks
      class(fyajl_callbacks) :: this
    end function
    integer function cb_integer (this, value)
      import fyajl_callbacks, c_long_long
      class(fyajl_callbacks) :: this
      integer(c_long_long), intent(in) :: value
    end function
    integer function cb_double (this, value)
      import fyajl_callbacks, c_double
      class(fyajl_callbacks) :: this
      real(c_double), intent(in) :: value
    end function
    integer function cb_logical (this, value)
      import fyajl_callbacks
      class(fyajl_callbacks) :: this
      logical, intent(in) :: value
    end function
    integer function cb_string (this, value)
      import fyajl_callbacks, c_char
      class(fyajl_callbacks) :: this
      character(*,kind=c_char), intent(in) :: value
    end function
  end interface

  !! Interoperable type with the yajl_callbacks struct (yajl_parse.h)
  !! This will be populated with pointers to the fyajl_* module functions.
  type, bind(c) :: yajl_callbacks
    type(c_funptr) :: yajl_null = C_NULL_FUNPTR
    type(c_funptr) :: yajl_boolean = C_NULL_FUNPTR
    type(c_funptr) :: yajl_integer = C_NULL_FUNPTR
    type(c_funptr) :: yajl_double = C_NULL_FUNPTR
    type(c_funptr) :: yajl_number = C_NULL_FUNPTR
    type(c_funptr) :: yajl_string = C_NULL_FUNPTR
    type(c_funptr) :: yajl_start_map = C_NULL_FUNPTR
    type(c_funptr) :: yajl_map_key = C_NULL_FUNPTR
    type(c_funptr) :: yajl_end_map = C_NULL_FUNPTR
    type(c_funptr) :: yajl_start_array = C_NULL_FUNPTR
    type(c_funptr) :: yajl_end_array = C_NULL_FUNPTR
  end type yajl_callbacks

  !! Box around a polymorphic callbacks variable.  C_LOC can be applied to
  !! variables of this type, but not the polymorphic variable it contains.
  type :: callbacks_box
    class(fyajl_callbacks), pointer :: callbacks => null()
  end type

  !! The Fortran yajl parser.  This holds the handle to the C yajl parser
  !! and provides bindings to the C parser functions as type-bound procedures.
  !! Note that the callback structure and callback context that are passed
  !! to the initial parser allocation function (yajl_alloc) are held as data
  !! components because they need to persist for the lifetime of the parser;
  !! they are unused otherwise.
  type, public :: fyajl_parser
    type(c_ptr) :: handle = C_NULL_PTR  ! handle to the yajl parser
    type(yajl_callbacks) :: callbacks   ! the callbacks struct passed to yajl_alloc
    type(callbacks_box) :: ctx          ! the context passed to yajl_alloc
  contains
    procedure :: init
    procedure :: parse
    procedure :: complete_parse
    procedure :: set_option
    procedure :: unset_option
    procedure :: bytes_consumed
    final :: fyajl_parser_dealloc
  end type

  !! Parser yajl_option enum values from yajl_parse.h.
  enum, bind(c)
    enumerator :: FYAJL_ALLOW_COMMENTS = 1
    enumerator :: FYAJL_DONT_VALIDATE_STRINGS = 2
    enumerator :: FYAJL_ALLOW_TRAILING_GARBAGE = 4
    enumerator :: FYAJL_ALLOW_MULTIPLE_DOCUMENTS = 8  ! ..._values in yajl_parse.h
    enumerator :: FYAJL_ALLOW_PARTIAL_DOCUMENT = 16   ! ..._values in yajl_parse.h
  end enum
  
  !! Fortran parser options.  Passed to the set and unset methods.
  public :: FYAJL_ALLOW_COMMENTS, FYAJL_DONT_VALIDATE_STRINGS, FYAJL_ALLOW_TRAILING_GARBAGE, &
            FYAJL_ALLOW_MULTIPLE_DOCUMENTS, FYAJL_ALLOW_PARTIAL_DOCUMENT

  !! Parser yajl_status enum values from yajl_parse.h.
  enum, bind(c)
    enumerator :: yajl_status_ok, yajl_status_client_canceled, yajl_status_error
  end enum

  !! Fortran parser return codes.  Wrapping the YAJL status value in a opaque
  !! derived type solves the potential integer type mismatch problem between C
  !! and Fortran.  The alternative is to require application code to use C_INT
  !! kind integers for status values.  The derived type approach seems slightly
  !! cleaner.  The comparison operators == and /= are defined to allow testing
  !! of returned status values.
  type, public :: fyajl_status
    private
    integer(c_int) :: value
  contains
    procedure, private :: fyajl_status_ne
    generic :: operator(/=) => fyajl_status_ne
    procedure, private :: fyajl_status_eq
    generic :: operator(==) => fyajl_status_eq
  end type fyajl_status
  type(fyajl_status), parameter, public :: &
      FYAJL_STATUS_OK              = fyajl_status(yajl_status_ok), &
      FYAJL_STATUS_CLIENT_CANCELED = fyajl_status(yajl_status_client_canceled), &
      FYAJL_STATUS_ERROR           = fyajl_status(yajl_status_error)

  !! Additional module functions (not type bound)
  public :: fyajl_get_error, fyajl_status_to_string

  !! INTEROPERABLE INTERFACES TO YAJL 2.0 LIBRARY FUNCTIONS
  interface
    function yajl_alloc (callbacks, afs, ctx) result(handle) bind(c)
      use,intrinsic :: iso_c_binding
      import yajl_callbacks
      type(yajl_callbacks) :: callbacks
      type(c_ptr), value :: afs, ctx
      type(c_ptr) :: handle
      end function
    subroutine yajl_free(handle) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: handle
      end subroutine
    function yajl_config (handle, option, enable) result(stat) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: handle
      integer(c_int), value :: option, enable
      integer(c_int) :: stat
      end function
    function yajl_parse (handle, buffer, length) result(stat) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: handle
      character(kind=c_char), intent(in) :: buffer(*)
      integer(c_size_t), value :: length
      integer(c_int) :: stat
      end function
    function yajl_complete_parse(handle) result(stat) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: handle
      integer(c_int) :: stat
      end function
    function yajl_status_to_string(code) result(string) bind(c)
      use,intrinsic :: iso_c_binding
      integer(c_int), value :: code
      type(c_ptr) :: string
      end function
    function yajl_get_bytes_consumed(handle) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: handle
      integer(c_size_t) :: yajl_get_bytes_consumed
      end function
    function yajl_get_error(handle, verbose, jsonText, jsonTextLength) result(string) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: handle
      integer(c_int), value :: verbose
      character(kind=c_char), intent(in) :: jsonText(*)
      integer(c_size_t), value :: jsonTextLength
      type(c_ptr) :: string
      end function
    subroutine yajl_free_error(handle, str) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: handle
      type(c_ptr), value :: str
      end subroutine
  end interface

  !! The Fortran yajl emitter.  This holds the handle to the C yajl generator
  !! and provides bindings to the C parser functions as type-bound procedures.
  !! Note that the callback structure and callback context that are passed
  !! to the initial parser allocation function (yajl_alloc) are held as data
  !! components because they need to persist for the lifetime of the parser;
  !! they are unused otherwise.
  type, public :: fyajl_emitter
    type(c_ptr) :: handle = C_NULL_PTR  ! handle to the yajl generator
  contains
    procedure :: init => emitter_init
    procedure, private :: emit_integer
    procedure, private :: emit_double
    procedure, private :: emit_logical
    procedure, private :: emit_string
    generic :: emit_value => emit_integer, emit_double, emit_logical, emit_string
    procedure :: emit_map_open
    procedure :: emit_map_close
    procedure :: emit_array_open
    procedure :: emit_array_close
    procedure :: emit_map_key => emit_string
    procedure :: get_buffer
    procedure :: clear_buffer
    final :: fyajl_emitter_dealloc
  end type

  !! Generator status enum values (yajl_gen_status) from yajl_gen.h
  enum, bind(c)
    enumerator :: yajl_gen_status_ok = 0
    enumerator :: yajl_gen_keys_must_be_strings
    enumerator :: yajl_max_depth_exceeded
    enumerator :: yajl_gen_in_error_state
    enumerator :: yajl_gen_generation_complete
    enumerator :: yajl_gen_invalid_number
    enumerator :: yajl_gen_no_buf
    enumerator :: yajl_gen_invalid_string
  end enum

  !! Fortran generator return codes.  Could just have used the yajl_gen_*
  !! values directly, but decided to wrap them in an opaque type and provide
  !! specific parameter values of this type that can be used in == and /=
  !! comparisons.
  type, public :: fyajl_gen_status
    private
    integer(c_int) :: value
  contains
    procedure, private :: fyajl_gen_status_ne
    generic :: operator(/=) => fyajl_gen_status_ne
    procedure, private :: fyajl_gen_status_eq
    generic :: operator(==) => fyajl_gen_status_eq
  end type fyajl_gen_status
  type(fyajl_gen_status), parameter, public :: &
      FYAJL_GEN_STATUS_OK            = fyajl_gen_status(yajl_gen_status_ok), &
      FYAJL_GEN_KEYS_MUST_BE_STRINGS = fyajl_gen_status(yajl_gen_keys_must_be_strings), &
      FYAJL_MAX_DEPTH_EXCEEDED       = fyajl_gen_status(yajl_max_depth_exceeded), &
      FYAJL_GEN_IN_ERROR_STATE       = fyajl_gen_status(yajl_gen_in_error_state), &
      FYAJL_GEN_GENERATION_COMPLETE  = fyajl_gen_status(yajl_gen_generation_complete), &
      FYAJL_GEN_INVALID_NUMBER       = fyajl_gen_status(yajl_gen_invalid_number), &
      FYAJL_GEN_NO_BUF               = fyajl_gen_status(yajl_gen_no_buf), &
      FYAJL_GEN_INVALID_STRING       = fyajl_gen_status(yajl_gen_invalid_string)

  interface
    function yajl_gen_alloc(allocFuncs) result(handle) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: allocFuncs
      type(c_ptr) :: handle
      end function
    subroutine yajl_gen_free(handle) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: handle
      end subroutine
    !function yajl_gen_config(g, opt, ...) result(stat) bind(c)
    !  use,intrinsic :: iso_c_binding
    !  type(c_ptr), value :: g
    !  integer(c_int) :: stat
    !  end function
    function yajl_gen_integer(hand, number) result(stat) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: hand
      integer(c_long_long), value :: number
      integer(c_int) :: stat
      end function
    function yajl_gen_double(hand, number) result(stat) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: hand
      real(c_double), value :: number
      integer(c_int) :: stat
      end function
    function yajl_gen_number(hand, num, len) result(stat) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: hand
      character(kind=c_char), intent(in) :: num(*)
      integer(c_size_t), value :: len
      integer(c_int) :: stat
      end function
    function yajl_gen_string(hand, str, len) result(stat) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: hand
      character(kind=c_char), intent(in) :: str(*)
      integer(c_size_t), value :: len
      integer(c_int) :: stat
      end function
    function yajl_gen_null(hand) result(stat) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: hand
      integer(c_int) :: stat
      end function
    function yajl_gen_bool(hand, boolean) result(stat) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: hand
      integer(c_int), value :: boolean
      integer(c_int) :: stat
      end function
    function yajl_gen_map_open(hand) result(stat) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: hand
      integer(c_int) :: stat
      end function
    function yajl_gen_map_close(hand) result(stat) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: hand
      integer(c_int) :: stat
      end function
    function yajl_gen_array_open(hand) result(stat) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: hand
      integer(c_int) :: stat
      end function
    function yajl_gen_array_close(hand) result(stat) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: hand
      integer(c_int) :: stat
      end function
    function yajl_gen_get_buf(hand, buf, len) result(stat) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: hand
      type(c_ptr), intent(out) :: buf
      integer(c_size_t), intent(out) :: len
      integer(c_int) :: stat
      end function
    subroutine yajl_gen_clear(hand) bind(c)
      use,intrinsic :: iso_c_binding
      type(c_ptr), value :: hand
      end subroutine
  end interface

contains

  subroutine init (this, callbacks)
    class(fyajl_parser), intent(out), target :: this
    class(fyajl_callbacks), intent(in), target :: callbacks
    !! Define the YAJL C callback structure
    this%callbacks%yajl_null = c_funloc(fyajl_null)
    this%callbacks%yajl_boolean = c_funloc(fyajl_boolean)
    this%callbacks%yajl_integer = c_funloc(fyajl_integer)
    this%callbacks%yajl_double = c_funloc(fyajl_double)
    this%callbacks%yajl_number = C_NULL_FUNPTR  ! we don't want this
    this%callbacks%yajl_string = c_funloc(fyajl_string)
    this%callbacks%yajl_start_map = c_funloc(fyajl_start_map)
    this%callbacks%yajl_map_key = c_funloc(fyajl_map_key)
    this%callbacks%yajl_end_map = c_funloc(fyajl_end_map)
    this%callbacks%yajl_start_array = c_funloc(fyajl_start_array)
    this%callbacks%yajl_end_array = c_funloc(fyajl_end_array)
    !! Define the context that gets passed to the yajl callback functions.
    this%ctx%callbacks => callbacks
    !! Create the yajl parser object.
    this%handle = yajl_alloc(this%callbacks, C_NULL_PTR, c_loc(this%ctx))
  end subroutine

  subroutine set_option (this, option)
    class(fyajl_parser), intent(in) :: this
    integer(c_int), intent(in) :: option
    integer :: stat
    stat = yajl_config(this%handle, option, 1) ! ignore return code
  end subroutine

  subroutine unset_option (this, option)
    class(fyajl_parser), intent(in) :: this
    integer(c_int), intent(in) :: option
    integer :: stat
    stat = yajl_config(this%handle, option, 0) ! ignore return code
  end subroutine

  subroutine parse (this, buffer, stat)
    class(fyajl_parser), intent(inout) :: this
    character(kind=c_char), intent(in) :: buffer(:)
    type(fyajl_status), intent(out) :: stat
    stat%value = yajl_parse(this%handle, buffer, int(size(buffer),kind=c_size_t))
  end subroutine

  subroutine complete_parse (this, stat)
    class(fyajl_parser), intent(in) :: this
    type(fyajl_status), intent(out) :: stat
    stat%value = yajl_complete_parse(this%handle)
  end subroutine complete_parse

  integer function bytes_consumed (this)
    class(fyajl_parser), intent(in) :: this
    bytes_consumed = yajl_get_bytes_consumed(this%handle)
  end function bytes_consumed

  subroutine fyajl_parser_dealloc (this)
    type(fyajl_parser) :: this
    if (c_associated(this%handle)) call yajl_free (this%handle)
    this%handle = C_NULL_PTR
  end subroutine fyajl_parser_dealloc

  !! Returns the descriptive string for an error code.
  function fyajl_status_to_string(code) result(string)
    type(fyajl_status), intent(in) :: code
    character(len=:), allocatable :: string
    string = f_string_pointer (yajl_status_to_string(code%value))
  end function fyajl_status_to_string

  !! Returns an error string describing the state of the parser.
  function fyajl_get_error (this, verbose, buffer) result(string)
    class(fyajl_parser), intent(in) :: this
    logical, intent(in) :: verbose
    character(kind=c_char), intent(in) :: buffer(:)
    character(:,kind=c_char), allocatable :: string
    type(c_ptr) :: cptr
    if (verbose) then
      cptr = yajl_get_error(this%handle, 1_c_int, buffer, int(size(buffer),kind=c_size_t))
    else
      cptr = yajl_get_error(this%handle, 0_c_int, buffer, int(size(buffer),kind=c_size_t))
    end if
    string = f_string_pointer(cptr)
    call yajl_free_error (this%handle, cptr)
  end function fyajl_get_error

  !! Auxillary function that converts a C string pointer to a Fortran character pointer.
  function f_string_pointer (cptr) result(fptr)
    type(c_ptr), intent(in) :: cptr
    character(:,kind=c_char), pointer :: fptr
    interface ! to strlen from the standard C library
      function strlen(s) result(len) bind(c)
        use,intrinsic :: iso_c_binding
        type(c_ptr), value :: s
        integer(c_size_t) :: len
      end function
    end interface
    fptr => f_string_pointer_aux (cptr, strlen(cptr))
    !! An alternative that avoids using strlen from the standard C library.
    !character(kind=c_char), pointer :: s(:)
    !integer :: n
    !call c_f_pointer (cptr, s, shape=[huge(1)])
    !n = 0
    !do while (s(n+1) /= c_null_char)  ! string better be null terminated!
    !  n = n + 1
    !end do
    !fptr => f_string_pointer_aux (cptr, n)
  contains
    function f_string_pointer_aux(cptr, len) result(fptr)
      type(c_ptr), intent(in) :: cptr
      integer(c_size_t), intent(in) :: len
      character(len,kind=c_char), pointer :: fptr
      call c_f_pointer (cptr, fptr)
    end function
  end function f_string_pointer

  !! FYAJL_STATUS type-bound procedures.

  logical function fyajl_status_ne (s1, s2)
    class(fyajl_status), intent(in) :: s1, s2
    fyajl_status_ne = (s1%value /= s2%value)
  end function fyajl_status_ne

  logical function fyajl_status_eq (s1, s2)
    class(fyajl_status), intent(in) :: s1, s2
    fyajl_status_eq = (s1%value == s2%value)
  end function fyajl_status_eq

  !! FYAJL_GEN_STATUS type-bound procedures.

  logical function fyajl_gen_status_ne (s1, s2)
    class(fyajl_gen_status), intent(in) :: s1, s2
    fyajl_gen_status_ne = (s1%value /= s2%value)
  end function fyajl_gen_status_ne

  logical function fyajl_gen_status_eq (s1, s2)
    class(fyajl_gen_status), intent(in) :: s1, s2
    fyajl_gen_status_eq = (s1%value == s2%value)
  end function fyajl_gen_status_eq

  !! The YAJL callback functions that populate the yajl_callbacks struct.
  !! Their interfaces are interoperable with the prototypes in yajl_parse.h.
  !! These need to be module functions in order to get their C addresses with
  !! the C_FUNLOC intrinsic (i.e., they can't be type-bound procedures).
  !! Note that these delegate to the appropriate application-supplied callback
  !! functions with only a bit of massaging of the input in the case of strings
  !! and boolean values.  Note that these are given empty binding names so that
  !! they are not directly exposed externally.

  integer(c_int) function fyajl_start_map(ctx) bind(c,name='')
    type(c_ptr), value :: ctx
    type(callbacks_box), pointer :: box
    call c_f_pointer (ctx, box)
    fyajl_start_map = box%callbacks%start_map()
  end function fyajl_start_map

  integer(c_int) function fyajl_end_map(ctx) bind(c,name='')
    type(c_ptr), value :: ctx
    type(callbacks_box), pointer :: box
    call c_f_pointer (ctx, box)
    fyajl_end_map = box%callbacks%end_map()
  end function fyajl_end_map

  integer(c_int) function fyajl_map_key(ctx, stringVal, stringLen) bind(c,name='')
    type(c_ptr), value :: ctx
    type(c_ptr), value :: stringVal
    integer(c_size_t), value :: stringLen
    character(len=stringLen,kind=c_char), pointer :: fstring
    type(callbacks_box), pointer :: box
    call c_f_pointer (stringVal, fstring)
    call c_f_pointer (ctx, box)
    fyajl_map_key = box%callbacks%map_key(fstring)
  end function fyajl_map_key

  integer(c_int) function fyajl_null(ctx) bind(c,name='')
    type(c_ptr), value :: ctx
    type(callbacks_box), pointer :: box
    call c_f_pointer (ctx, box)
    fyajl_null = box%callbacks%null_value()
  end function fyajl_null

  integer(c_int) function fyajl_boolean(ctx, boolVal) bind(c,name='')
    type(c_ptr), value :: ctx
    integer(c_int), value :: boolVal
    type(callbacks_box), pointer :: box
    call c_f_pointer (ctx, box)
    fyajl_boolean = box%callbacks%logical_value(boolVal/=0)
  end function fyajl_boolean

  integer(c_int) function fyajl_integer(ctx, integerVal) bind(c,name='')
    type(c_ptr), value :: ctx
    integer(c_long_long), value :: integerVal
    type(callbacks_box), pointer :: box
    call c_f_pointer (ctx, box)
    fyajl_integer = box%callbacks%integer_value(integerVal)
  end function fyajl_integer

  integer(c_int) function fyajl_double(ctx, doubleVal) bind(c,name='')
    type(c_ptr), value :: ctx
    real(c_double), value :: doubleVal
    type(callbacks_box), pointer :: box
    call c_f_pointer (ctx, box)
    fyajl_double = box%callbacks%double_value(doubleVal)
  end function fyajl_double

  integer(c_int) function fyajl_string(ctx, stringVal, stringLen) bind(c,name='')
    type(c_ptr), value :: ctx
    type(c_ptr), value :: stringVal
    integer(c_size_t), value :: stringLen
    character(len=stringLen,kind=c_char), pointer :: fstring
    type(callbacks_box), pointer :: box
    call c_f_pointer (stringVal, fstring)
    call c_f_pointer (ctx, box)
    fyajl_string = box%callbacks%string_value(fstring)
  end function fyajl_string

  integer(c_int) function fyajl_start_array(ctx) bind(c,name='')
    type(c_ptr), value :: ctx
    type(callbacks_box), pointer :: box
    call c_f_pointer (ctx, box)
    fyajl_start_array = box%callbacks%start_array()
  end function fyajl_start_array

  integer(c_int) function fyajl_end_array(ctx) bind(c,name='')
    type(c_ptr), value :: ctx
    type(callbacks_box), pointer :: box
    call c_f_pointer (ctx, box)
    fyajl_end_array = box%callbacks%end_array()
  end function fyajl_end_array

  subroutine emitter_init (this)
    class(fyajl_emitter), intent(out), target :: this
    !! Create the yajl parser object.
    this%handle = yajl_gen_alloc(C_NULL_PTR)
  end subroutine emitter_init

  subroutine fyajl_emitter_dealloc (this)
    type(fyajl_emitter) :: this
    if (c_associated(this%handle)) call yajl_gen_free (this%handle)
    this%handle = C_NULL_PTR
  end subroutine fyajl_emitter_dealloc

  subroutine emit_array_open (this)
    class(fyajl_emitter), intent(in) :: this
    type(fyajl_gen_status) :: stat
    stat%value = yajl_gen_array_open(this%handle)
    INSIST(stat == FYAJL_GEN_STATUS_OK)
  end subroutine emit_array_open

  subroutine emit_array_close (this)
    class(fyajl_emitter), intent(in) :: this
    type(fyajl_gen_status) :: stat
    stat%value = yajl_gen_array_close(this%handle)
    INSIST(stat == FYAJL_GEN_STATUS_OK)
  end subroutine emit_array_close

  subroutine emit_map_open (this)
    class(fyajl_emitter), intent(in) :: this
    type(fyajl_gen_status) :: stat
    stat%value = yajl_gen_map_open(this%handle)
    INSIST(stat == FYAJL_GEN_STATUS_OK)
  end subroutine emit_map_open

  subroutine emit_map_close (this)
    class(fyajl_emitter), intent(in) :: this
    type(fyajl_gen_status) :: stat
    stat%value = yajl_gen_map_close(this%handle)
    INSIST(stat == FYAJL_GEN_STATUS_OK)
  end subroutine emit_map_close

  subroutine emit_integer (this, value)
    class(fyajl_emitter), intent(in) :: this
    integer(c_long_long), intent(in) :: value
    type(fyajl_gen_status) :: stat
    stat%value = yajl_gen_integer(this%handle, value)
    INSIST(stat == FYAJL_GEN_STATUS_OK)
  end subroutine emit_integer

  subroutine emit_double (this, value)
    class(fyajl_emitter), intent(in) :: this
    real(c_double), intent(in) :: value
    type(fyajl_gen_status) :: stat
    stat%value = yajl_gen_double(this%handle, value)
    INSIST(stat == FYAJL_GEN_STATUS_OK)
  end subroutine emit_double

  subroutine emit_logical (this, value)
    class(fyajl_emitter), intent(in) :: this
    logical, intent(in) :: value
    type(fyajl_gen_status) :: stat
    integer(c_int) :: cbool
    cbool = 0
    if (value) cbool = 1
    stat%value = yajl_gen_bool(this%handle, cbool)
    INSIST(stat == FYAJL_GEN_STATUS_OK)
  end subroutine emit_logical

  subroutine emit_string (this, value)
    class(fyajl_emitter), intent(in) :: this
    character(*,kind=c_char), intent(in) :: value
    type(fyajl_gen_status) :: stat
    stat%value = yajl_gen_string(this%handle, value, int(len_trim(value),kind=c_size_t))
    INSIST(stat == FYAJL_GEN_STATUS_OK)
  end subroutine emit_string

  subroutine get_buffer (this, buffer)
    class(fyajl_emitter), intent(in) :: this
    character(:,kind=c_char), pointer, intent(out) :: buffer
    type(fyajl_gen_status) :: stat
    type(c_ptr) :: cptr
    integer(c_size_t) :: buflen
    stat%value = yajl_gen_get_buf(this%handle, cptr, buflen)
    INSIST(stat == FYAJL_GEN_STATUS_OK)
    buffer => f_string_pointer(cptr, buflen)
  contains
    function f_string_pointer (cptr, len) result(fptr)
      type(c_ptr), intent(in) :: cptr
      integer(c_size_t), intent(in) :: len
      character(len,kind=c_char), pointer :: fptr
      call c_f_pointer (cptr, fptr)
    end function
  end subroutine get_buffer

  subroutine clear_buffer (this)
    class(fyajl_emitter), intent(in) :: this
    call yajl_gen_clear (this%handle)
  end subroutine clear_buffer

end module yajl_fort
