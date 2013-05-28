!!
!! FORTRAN_DYNAMIC_LOADER
!!
!! Neil Carlson <neil.n.carlson@gmail.com>
!! 10 Apr 2006; last revised 29 Mar 2013.
!!
!! This module provides access to the dynamic loader.  It uses the
!! C-interoperability features of Fortran 2003 to directly interface with
!! the POSIX C functions dlopen, dlclose, dlsym, and dlerror from the system
!! DLL library (libdl on linux).
!!
!! PROGRAMMING INTERFACE
!!
!! Each subroutine has the optional intent-out arguments STAT and ERRMSG.
!! If STAT is present, it is assigned the value 0 if the subroutine completes
!! successfully, and a nonzero value if an error occurs.  In the latter case,
!! the allocatable character string ERRMSG, if present, is assigned the error
!! string returned by the underlying DL library.  If STAT is not present and
!! an error occurs, the error string is written to the preconnected error unit
!! and the program exits with a nonzero status.
!!
!! The DLL library must included when linking the executable (-ldl on linux)
!! to resolve the symbols dlopen, dlclose, dlsym and dlerror.
!!
!!  CALL DLL_OPEN (PATH, MODE, HANDLE [,STAT [,ERRMSG]])
!!    CHARACTER(*), INTENT(IN) :: PATH
!!    INTEGER(C_INT), INTENT(IN) :: MODE
!!    TYPE(DL_HANDLE) :: HANDLE
!!    INTEGER, INTENT(OUT), OPTIONAL :: STAT
!!    CHARACTER(:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: ERRMSG
!!
!!    This call opens the shared library file specified by PATH and returns
!!    a handle for the library.  This handle is intended only to be passed
!!    back to DLL_FUNC and DLL_CLOSE.  This can be called multiple times
!!    for the same library; the same handle is returned.  The value supplied
!!    for MODE must be one of the module parameters RTLD_LAZY or RTLD_NOW,
!!    optionally or'ed with the module parameter RTLD_GLOBAL.  See the man
!!    page for dlopen(3) for a detailed description of the behavior.
!!
!!  CALL DLL_CLOSE (HANDLE [,STAT [,ERRMSG]])
!!    TYPE(DL_HANDLE) :: HANDLE
!!    INTEGER, INTENT(OUT), OPTIONAL :: STAT
!!    CHARACTER(:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: ERRMSG
!!
!!    This call decrements the reference count on the specified shared library
!!    handle.  When the reference count reaches zero, the shared library is
!!    unloaded.  See the man page for dlclose(3) for more details.
!!
!!  CALL DLL_FUNC (HANDLE, SYMBOL, FUNPTR [,STAT [,ERRMSG]])
!!    TYPE(DL_HANDLE) :: HANDLE
!!    CHARACTER(*), INTENT(IN) :: SYMBOL
!!    TYPE(C_FUNPTR), INTENT(OUT) :: FUNPTR
!!    INTEGER, INTENT(OUT), OPTIONAL :: STAT
!!    CHARACTER(:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: ERRMSG
!!
!!    Given the handle of a shared library returned by DLL_OPEN and the name of
!!    a function symbol in that library, this routine returns the address of
!!    the function in FUNPTR.  The caller is responsible for converting this
!!    C_FUNPTR type value to the appropriate Fortran procedure pointer using
!!    the intrinsic module procedure C_F_PROCPOINTER from ISO_C_BINDING.
!!
!!  CALL DLL_SYM (HANDLE, SYMBOL, SYMPTR [,STAT [,ERRMSG]])
!!    TYPE(DL_HANDLE) :: HANDLE
!!    CHARACTER(*), INTENT(IN) :: SYMBOL
!!    TYPE(C_PTR), INTENT(OUT) :: SYMPTR
!!    INTEGER, INTENT(OUT), OPTIONAL :: STAT
!!    CHARACTER(:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: ERRMSG
!!
!!    Given the handle of a shared library returned by DLL_OPEN and the name of
!!    a (data) symbol in that library, this routine returns the address of
!!    the symbol in SYMPTR.  The caller is responsible for converting this
!!    C_PTR type value to the appropriate Fortran data pointer using
!!    the intrinsic module procedure C_F_POINTER from ISO_C_BINDING.
!!
!! An alternative object-oriented interface is also provided.  The derived type
!! SHLIB has type-bound procedures OPEN, CLOSE, FUNC, and SYM that have the same
!! interfaces as DLL_OPEN, DLL_CLOSE, DLL_FUNC, and DLL_SYM but with the HANDLE
!! argument omitted.  An object of this type is effectively the handle.
!!
!! EXAMPLE
!!
!!  Load the C math library libm.so and print the cube root of 8.0 using
!!  the function cbrtf from the library:
!!
!!    program example
!!
!!      use dynamic_linking_loader
!!      use,intrinsic :: iso_c_binding, only: c_funptr, c_f_procpointer
!!      implicit none
!!
!!      type(dl_handle) :: so
!!      type(c_funptr)  :: funptr
!!      procedure(f), pointer :: fptr
!!
!!      abstract interface
!!        real function f(x)
!!          real, value :: x
!!        end function
!!      end interface
!!
!!      call dll_open ('libm.so', RTLD_NOW, so)
!!      call dll_func (so, 'cbrtf', funptr)
!!      call c_f_procpointer (funptr, fptr)
!!      print *, 'the cube root of 8 is', fptr(8.0)
!!      call dll_close (so)
!!
!!    end program
!!
!!  The same program using the object-oriented interface is nearly the same:
!!
!!    program example
!!
!!      use dynamic_linking_loader
!!      use,intrinsic :: iso_c_binding, only: c_funptr, c_f_procpointer
!!      implicit none
!!
!!      type(shlib) :: libm
!!      type(c_funptr)  :: funptr
!!      procedure(f), pointer :: fptr
!!
!!      abstract interface
!!        real function f(x)
!!          real, value :: x
!!        end function
!!      end interface
!!
!!      call libm%open ('libm.so', RTLD_NOW)
!!      call libm%func ('cbrtf', funptr)
!!      call c_f_procpointer (funptr, fptr)
!!      print *, 'the cube root of 8 is', fptr(8.0)
!!      call libm%close
!!
!!    end program
!!
!! IMPLEMENTATION NOTES
!!
!! There is the well-known dilemma that dlsym returns a pointer to a data
!! object (void *) regardless of whether the symbol is a function or a data
!! object, but that casts between pointer to data and pointer to function are
!! disallowed in C; there is no guarantee that the two C pointer types have
!! the same size.  In practice, compilers allow the casting as an extension,
!! or if not, there is another method that effectively does the casting.
!! The assumption is that a data pointer is wide enough to hold a function
!! pointer.
!!
!! Here we lie about the return value of dlsym and say that it is a C
!! function pointer so that we can translate it into a Fortran procedure
!! pointer.  The expectation is that what users want is to link to functions
!! from a shared object and not global data.
!!
!! It would be desirable to also provide a DLL_SYM procedure that returned
!! a C data pointer, in addition to the DLL_FUNC.  Attempting to define a
!! different interface block, say for dlfunc, that binds to the same external
!! name dlsym though doesn't work.  The compiler sees that there are two
!! different and incompatible interfaces to the same external binding symbol.
!! Thus this will have to wait until a time when the POSIX DLL specification
!! introduces a separate dlfunc (like in the BSD implementation) that returns
!! a function pointer.
!!
!! The object-oriented interface was introduced as an experiment.  It works
!! quite well for the capabilities currently exposed, however the Linux dll
!! implementation has additional capabilities beyond POSIX, and some of these
!! involve inspection of the library handle.  The object-oriented interface
!! doesn't look too well suited for that.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Pick one of two alternatives here: define a secondary binding to dlsym
! that lies about the return type (FAKE_DLFUNC), otherwise use the TRANSFER
! intrinsic to cast a C_PTR to a C_FUNPTR.  Hopefully one will work for you.
#ifdef __INTEL_COMPILER
#  define FAKE_DLFUNC
#elif defined(FAKE_DLFUNC)
#  undef FAKE_DLFUNC
#endif

module fortran_dynamic_loader

  use,intrinsic :: iso_c_binding
  implicit none
  private

  type, public :: shlib
    private
    type(c_ptr) :: handle = C_NULL_PTR
  contains
    procedure :: open  => shlib_open
    procedure :: close => shlib_close
    procedure :: sym   => shlib_sym
    procedure :: func  => shlib_func
  end type shlib

  !! Linking mode flags; values from bits/dlfcn.h on Linux.
  integer(c_int), parameter, public :: RTLD_LAZY   = 1
  integer(c_int), parameter, public :: RTLD_NOW    = 2
  integer(c_int), parameter, public :: RTLD_LOCAL  = 0
  integer(c_int), parameter, public :: RTLD_GLOBAL = 256

  !! Interfaces for the POSIX DLL library functions.
  interface
    function dlopen(filename, flag) result(handle) bind(c)
      use,intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr
      character(kind=c_char), intent(in) :: filename(*)
      integer(c_int), value  :: flag
      type(c_ptr) :: handle
      end function
    function dlsym(handle, symbol) result(addr) bind(c)
      use,intrinsic :: iso_c_binding, only: c_char, c_ptr, c_ptr
      type(c_ptr), value :: handle
      character(kind=c_char), intent(in) :: symbol(*)
      type(c_ptr) :: addr
      end function
    function dlclose (handle) result(status) bind(c)
      use,intrinsic :: iso_c_binding, only: c_ptr, c_int
      type(c_ptr), value :: handle
      integer(c_int) :: status
      end function
    function dlerror() result(error) bind(c)
      use,intrinsic :: iso_c_binding, only: c_ptr
      type(c_ptr) :: error
      end function
  end interface

contains

  subroutine shlib_open (this, path, mode, stat, errmsg)
    class(shlib), intent(inout) :: this
    character(*), intent(in) :: path
    integer(c_int), intent(in) :: mode
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    if (present(stat)) stat = 0
    this%handle = dlopen(trim(path)//C_NULL_CHAR, mode)
    if (.not.c_associated(this%handle)) call error_handler ('SHLIB:OPEN', dlerror(), stat, errmsg)
  end subroutine shlib_open

  subroutine shlib_close (this, stat, errmsg)
    class(shlib), intent(inout) :: this
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    if (present(stat)) stat = 0
    if (dlclose(this%handle) /= 0) call error_handler ('SHLIB:CLOSE', dlerror(), stat, errmsg)
  end subroutine shlib_close

  subroutine shlib_sym (this, symbol, symptr, stat, errmsg)
    class(shlib), intent(in) :: this
    character(*), intent(in) :: symbol
    type(c_ptr), intent(out) :: symptr
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    type(c_ptr) :: cptr
    if (present(stat)) stat = 0
    cptr = dlerror()  ! clear any existing error
    symptr = dlsym(this%handle, trim(symbol)//C_NULL_CHAR)
    cptr = dlerror()
    if (c_associated(cptr)) call error_handler ('SHLIB:SYM', cptr, stat, errmsg)
  end subroutine shlib_sym

  subroutine shlib_func (this, symbol, funptr, stat, errmsg)
    class(shlib), intent(in) :: this
    character(*), intent(in) :: symbol
    type(c_funptr), intent(out) :: funptr
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    type(c_ptr) :: cptr
#ifdef FAKE_DLFUNC
    interface
      function dlfunc(handle, symbol) result(addr) bind(c, name='dlsym')
        use,intrinsic :: iso_c_binding, only: c_char, c_ptr, c_funptr
        type(c_ptr), value :: handle
        character(kind=c_char), intent(in) :: symbol(*)
        type(c_funptr) :: addr  ! a lie -- really a c_ptr
      end function
    end interface
#endif
    if (present(stat)) stat = 0
    cptr = dlerror()  ! clear any existing error
#ifdef FAKE_DLFUNC
    funptr = dlfunc(this%handle, trim(symbol)//C_NULL_CHAR)
#else
    funptr = transfer(dlsym(this%handle, trim(symbol)//C_NULL_CHAR),funptr)
#endif
    cptr = dlerror()
    if (c_associated(cptr)) call error_handler ('SHLIB:FUNC', cptr, stat, errmsg)
  end subroutine shlib_func

  subroutine error_handler (proc, cptr, stat, errmsg)

    use,intrinsic :: iso_fortran_env, only: error_unit
#ifdef NAGFOR
    use,intrinsic :: f90_unix, only: exit
#endif

    character(*), intent(in) :: proc
    type(c_ptr),  intent(in) :: cptr
    integer, optional, intent(out) :: stat
    character(:), allocatable, optional, intent(out) :: errmsg

    character(:,kind=c_char), allocatable :: string

    string = f_string_pointer(cptr)
    if (present(stat)) then
      stat = 1
      if (present(errmsg)) errmsg = string
    else
      write(error_unit,'(3a)') proc, ': ', string
      call exit (1)
    end if

  end subroutine error_handler

  !! Auxillary function that converts a C string pointer to a Fortran character pointer.
  function f_string_pointer (cptr) result(fptr)
    type(c_ptr), intent(in) :: cptr
    character(:,kind=c_char), pointer :: fptr
    interface ! to strlen from the standard C library
      function strlen(s) result(len) bind(c)
        use,intrinsic :: iso_c_binding, only: c_ptr, c_size_t
        type(c_ptr), value :: s
        integer(c_size_t) :: len
      end function
    end interface
    fptr => f_string_pointer_aux(cptr, strlen(cptr))
  contains
    function f_string_pointer_aux(cptr, len) result(fptr)
      type(c_ptr), intent(in) :: cptr
      integer(c_size_t), intent(in) :: len
      character(len,kind=c_char), pointer :: fptr
      call c_f_pointer (cptr, fptr)
    end function
  end function f_string_pointer

end module fortran_dynamic_loader
