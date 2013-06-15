!!
!! FORTRAN_DYNAMIC_LOADER
!!
!! Neil Carlson <neil.n.carlson@gmail.com>
!! 10 Apr 2006; last revised 29 Mar 2013.
!!
!! This module provides an interface to the system dynamic loader (DL).
!! It uses the C-interoperability features of Fortran to directly interface
!! with the POSIX C functions dlopen, dlclose, dlsym, and dlerror from the
!! system DL library (libdl on linux).
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2006, 2013, Neil N. Carlson
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
!! The derived type SHLIB implements an object-oriented interface to the
!! dynamic loader, providing access to data and procedures defined by a
!! shared library.  The derived type has the following type bound subroutines.
!! Each subroutine has the optional intent-out arguments STAT and ERRMSG.
!! If STAT is present, it is assigned the value 0 if the subroutine completes
!! successfully, and a nonzero value if an error occurs.  In the latter case,
!! the allocatable character string ERRMSG, if present, is assigned the error
!! string returned by the underlying DL library.  If STAT is not present and
!! an error occurs, the error string is written to the preconnected error unit
!! and the program exits with a nonzero status.
!!
!! The DL library must included when linking the executable (-ldl on linux)
!! to resolve the symbols dlopen, dlclose, dlsym and dlerror.
!!
!!  OPEN(FILENAME, MODE [,STAT [,ERRMSG]]) loads the shared library file
!!    named by the character argument FILENAME and associates it with the
!!    SHLIB object.  If FILENAME contains a slash (/), it is interpreted as
!!    a relative or absolute pathname.  Otherwise the dynamic loader searches
!!    a certain list of directories for the library; see dlopen(3) for details.
!!    The value supplied for MODE must be one of the module parameters
!!    RTLD_LAZY or RTLD_NOW, optionally or'ed with one of the module parameters
!!    RTLD_GLOBAL or RTLD_LOCAL.  See dlopen(3) for a detailed description of
!!    the behavior.  A shared library file may be "loaded" multiple times.
!!    In reality it is only loaded once by the underlying DL, which maintains
!!    a reference count.
!!
!!  CLOSE([STAT [,ERRMSG]]) disassociates the shared library from the object
!!    and decrements the library reference count.  When the reference count
!!    and the library reaches zero, it is unloaded.  See the man page for
!!    dlclose(3) for more details.
!!
!!  FUNC(SYMBOL, FUNPTR [,STAT [,ERRMSG]]) returns the memory address where
!!    the specified function symbol from the shared library is loaded.  The
!!    character argument SYMBOL gives the symbol name, and the address is
!!    returned in the TYPE(C_FUNPTR) argument FUNPTR.  The caller is
!!    responsible for converting this C function pointer value to an
!!    appropriate Fortran procedure pointer using the subroutine
!!    C_F_PROCPOINTER from the intrinsic ISO_C_BINDING module.
!!
!!  SYM(SYMBOL, SYMPTR [,STAT [,ERRMSG]]) returns the memory address where
!!    the specified data symbol from the shared library is loaded.  The
!!    character argument SYMBOL gives the symbol name, and the address is
!!    returned in the TYPE(C_PTR) argument SYMPTR. The caller is responsible
!!    for converting this C data pointer value to an appropriate Fortran
!!    data pointer using the subroutine C_F_POINTER from the intrinsic
!!    ISO_C_BINDING module.
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
!! There is the well-known issue that dlsym returns a pointer to a data
!! object (void *) regardless of whether the symbol is a function or a data
!! object, but that casts between pointer to data and pointer to function are
!! disallowed in C; there is no guarantee that the two C pointer types have
!! the same size.  In practice, compilers allow the casting as an extension,
!! or if not, there is another method that effectively does the casting.
!! The assumption is that a data pointer is wide enough to hold a function
!! pointer.
!!
!! Following the example of BSD, we have provided distinct methods for
!! getting a pointer to a data symbol (SYM) and and a function symbol (FUNC).
!! Under the covers, however, there is only the single library function dlsym
!! (on Linux, at least).  This has required using two alternative approaches,
!! depending on the Fortran compiler.  One is to define (locally) two different
!! interfaces for dlsym, one returning a C data pointer and the other a C
!! function pointer.  The other is to use a single interface for dlsym, but
!! use the transfer intrinsic to copy its C_PTR return result to a C_FUNPTR
!! variable.  The issue with the former alternative is that the compiler may
!! detect the interface mismatch (interfaces are global entities according to
!! the standard) and refuse to compile the code.  The danger with the latter
!! is that the implementation of C_PTR and C_FUNPTR is not specified by the
!! standard, and so there is no reason to expect the transfer operation to
!! do the right thing.  The expectation, however, is that both types simply
!! enclose a C pointers and that the transfer is doing the copy that the C
!! compiler would do.  See the preprocessor macro FAKE_DLFUNC below.
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

  subroutine shlib_open (this, filename, mode, stat, errmsg)
    class(shlib), intent(inout) :: this
    character(*), intent(in) :: filename
    integer(c_int), intent(in) :: mode
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    if (present(stat)) stat = 0
    this%handle = dlopen(trim(filename)//C_NULL_CHAR, mode)
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
