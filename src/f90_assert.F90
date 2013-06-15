!!
!! F90_ASSERT -- C-style assertions for Fortran.
!!
!!  Neil N. Carlson <neil.n.carlson@gmail.com>
!!
!! USAGE
!!
!!  At the top of each source file where you wish to use assertions,
!!  outside the definition of any module or procedure, include the
!!  preprocessor file f90_assert.fpp,
!!
!!    #include "f90_assert.fpp"
!!
!!  to define the ASSERT() and INSIST() preprocessor macros.  If the macro
!!  NDEBUG is not defined when the file is passed through the preprocessor,
!!  lines of the form
!!
!!    ASSERT( <scalar logical expression> )
!!
!!  will be expanded to Fortran code which tests whether the logical
!!  expression is true, and if not, calls the following routine which
!!  will print the file name and line number and then halt execution.
!!  If the macro NDEBUG is defined (e.g., -D NDEBUG), then the ASSERT()
!!  is expanded to a Fortran comment line.
!!
!!  The INSIST() macro functions exactly like ASSERT() except that it is
!!  always expanded to Fortran code that tests the logical expression,
!!  regardless of whether NDEBUG is defined or not.
!!
!!  This is intentionally not a module procedure.
!!
!!  NB: Use with Fortran-aware preprocessors like fpp is robust.  One
!!  can use the C preprocessor cpp, but if the expanded macro extends
!!  the line past 132 characters, a compiler error will probably result.
!!

subroutine f90_assert (file, line)

  use iso_fortran_env, only: error_unit

  character(*), intent(in) :: file
  integer,      intent(in) :: line

  write(error_unit,fmt='(3a,i4.4)') 'Assertion failed at ', file, ':', line
  stop

end subroutine f90_assert

!! Here is a possible alternative that should throw an exception instead of
!! terminating normally.  Debuggers will catch this.  However a breakpoint
!! could be set in the above routine almost as easily.

!subroutine f90_assert (file, line)
!
!  use iso_fortran_env, only: error_unit
!  use ieee_exceptions
!
!  character(*), intent(in) :: file
!  integer,      intent(in) :: line
!
!  write(error_unit,fmt='(3a,i4.4)') 'Assertion failed at ', file, ':', line
!  call ieee_set_halting_mode (ieee_invalid, .true.)
!  call ieee_set_flag (ieee_invalid, .true.)
!  stop
!
!end subroutine f90_assert
