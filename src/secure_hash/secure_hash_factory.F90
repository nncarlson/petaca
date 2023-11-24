!!
!! SECURE_HASH_FACTORY
!!
!! Convenience procedure to create a CLASS(SECURE_HASH) variable of a given
!! dynamic type.
!!
!! Neil N. Carlson <neil.n.carlson@gmail.com>
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2013  Neil N. Carlson
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
!!  CALL NEW_SECURE_HASH (HASH, HASH_TYPE) allocates a new CLASS(SECURE_HASH)
!!    variable HASH, of the type specified by HASH_TYPE.  The allowed values
!!    for HASH_TYPE are 'md5' and 'sha1'.  HASH may be either allocatable or
!!    a pointer.
!!
!!    With the Intel compiler, HASH is limited to being allocatable only, due
!!    to a missing implementation of a Fortran 2008 feature.
!!

#include "f90_assert.fpp"

module secure_hash_factory

  use secure_hash_class
  use md5_hash_type
  use sha1_hash_type
  implicit none
  private

  public :: secure_hash, new_secure_hash

  interface new_secure_hash
#ifdef INTEL_BUG20231123
    ! Intel compiler can't distinguish the two specifics per F2008
    procedure new_secure_hash_alloc!, new_secure_hash_ptr
#else
    procedure new_secure_hash_alloc, new_secure_hash_ptr
#endif
  end interface

contains

  subroutine new_secure_hash_alloc (hash, hash_type)
    class(secure_hash), allocatable, intent(out) :: hash
    character(*), intent(in) :: hash_type
    select case (hash_type)
    case ('sha1')
      allocate(sha1_hash::hash)
    case ('md5')
      allocate(md5_hash::hash)
    case default
      INSIST(.false.)
    end select
  end subroutine new_secure_hash_alloc

  subroutine new_secure_hash_ptr (hash, hash_type)
    class(secure_hash), pointer, intent(out) :: hash
    character(*), intent(in) :: hash_type
    select case (hash_type)
    case ('sha1')
      allocate(sha1_hash::hash)
    case ('md5')
      allocate(md5_hash::hash)
    case default
      INSIST(.false.)
    end select
  end subroutine new_secure_hash_ptr

end module secure_hash_factory
