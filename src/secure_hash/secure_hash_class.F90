!!
!! SECURE_HASH_CLASS
!!
!! A common interface to multiple cryptographic hash algorithms.
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
!!  This module defines the abstract class SECURE_HASH which defines a common
!!  interface to multiple crytographic hash functions, like MD5 and SHA1. Each
!!  function is implemented by a derived type that extends the SECURE_HASH base
!!  class; MD5_HASH_TYPE and SHA1_HASH_TYPE, for example.  While it is possible
!!  to use a variable of one of these types directly, it is recommended to use
!!  a polymorphic variable of CLASS(SECURE_HASH) instead.  This permits easy
!!  switching between different hash functions with minimal code changes.
!!  The SECURE_HASH_FACTORY module provides a subroutine that allocates a
!!  polymorphic SECURE_HASH variable with a dynamic type given by a character
!!  string argument; that is likely the only module that application code needs
!!  to use.
!!
!!  Objects of class SECURE_HASH have the following type bound procedures.
!!  Note that objects of this class come into existence well-defined with
!!  a 0-length message; no initialization is needed.  The hash functions are
!!  currently only implemented for little endian systems.
!!
!!  UPDATE (DATA) appends the given data to the existing message.  Unlike most
!!    implementations in other languages, DATA is not restricted to being a
!!    rank-1 byte array, but can be a scalar, or rank-1, 2, or 3 array of most
!!    any intrinsic type.  Currently this is integers of kinds int8, int16,
!!    int32, int64; reals of kinds real32, real64, real128; logicals of kinds
!!    int8, int16, int32, int64 (these are available with the Intel and NAG
!!    compilers); characters of the default kind.  The kind parameters are
!!    those from the intrinsic ISO_FORTRAN_ENV module.  The storage for the
!!    actual argument DATA need not be contiguous, however this will result
!!    in an automatic copy-in to a contiguous temporary array, and it is not
!!    recommended; a future version may remove the ability to pass non-
!!    contiguous DATA.
!!
!!  HEXDIGEST() returns the digest (or hash sum) of the accumulated message as
!!    a hexadecimal character string.  The length of the string depends on the
!!    specific hash function.  This function also resets the object so that it
!!    is ready to be used for another message digest computation.
!!
!!  RESET() resets the object, returning it to its initial state, so that it is
!!    ready to be used for another message digest computation.  The HEXDIGEST
!!    method also resets the object (in addition to computing the digest), so
!!    there is very little use for this method.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! NOTES
!!
!! 1. Declaring the dummy argument DATA as an assumed-size array (DATA(*))
!!    will force non-contiguous actual arguments to be copied to a contiguous
!!    temporary that is passed instead.
!!
!! 2. The construct c_f_pointer(c_loc(...),...) effectively recasts the
!!    incoming array of one type to an array of another type.  For this to
!!    work properly requires the relative sizes to be known and for the
!!    incoming array to be contiguously stored.
!!
!! 3. We assume the storage size for a default character (len=1) to be 1 byte.
!!
!! 4. For non-interoperable types the argument to c_loc must be a scalar,
!!    so we pass just the first element of the array in these cases (dicey?)
!!

#ifdef NAGFOR
#define NAG_WORKAROUND
#endif

#include "f90_assert.fpp"

module secure_hash_class

  use,intrinsic :: iso_c_binding, only: c_loc, c_f_pointer
  use,intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
  implicit none
  private

  !! Logical kinds.  This works for the NAG and Intel compilers
  integer, parameter :: log8=int8, log16=int16, log32=int32, log64=int64

  type, abstract, public :: secure_hash
  contains
    private
    procedure(reset), deferred, public :: reset
    procedure(hexdigest), deferred, public :: hexdigest
    procedure(process_bytes), deferred, public :: process_bytes
    generic, public :: update => update_int8_0, update_int8_1, update_int8_2, update_int8_3
    procedure, non_overridable :: update_int8_0
    procedure, non_overridable :: update_int8_1
    procedure, non_overridable :: update_int8_2
    procedure, non_overridable :: update_int8_3
    generic, public :: update => update_int16_0, update_int16_1, update_int16_2, update_int16_3
    procedure, non_overridable :: update_int16_0
    procedure, non_overridable :: update_int16_1
    procedure, non_overridable :: update_int16_2
    procedure, non_overridable :: update_int16_3
    generic, public :: update => update_int32_0, update_int32_1, update_int32_2, update_int32_3
    procedure, non_overridable :: update_int32_0
    procedure, non_overridable :: update_int32_1
    procedure, non_overridable :: update_int32_2
    procedure, non_overridable :: update_int32_3
    generic, public :: update => update_int64_0, update_int64_1, update_int64_2, update_int64_3
    procedure, non_overridable :: update_int64_0
    procedure, non_overridable :: update_int64_1
    procedure, non_overridable :: update_int64_2
    procedure, non_overridable :: update_int64_3
    generic, public :: update => update_log8_0, update_log8_1, update_log8_2, update_log8_3
    procedure, non_overridable :: update_log8_0
    procedure, non_overridable :: update_log8_1
    procedure, non_overridable :: update_log8_2
    procedure, non_overridable :: update_log8_3
    generic, public :: update => update_log16_0, update_log16_1, update_log16_2, update_log16_3
    procedure, non_overridable :: update_log16_0
    procedure, non_overridable :: update_log16_1
    procedure, non_overridable :: update_log16_2
    procedure, non_overridable :: update_log16_3
    generic, public :: update => update_log32_0, update_log32_1, update_log32_2, update_log32_3
    procedure, non_overridable :: update_log32_0
    procedure, non_overridable :: update_log32_1
    procedure, non_overridable :: update_log32_2
    procedure, non_overridable :: update_log32_3
    generic, public :: update => update_log64_0, update_log64_1, update_log64_2, update_log64_3
    procedure, non_overridable :: update_log64_0
    procedure, non_overridable :: update_log64_1
    procedure, non_overridable :: update_log64_2
    procedure, non_overridable :: update_log64_3
    generic, public :: update => update_real32_0, update_real32_1, update_real32_2, update_real32_3
    procedure, non_overridable :: update_real32_0
    procedure, non_overridable :: update_real32_1
    procedure, non_overridable :: update_real32_2
    procedure, non_overridable :: update_real32_3
    generic, public :: update => update_real64_0, update_real64_1, update_real64_2, update_real64_3
    procedure, non_overridable :: update_real64_0
    procedure, non_overridable :: update_real64_1
    procedure, non_overridable :: update_real64_2
    procedure, non_overridable :: update_real64_3
    generic, public :: update => update_real128_0, update_real128_1, update_real128_2, update_real128_3
    procedure, non_overridable :: update_real128_0
    procedure, non_overridable :: update_real128_1
    procedure, non_overridable :: update_real128_2
    procedure, non_overridable :: update_real128_3
    generic, public :: update => update_char_0, update_char_1, update_char_2, update_char_3
    procedure, non_overridable :: update_char_0
    procedure, non_overridable :: update_char_1
    procedure, non_overridable :: update_char_2
    procedure, non_overridable :: update_char_3
  end type secure_hash

  abstract interface
    subroutine reset (this)
      import :: secure_hash
      class(secure_hash), intent(out) :: this
      end subroutine
    function hexdigest (this) result (string)
      import :: secure_hash
      class(secure_hash), intent(inout) :: this
      character(:), allocatable :: string
      end function
    subroutine process_bytes (this, data, len)
      import :: secure_hash, int8
      class(secure_hash), intent(inout) :: this
      integer(int8), intent(in) :: data(*)
      integer, intent(in) :: len
      end subroutine
  end interface

contains

!!
!! The following single-procedure implementation of update (for rank-1) data
!! would be very attractive but for one problem: it only works for contiguously
!! stored data, and it gives incorrect results otherwise.  This is due to the
!! c_f_pointer(c_loc(...)) process of casting the data to a byte array. There
!! is nothing in the interface that would require the compiler to use copy-in
!! to a contiguous temporary when the the actual data argument is not, and
!! that is what is required to make this implementation robust.  Note that the
!! usual way of forcing copy-in by declaring the array to be assumed-size is
!! not allowed for class(*) dummy arguments.  The Fortran 2008 "contiguous"
!! attribute for dummy arguments may solve this problem, however the current
!! versions of the Intel and NAG compilers do not yet support this feature.
!
!  subroutine update1 (this, data)
!    use,intrinsic :: iso_c_binding, only: c_loc, c_f_pointer
!    class(secure_hash), intent(inout) :: this
!    class(*), intent(in), target :: data(:)
!    integer(int8), pointer :: ptr(:)
!    select type (data)
!    type is (integer(int8))
!      call this%process_bytes (data, size(data))
!    type is (integer(int16))
!      call c_f_pointer (c_loc(data), ptr, shape=[2*size(data)])
!      call this%process_bytes (ptr, size(ptr))
!    !! and so forth for other types.
!    type is (character(*))
!      call char_update (this, data, len(data)*size(data))
!    class default
!      INSIST(.false.)
!    end select
!  contains
!    !! Character data is special -- we have to deal with the length value.
!    subroutine char_update (this, data, len)
!      class(secure_hash), intent(inout) :: this
!      character, intent(in), target :: data(*)
!      integer, intent(in) :: len
!      !! We assume a byte is the storage size for a character.
!      call c_f_pointer (c_loc(data), ptr, shape=[len])
!      call this%process_bytes (ptr, size(ptr))
!    end subroutine
!  end subroutine

!!!! INTEGER(KIND=INT8) DATA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine update_int8 (this, data, len)
    class(secure_hash), intent(inout) :: this
    integer(int8), intent(in), target :: data(*) ! See Note 1
    integer, intent(in) :: len
    integer(int8), pointer :: ptr(:)
    call this%process_bytes (data, len)
  end subroutine update_int8

  subroutine update_int8_0 (this, data)
    class(secure_hash), intent(inout) :: this
    integer(int8), intent(in) :: data
    call update_int8 (this, [data], 1)
  end subroutine

  subroutine update_int8_1 (this, data)
    class(secure_hash), intent(inout) :: this
    integer(int8), intent(in) :: data(:)
    call update_int8 (this, data, size(data))
  end subroutine

  subroutine update_int8_2 (this, data)
    class(secure_hash), intent(inout) :: this
    integer(int8), intent(in) :: data(:,:)
    call update_int8 (this, data, size(data))
  end subroutine

  subroutine update_int8_3 (this, data)
    class(secure_hash), intent(inout) :: this
    integer(int8), intent(in) :: data(:,:,:)
    call update_int8 (this, data, size(data))
  end subroutine

!!!! INTEGER(KIND=INT16) DATA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine update_int16 (this, data, len)
    class(secure_hash), intent(inout) :: this
    integer(int16), intent(in), target :: data(*) ! See Note 1
    integer, intent(in) :: len
    integer(int8), pointer :: ptr(:)
    call c_f_pointer (c_loc(data), ptr, shape=[2*len]) ! See Note 2
    call this%process_bytes (ptr, size(ptr))
  end subroutine update_int16

  subroutine update_int16_0 (this, data)
    class(secure_hash), intent(inout) :: this
    integer(int16), intent(in) :: data
    call update_int16 (this, [data], 1)
  end subroutine

  subroutine update_int16_1 (this, data)
    class(secure_hash), intent(inout) :: this
    integer(int16), intent(in) :: data(:)
    call update_int16 (this, data, size(data))
  end subroutine

  subroutine update_int16_2 (this, data)
    class(secure_hash), intent(inout) :: this
    integer(int16), intent(in) :: data(:,:)
    call update_int16 (this, data, size(data))
  end subroutine

  subroutine update_int16_3 (this, data)
    class(secure_hash), intent(inout) :: this
    integer(int16), intent(in) :: data(:,:,:)
    call update_int16 (this, data, size(data))
  end subroutine

!!!! INTEGER(KIND=INT32) DATA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine update_int32 (this, data, len)
    class(secure_hash), intent(inout) :: this
    integer(int32), intent(in), target :: data(*) ! See Note 1
    integer, intent(in) :: len
    integer(int8), pointer :: ptr(:)
    call c_f_pointer (c_loc(data), ptr, shape=[4*len]) ! See Note 2
    call this%process_bytes (ptr, size(ptr))
  end subroutine update_int32

  subroutine update_int32_0 (this, data)
    class(secure_hash), intent(inout) :: this
    integer(int32), intent(in) :: data
    call update_int32 (this, [data], 1)
  end subroutine

  subroutine update_int32_1 (this, data)
    class(secure_hash), intent(inout) :: this
    integer(int32), intent(in) :: data(:)
    call update_int32 (this, data, size(data))
  end subroutine

  subroutine update_int32_2 (this, data)
    class(secure_hash), intent(inout) :: this
    integer(int32), intent(in) :: data(:,:)
    call update_int32 (this, data, size(data))
  end subroutine

  subroutine update_int32_3 (this, data)
    class(secure_hash), intent(inout) :: this
    integer(int32), intent(in) :: data(:,:,:)
    call update_int32 (this, data, size(data))
  end subroutine

!!!! INTEGER(KIND=INT64) DATA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine update_int64 (this, data, len)
    class(secure_hash), intent(inout) :: this
    integer(int64), intent(in), target :: data(*) ! See Note 1
    integer, intent(in) :: len
    integer(int8), pointer :: ptr(:)
    call c_f_pointer (c_loc(data), ptr, shape=[8*len]) ! See Note 2
    call this%process_bytes (ptr, size(ptr))
  end subroutine update_int64

  subroutine update_int64_0 (this, data)
    class(secure_hash), intent(inout) :: this
    integer(int64), intent(in) :: data
    call update_int64 (this, [data], 1)
  end subroutine

  subroutine update_int64_1 (this, data)
    class(secure_hash), intent(inout) :: this
    integer(int64), intent(in) :: data(:)
    call update_int64 (this, data, size(data))
  end subroutine

  subroutine update_int64_2 (this, data)
    class(secure_hash), intent(inout) :: this
    integer(int64), intent(in) :: data(:,:)
    call update_int64 (this, data, size(data))
  end subroutine

  subroutine update_int64_3 (this, data)
    class(secure_hash), intent(inout) :: this
    integer(int64), intent(in) :: data(:,:,:)
    call update_int64 (this, data, size(data))
  end subroutine

!!!! LOGICAL(KIND=LOG8) DATA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine update_log8 (this, data, len)
    class(secure_hash), intent(inout) :: this
    logical(log8), intent(in), target :: data(*) ! See Note 1
    integer, intent(in) :: len
    integer(int8), pointer :: ptr(:)
    call c_f_pointer (c_loc(data), ptr, shape=[len]) ! See Note 2
    call this%process_bytes (ptr, size(ptr))
  end subroutine update_log8

  subroutine update_log8_0 (this, data)
    class(secure_hash), intent(inout) :: this
    logical(log8), intent(in) :: data
    call update_log8 (this, [data], 1)
  end subroutine

  subroutine update_log8_1 (this, data)
    class(secure_hash), intent(inout) :: this
    logical(log8), intent(in) :: data(:)
    call update_log8 (this, data, size(data))
  end subroutine

  subroutine update_log8_2 (this, data)
    class(secure_hash), intent(inout) :: this
    logical(log8), intent(in) :: data(:,:)
    call update_log8 (this, data, size(data))
  end subroutine

  subroutine update_log8_3 (this, data)
    class(secure_hash), intent(inout) :: this
    logical(log8), intent(in) :: data(:,:,:)
    call update_log8 (this, data, size(data))
  end subroutine

!!!! LOGICAL(KIND=LOG16) DATA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine update_log16 (this, data, len)
    class(secure_hash), intent(inout) :: this
    logical(log16), intent(in), target :: data(*) ! See Note 1
    integer, intent(in) :: len
    integer(int8), pointer :: ptr(:)
    call c_f_pointer (c_loc(data(1)), ptr, shape=[2*len]) ! See Notes 2, 4
    call this%process_bytes (ptr, size(ptr))
  end subroutine update_log16

  subroutine update_log16_0 (this, data)
    class(secure_hash), intent(inout) :: this
    logical(log16), intent(in) :: data
    call update_log16 (this, [data], 1)
  end subroutine

  subroutine update_log16_1 (this, data)
    class(secure_hash), intent(inout) :: this
    logical(log16), intent(in) :: data(:)
    call update_log16 (this, data, size(data))
  end subroutine

  subroutine update_log16_2 (this, data)
    class(secure_hash), intent(inout) :: this
    logical(log16), intent(in) :: data(:,:)
    call update_log16 (this, data, size(data))
  end subroutine

  subroutine update_log16_3 (this, data)
    class(secure_hash), intent(inout) :: this
    logical(log16), intent(in) :: data(:,:,:)
    call update_log16 (this, data, size(data))
  end subroutine

!!!! LOGICAL(KIND=LOG32) DATA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine update_log32 (this, data, len)
    class(secure_hash), intent(inout) :: this
    logical(log32), intent(in), target :: data(*) ! See Note 1
    integer, intent(in) :: len
    integer(int8), pointer :: ptr(:)
    call c_f_pointer (c_loc(data(1)), ptr, shape=[4*len]) ! See Notes 2, 4
    call this%process_bytes (ptr, size(ptr))
  end subroutine update_log32

  subroutine update_log32_0 (this, data)
    class(secure_hash), intent(inout) :: this
    logical(log32), intent(in) :: data
    call update_log32 (this, [data], 1)
  end subroutine

  subroutine update_log32_1 (this, data)
    class(secure_hash), intent(inout) :: this
    logical(log32), intent(in) :: data(:)
    call update_log32 (this, data, size(data))
  end subroutine

  subroutine update_log32_2 (this, data)
    class(secure_hash), intent(inout) :: this
    logical(log32), intent(in) :: data(:,:)
    call update_log32 (this, data, size(data))
  end subroutine

  subroutine update_log32_3 (this, data)
    class(secure_hash), intent(inout) :: this
    logical(log32), intent(in) :: data(:,:,:)
    call update_log32 (this, data, size(data))
  end subroutine

!!!! LOGICAL(KIND=LOG64) DATA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine update_log64 (this, data, len)
    class(secure_hash), intent(inout) :: this
    logical(log64), intent(in), target :: data(*) ! See Note 1
    integer, intent(in) :: len
    integer(int8), pointer :: ptr(:)
    call c_f_pointer (c_loc(data(1)), ptr, shape=[8*len]) ! See Notes 2, 4
    call this%process_bytes (ptr, size(ptr))
  end subroutine update_log64

  subroutine update_log64_0 (this, data)
    class(secure_hash), intent(inout) :: this
    logical(log64), intent(in) :: data
    call update_log64 (this, [data], 1)
  end subroutine

  subroutine update_log64_1 (this, data)
    class(secure_hash), intent(inout) :: this
    logical(log64), intent(in) :: data(:)
    call update_log64 (this, data, size(data))
  end subroutine

  subroutine update_log64_2 (this, data)
    class(secure_hash), intent(inout) :: this
    logical(log64), intent(in) :: data(:,:)
    call update_log64 (this, data, size(data))
  end subroutine

  subroutine update_log64_3 (this, data)
    class(secure_hash), intent(inout) :: this
    logical(log64), intent(in) :: data(:,:,:)
    call update_log64 (this, data, size(data))
  end subroutine

!!!! REAL(KIND=REAL32) DATA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine update_real32 (this, data, len)
    class(secure_hash), intent(inout) :: this
    real(real32), intent(in), target :: data(*) ! See Note 1
    integer, intent(in) :: len
    integer(int8), pointer :: ptr(:)
    call c_f_pointer (c_loc(data), ptr, shape=[4*len]) ! See Note 2
    call this%process_bytes (ptr, size(ptr))
  end subroutine update_real32

  subroutine update_real32_0 (this, data)
    class(secure_hash), intent(inout) :: this
    real(real32), intent(in) :: data
    call update_real32 (this, [data], 1)
  end subroutine

  subroutine update_real32_1 (this, data)
    class(secure_hash), intent(inout) :: this
    real(real32), intent(in) :: data(:)
    call update_real32 (this, data, size(data))
  end subroutine

  subroutine update_real32_2 (this, data)
    class(secure_hash), intent(inout) :: this
    real(real32), intent(in) :: data(:,:)
    call update_real32 (this, data, size(data))
  end subroutine

  subroutine update_real32_3 (this, data)
    class(secure_hash), intent(inout) :: this
    real(real32), intent(in) :: data(:,:,:)
    call update_real32 (this, data, size(data))
  end subroutine

!!!! REAL(KIND=REAL64) DATA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine update_real64 (this, data, len)
    class(secure_hash), intent(inout) :: this
    real(real64), intent(in), target :: data(*) ! See Note 1
    integer, intent(in) :: len
    integer(int8), pointer :: ptr(:)
    call c_f_pointer (c_loc(data), ptr, shape=[8*len]) ! See Note 2
    call this%process_bytes (ptr, size(ptr))
  end subroutine update_real64

  subroutine update_real64_0 (this, data)
    class(secure_hash), intent(inout) :: this
    real(real64), intent(in) :: data
    call update_real64 (this, [data], 1)
  end subroutine

  subroutine update_real64_1 (this, data)
    class(secure_hash), intent(inout) :: this
    real(real64), intent(in) :: data(:)
    call update_real64 (this, data, size(data))
  end subroutine

  subroutine update_real64_2 (this, data)
    class(secure_hash), intent(inout) :: this
    real(real64), intent(in) :: data(:,:)
    call update_real64 (this, data, size(data))
  end subroutine

  subroutine update_real64_3 (this, data)
    class(secure_hash), intent(inout) :: this
    real(real64), intent(in) :: data(:,:,:)
    call update_real64 (this, data, size(data))
  end subroutine

!!!! REAL(KIND=REAL128) DATA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine update_real128 (this, data, len)
    class(secure_hash), intent(inout) :: this
    real(real128), intent(in), target :: data(*) ! See Note 1
    integer, intent(in) :: len
    integer(int8), pointer :: ptr(:)
    call c_f_pointer (c_loc(data(1)), ptr, shape=[16*len]) ! See Notes 2, 4
    call this%process_bytes (ptr, size(ptr))
  end subroutine update_real128

  subroutine update_real128_0 (this, data)
    class(secure_hash), intent(inout) :: this
    real(real128), intent(in) :: data
    call update_real128 (this, [data], 1)
  end subroutine

  subroutine update_real128_1 (this, data)
    class(secure_hash), intent(inout) :: this
    real(real128), intent(in) :: data(:)
    call update_real128 (this, data, size(data))
  end subroutine

  subroutine update_real128_2 (this, data)
    class(secure_hash), intent(inout) :: this
    real(real128), intent(in) :: data(:,:)
    call update_real128 (this, data, size(data))
  end subroutine

  subroutine update_real128_3 (this, data)
    class(secure_hash), intent(inout) :: this
    real(real128), intent(in) :: data(:,:,:)
    call update_real128 (this, data, size(data))
  end subroutine

!!!! DEFAULT CHARACTER DATA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine update_char (this, data, len)
    class(secure_hash), intent(inout) :: this
    character, intent(in), target :: data(*) ! See Note 1
    integer, intent(in) :: len
    integer(int8), pointer :: ptr(:)
    call c_f_pointer (c_loc(data), ptr, shape=[len]) ! See Notes 2, 3
    call this%process_bytes (ptr, size(ptr))
  end subroutine

  subroutine update_char_0 (this, data)
    class(secure_hash), intent(inout) :: this
    character(*), intent(in) :: data
#ifdef NAG_WORKAROUND
    !! There's something wrong with the -C generated code.
    !! Probably ought to do something similar with the other flavors below.
    if (len(data) == 0) return
#endif
    call update_char (this, data, len(data))
  end subroutine

  subroutine update_char_1 (this, data)
    class(secure_hash), intent(inout) :: this
    character(*), intent(in) :: data(:)
    call update_char (this, data, len(data)*size(data))
  end subroutine

  subroutine update_char_2 (this, data)
    class(secure_hash), intent(inout) :: this
    character(*), intent(in) :: data(:,:)
    call update_char (this, data, len(data)*size(data))
  end subroutine

  subroutine update_char_3 (this, data)
    class(secure_hash), intent(inout) :: this
    character(*), intent(in) :: data(:,:,:)
    call update_char (this, data, len(data)*size(data))
  end subroutine

end module secure_hash_class
