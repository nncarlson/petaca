!!
!! SHA1_HASH_TYPE
!!
!! Implements the SHA1 hash algorithm for the SECURE_HASH class.
!! See that class for documentation.
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
!! IMPLEMENTATION NOTES
!!
!! 1. This implementation is only correct for little-endian systems. It follows
!!    the C implementation "sha1.c" found in the coreutils package used on many
!!    Linux distributions; e.g. Fedora.  That's the place to consult for changes
!!    required to make it work on big-endian systems.
!!
!! 2. Data is processed in 64-byte chunks.  Any remainder is stored in the
!!    buffer component of the object.  Its length need only be 64, but for
!!    convenience it is twice that to simplify the handling of the padding
!!    that is added to the end of the message by the DIGEST method
!!

#include "f90_assert.fpp"

module sha1_hash_type

  use,intrinsic :: iso_fortran_env, only: int8, int32, int64
  use secure_hash_class
  implicit none
  private

  type, extends(secure_hash), public :: sha1_hash
    private
    integer(int32) :: a = int(z'67452301',int32)
    integer(int32) :: b = int(z'EFCDAB89',int32)
    integer(int32) :: c = int(z'98BADCFE',int32)
    integer(int32) :: d = int(z'10325476',int32)
    integer(int32) :: e = int(z'C3D2E1F0',int32)
    integer(int64) :: total = 0
    integer        :: buflen = 0
    integer(int8)  :: buffer(128) ! See Note 2
  contains
    procedure :: digest => sha1_digest
    procedure :: hexdigest => sha1_hexdigest
    procedure :: reset => sha1_reset
    procedure :: process_bytes => sha1_process_bytes
  end type sha1_hash

contains

  elemental integer(int32) function swap32 (in) result (out)
    integer(int32), intent(in) :: in
    out = 0 ! that argument of MVBITS is INTENT(INOUT)
    call mvbits (in,  0, 8, out, 24)
    call mvbits (in,  8, 8, out, 16)
    call mvbits (in, 16, 8, out,  8)
    call mvbits (in, 24, 8, out,  0)
  end function swap32

  elemental integer(int64) function swap64 (in) result (out)
    integer(int64), intent(in) :: in
    out = 0 ! that argument of MVBITS is INTENT(INOUT)
    call mvbits (in,  0, 8, out, 56)
    call mvbits (in,  8, 8, out, 48)
    call mvbits (in, 16, 8, out, 40)
    call mvbits (in, 24, 8, out, 32)
    call mvbits (in, 32, 8, out, 24)
    call mvbits (in, 40, 8, out, 16)
    call mvbits (in, 48, 8, out,  8)
    call mvbits (in, 56, 8, out,  0)
  end function swap64

  subroutine sha1_reset (this)
    class(sha1_hash), intent(out) :: this
    !! Intent-out results in default initialization of THIS.
  end subroutine sha1_reset

  function sha1_digest (this) result (digest)

    class(sha1_hash), intent(inout) :: this
    integer(int8) :: digest(20)

    integer :: bytes, len

    bytes = this%buflen
    this%total = this%total + bytes

    !! Final buffer size after the final padding.  The final padding consists
    !! of a 1 bit, a sequence of zero bits, terminated by 8-bytes giving the
    !! size of the (unpadded message) in bits.  The 0 bits stretch the padded
    !! message out to a multiple of 64 bytes.
    if (bytes < 56) then
      len = 64
    else
      len = 128
    end if

    !! Append padding to buffer.
    this%buffer(bytes+1) = int(z'80',kind=int8)
    this%buffer(bytes+2:len-8) = 0

    !! Append total data size (in bits!) as the final 8 bytes (big-endian).
    this%buffer(len-7:len) = transfer(swap64(8*this%total), this%buffer, size=8)

    !! Process the final blocks.
    call sha1_process_blocks (this, this%buffer, len)

    !! The final 160-bit digest (20 bytes).
    digest = transfer(swap32([this%a, this%b, this%c, this%d, this%e]), digest)

    call this%reset()

  end function sha1_digest

  function sha1_hexdigest (this) result (string)

    class(sha1_hash), intent(inout) :: this
    character(:), allocatable :: string

    integer :: i, j, n
    integer(int8) :: digest(20)

    digest = sha1_digest(this)
    allocate(character(40)::string)

    j = 0
    do i = 1, 20
      n = 1 + ishft(digest(i),-4)     ! high order hex digit
      j = j + 1
      string(j:j) = '0123456789abcdef'(n:n)
      n = 1 + iand(digest(i),15_int8) ! low order hex digit
      j = j + 1
      string(j:j) = '0123456789abcdef'(n:n)
    end do

  end function sha1_hexdigest

  subroutine sha1_process_bytes (this, data, len)

    class(sha1_hash), intent(inout) :: this
    integer(int8), intent(in) :: data(*)
    integer, intent(in) :: len

    integer :: add, excess, offset

    offset = 0
    if (this%buflen > 0) then
      !! Top-off the buffer with bytes from DATA.
      add = min(len, 64 - this%buflen)
      this%buffer(this%buflen+1:this%buflen+add) = data(:add)
      this%buflen = this%buflen + add
      offset = add
      ASSERT(this%buflen <= 64)

      !! Process a completed block.
      if (this%buflen >= 64) then
        call sha1_process_blocks (this, this%buffer, 64)
        this%total = this%total + 64
        this%buflen = 0
      end if
    end if
    ASSERT(offset == len .or. this%buflen == 0)

    !! Process block-sized chunks from DATA.
    excess = modulo(len-offset,64)
    if (len - offset >= 64) then
      call sha1_process_blocks (this, data(offset+1:len-excess), len-offset-excess)
      this%total = this%total + (len-offset-excess)
      offset = len - excess
    end if

    !! Put the remaining excess bytes from DATA in the buffer.
    if (excess > 0) then
      this%buffer(:excess) = data(offset+1:len)
      this%buflen = excess
    end if

  end subroutine sha1_process_bytes

  subroutine sha1_process_blocks (this, buffer, len)

    use,intrinsic :: iso_c_binding, only: c_loc, c_f_pointer

    type(sha1_hash), intent(inout) :: this
    integer(int8), intent(in), target :: buffer(*)
    integer, intent(in) :: len

    integer :: n, first
    integer(int32) :: a, b, c, d, e, x(16)
    integer(int32), pointer :: block(:)

    ASSERT(modulo(len,64) == 0)

    first = 1
    do n = 1, len/64

      call c_f_pointer (c_loc(buffer(first)), block, shape=[16])

      x = swap32(block)

      a = this%a
      b = this%b
      c = this%c
      d = this%d
      e = this%e

      call operation (a, b, c, d, e, x,  0)
      call operation (e, a, b, c, d, x,  1)
      call operation (d, e, a, b, c, x,  2)
      call operation (c, d, e, a, b, x,  3)
      call operation (b, c, d, e, a, x,  4)
      call operation (a, b, c, d, e, x,  5)
      call operation (e, a, b, c, d, x,  6)
      call operation (d, e, a, b, c, x,  7)
      call operation (c, d, e, a, b, x,  8)
      call operation (b, c, d, e, a, x,  9)
      call operation (a, b, c, d, e, x, 10)
      call operation (e, a, b, c, d, x, 11)
      call operation (d, e, a, b, c, x, 12)
      call operation (c, d, e, a, b, x, 13)
      call operation (b, c, d, e, a, x, 14)
      call operation (a, b, c, d, e, x, 15)
      call operation (e, a, b, c, d, x, 16)
      call operation (d, e, a, b, c, x, 17)
      call operation (c, d, e, a, b, x, 18)
      call operation (b, c, d, e, a, x, 19)
      call operation (a, b, c, d, e, x, 20)
      call operation (e, a, b, c, d, x, 21)
      call operation (d, e, a, b, c, x, 22)
      call operation (c, d, e, a, b, x, 23)
      call operation (b, c, d, e, a, x, 24)
      call operation (a, b, c, d, e, x, 25)
      call operation (e, a, b, c, d, x, 26)
      call operation (d, e, a, b, c, x, 27)
      call operation (c, d, e, a, b, x, 28)
      call operation (b, c, d, e, a, x, 29)
      call operation (a, b, c, d, e, x, 30)
      call operation (e, a, b, c, d, x, 31)
      call operation (d, e, a, b, c, x, 32)
      call operation (c, d, e, a, b, x, 33)
      call operation (b, c, d, e, a, x, 34)
      call operation (a, b, c, d, e, x, 35)
      call operation (e, a, b, c, d, x, 36)
      call operation (d, e, a, b, c, x, 37)
      call operation (c, d, e, a, b, x, 38)
      call operation (b, c, d, e, a, x, 39)
      call operation (a, b, c, d, e, x, 40)
      call operation (e, a, b, c, d, x, 41)
      call operation (d, e, a, b, c, x, 42)
      call operation (c, d, e, a, b, x, 43)
      call operation (b, c, d, e, a, x, 44)
      call operation (a, b, c, d, e, x, 45)
      call operation (e, a, b, c, d, x, 46)
      call operation (d, e, a, b, c, x, 47)
      call operation (c, d, e, a, b, x, 48)
      call operation (b, c, d, e, a, x, 49)
      call operation (a, b, c, d, e, x, 50)
      call operation (e, a, b, c, d, x, 51)
      call operation (d, e, a, b, c, x, 52)
      call operation (c, d, e, a, b, x, 53)
      call operation (b, c, d, e, a, x, 54)
      call operation (a, b, c, d, e, x, 55)
      call operation (e, a, b, c, d, x, 56)
      call operation (d, e, a, b, c, x, 57)
      call operation (c, d, e, a, b, x, 58)
      call operation (b, c, d, e, a, x, 59)
      call operation (a, b, c, d, e, x, 60)
      call operation (e, a, b, c, d, x, 61)
      call operation (d, e, a, b, c, x, 62)
      call operation (c, d, e, a, b, x, 63)
      call operation (b, c, d, e, a, x, 64)
      call operation (a, b, c, d, e, x, 65)
      call operation (e, a, b, c, d, x, 66)
      call operation (d, e, a, b, c, x, 67)
      call operation (c, d, e, a, b, x, 68)
      call operation (b, c, d, e, a, x, 69)
      call operation (a, b, c, d, e, x, 70)
      call operation (e, a, b, c, d, x, 71)
      call operation (d, e, a, b, c, x, 72)
      call operation (c, d, e, a, b, x, 73)
      call operation (b, c, d, e, a, x, 74)
      call operation (a, b, c, d, e, x, 75)
      call operation (e, a, b, c, d, x, 76)
      call operation (d, e, a, b, c, x, 77)
      call operation (c, d, e, a, b, x, 78)
      call operation (b, c, d, e, a, x, 79)

      this%a = this%a + a
      this%b = this%b + b
      this%c = this%c + c
      this%d = this%d + d
      this%e = this%e + e

      first = first + 64
    end do

  contains

    subroutine operation (a, b, c, d, e, x, i)
      integer(int32), intent(in)    :: a, c, d
      integer(int32), intent(inout) :: b, e, x(0:15)
      integer, intent(in) :: i
      integer(int32) :: f, k, m
      select case (i)
      case (0:19)
        f = ieor(d,iand(b,ieor(c,d)))
        k = int(z'5A827999',kind=int32)
      case (20:39)
        f = ieor(ieor(b,c),d)
        k = int(z'6ED9EBA1',kind=int32)
      case (40:59)
        f = ior(iand(b,c),iand(d,ior(b,c)))
        k = int(z'8F1BBCDC',kind=int32)
      case (60:79)
        f = ieor(ieor(b,c),d)
        k = int(z'CA62C1D6',kind=int32)
      end select
      if (i >= 16) then
        m = ishftc(ieor(ieor(ieor(x(iand(i,maskr(4))),x(iand(i-14,maskr(4)))),&
                      x(iand(i-8,maskr(4)))),x(iand(i-3,maskr(4)))),1)
        x(iand(i,maskr(4))) = m
      else
        m = x(i)
      end if
      e = e + ishftc(a,5) + f + k + m
      b = ishftc(b,30)
    end subroutine

  end subroutine sha1_process_blocks

end module sha1_hash_type
