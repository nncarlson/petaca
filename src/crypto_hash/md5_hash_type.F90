!!
!! MD5_HASH_TYPE
!!
!! Implements MD5 hash sums for the CRYPTO_HASH class.
!!
!! Neil N. Carlson <neil.n.carlson@gmail.com>
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright © 2008, 2011, 2013  Neil N. Carlson
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
!!    the C implementation "md5.c" found in the coreutils package used on many
!!    Linux distributions; e.g. Fedora.  That's the place to consult for changes
!!    required to make it work on big-endian systems.
!!
!! 2. Data is processed in 64-byte chunks.  Any remainder is stored in the
!!    buffer component of the object.  Its length need only be 64, but for
!!    convenience it is twice that to simplify the handling of the padding
!!    that is added to the end of the message by the DIGEST method
!!

#include "f90_assert.fpp"

module md5_hash_type

  use,intrinsic :: iso_fortran_env, only: int8, int32, int64
  use crypto_hash_class
  implicit none
  private

  type, extends(crypto_hash), public :: md5_hash
    private
    integer(int32) :: a = int(z'67452301')
    integer(int32) :: b = int(z'EFCDAB89')
    integer(int32) :: c = int(z'98BADCFE')
    integer(int32) :: d = int(z'10325476')
    integer(int64) :: total = 0
    integer        :: buflen = 0
    integer(int8)  :: buffer(128) ! See Note 2
  contains
    procedure :: digest => md5_digest
    procedure :: hexdigest => md5_hexdigest
    procedure :: reset => md5_reset
    procedure :: process_bytes => md5_process_bytes
  end type md5_hash

contains

  subroutine md5_reset (this)
    class(md5_hash), intent(out) :: this
    !! Intent-out results in default initialization of THIS.
  end subroutine md5_reset

  function md5_digest (this) result (digest)

    class(md5_hash), intent(inout) :: this
    integer(int8) :: digest(16)

    integer :: bytes, len

    bytes = this%buflen
    this%total = this%total + bytes

    !! Final buffer size after the final padding.
    if (bytes < 56) then
      len = 64
    else
      len = 128
    end if

    !! Append padding to buffer.
    this%buffer(bytes+1) = int(z'80',kind=int8)
    this%buffer(bytes+2:len-8) = 0

    !! Append total data size (in bits!) as the final 8 bytes (little-endian).
    this%buffer(len-7:len) = transfer(8*this%total, this%buffer, size=8)

    !! Process the final blocks.
    call md5_process_blocks (this, this%buffer, len)

    !! The final 128-bit digest (16 bytes).
    digest = transfer([this%a, this%b, this%c, this%d], digest)

    call this%reset()

  end function md5_digest

  function md5_hexdigest (this) result (string)

    class(md5_hash), intent(inout) :: this
    character(:), allocatable :: string

    integer :: i, j, n
    integer(int8) :: digest(16)

    digest = md5_digest(this)
    allocate(character(32)::string)

    j = 0
    do i = 1, 16
      n = 1 + ishft(digest(i),-4)     ! high order hex digit
      j = j + 1
      string(j:j) = '0123456789abcdef'(n:n)
      n = 1 + iand(digest(i),15_int8) ! low order hex digit
      j = j + 1
      string(j:j) = '0123456789abcdef'(n:n)
    end do

  end function md5_hexdigest

  subroutine md5_process_bytes (this, data, len)

    class(md5_hash), intent(inout) :: this
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
        call md5_process_blocks (this, this%buffer, 64)
        this%total = this%total + 64
        this%buflen = 0
      end if
    end if
    ASSERT(offset == len .or. this%buflen == 0)

    !! Process block-sized chunks from DATA.
    excess = modulo(len-offset,64)
    if (len - offset >= 64) then
      call md5_process_blocks (this, data(offset+1:len-excess), len-offset-excess)
      this%total = this%total + (len-offset-excess)
      offset = len - excess
    end if

    !! Put the remaining excess bytes from DATA in the buffer.
    if (excess > 0) then
      this%buffer(:excess) = data(offset+1:len)
      this%buflen = excess
    end if

  end subroutine md5_process_bytes

  subroutine md5_process_blocks (this, buffer, len)

    use,intrinsic :: iso_c_binding, only: c_loc, c_f_pointer

    type(md5_hash), intent(inout) :: this
    integer(int8), intent(in), target :: buffer(*)
    integer, intent(in) :: len

    integer :: n, first
    integer(int32) :: a, b, c, d
    integer(int32), pointer :: block(:)

    ASSERT(modulo(len,64) == 0)

    first = 1
    do n = 1, len/64

      call c_f_pointer (c_loc(buffer(first)), block, shape=[16])

      a = this%a
      b = this%b
      c = this%c
      d = this%d

      !! Round 1
      call R1_operation (a, b, c, d, block( 1), int(z'D76AA478',kind=int32),  7)
      call R1_operation (d, a, b, c, block( 2), int(z'E8C7B756',kind=int32), 12)
      call R1_operation (c, d, a, b, block( 3), int(z'242070DB',kind=int32), 17)
      call R1_operation (b, c, d, a, block( 4), int(z'C1BDCEEE',kind=int32), 22)
      call R1_operation (a, b, c, d, block( 5), int(z'F57C0FAF',kind=int32),  7)
      call R1_operation (d, a, b, c, block( 6), int(z'4787C62A',kind=int32), 12)
      call R1_operation (c, d, a, b, block( 7), int(z'A8304613',kind=int32), 17)
      call R1_operation (b, c, d, a, block( 8), int(z'FD469501',kind=int32), 22)
      call R1_operation (a, b, c, d, block( 9), int(z'698098D8',kind=int32),  7)
      call R1_operation (d, a, b, c, block(10), int(z'8B44F7AF',kind=int32), 12)
      call R1_operation (c, d, a, b, block(11), int(z'FFFF5BB1',kind=int32), 17)
      call R1_operation (b, c, d, a, block(12), int(z'895CD7BE',kind=int32), 22)
      call R1_operation (a, b, c, d, block(13), int(z'6B901122',kind=int32),  7)
      call R1_operation (d, a, b, c, block(14), int(z'FD987193',kind=int32), 12)
      call R1_operation (c, d, a, b, block(15), int(z'A679438E',kind=int32), 17)
      call R1_operation (b, c, d, a, block(16), int(z'49B40821',kind=int32), 22)

      !! Round 2
      call R2_operation (a, b, c, d, block( 2), int(z'F61E2562',kind=int32),  5)
      call R2_operation (d, a, b, c, block( 7), int(z'C040B340',kind=int32),  9)
      call R2_operation (c, d, a, b, block(12), int(z'265E5A51',kind=int32), 14)
      call R2_operation (b, c, d, a, block( 1), int(z'E9B6C7AA',kind=int32), 20)
      call R2_operation (a, b, c, d, block( 6), int(z'D62F105D',kind=int32),  5)
      call R2_operation (d, a, b, c, block(11), int(z'02441453',kind=int32),  9)
      call R2_operation (c, d, a, b, block(16), int(z'D8A1E681',kind=int32), 14)
      call R2_operation (b, c, d, a, block( 5), int(z'E7D3FBC8',kind=int32), 20)
      call R2_operation (a, b, c, d, block(10), int(z'21E1CDE6',kind=int32),  5)
      call R2_operation (d, a, b, c, block(15), int(z'C33707D6',kind=int32),  9)
      call R2_operation (c, d, a, b, block( 4), int(z'F4D50D87',kind=int32), 14)
      call R2_operation (b, c, d, a, block( 9), int(z'455A14ED',kind=int32), 20)
      call R2_operation (a, b, c, d, block(14), int(z'A9E3E905',kind=int32),  5)
      call R2_operation (d, a, b, c, block( 3), int(z'FCEFA3F8',kind=int32),  9)
      call R2_operation (c, d, a, b, block( 8), int(z'676F02D9',kind=int32), 14)
      call R2_operation (b, c, d, a, block(13), int(z'8D2A4C8A',kind=int32), 20)

      !! Round 3
      call R3_operation (a, b, c, d, block( 6), int(z'FFFA3942',kind=int32),  4)
      call R3_operation (d, a, b, c, block( 9), int(z'8771F681',kind=int32), 11)
      call R3_operation (c, d, a, b, block(12), int(z'6D9D6122',kind=int32), 16)
      call R3_operation (b, c, d, a, block(15), int(z'FDE5380C',kind=int32), 23)
      call R3_operation (a, b, c, d, block( 2), int(z'A4BEEA44',kind=int32),  4)
      call R3_operation (d, a, b, c, block( 5), int(z'4BDECFA9',kind=int32), 11)
      call R3_operation (c, d, a, b, block( 8), int(z'F6BB4B60',kind=int32), 16)
      call R3_operation (b, c, d, a, block(11), int(z'BEBFBC70',kind=int32), 23)
      call R3_operation (a, b, c, d, block(14), int(z'289B7EC6',kind=int32),  4)
      call R3_operation (d, a, b, c, block( 1), int(z'EAA127FA',kind=int32), 11)
      call R3_operation (c, d, a, b, block( 4), int(z'D4EF3085',kind=int32), 16)
      call R3_operation (b, c, d, a, block( 7), int(z'04881D05',kind=int32), 23)
      call R3_operation (a, b, c, d, block(10), int(z'D9D4D039',kind=int32),  4)
      call R3_operation (d, a, b, c, block(13), int(z'E6DB99E5',kind=int32), 11)
      call R3_operation (c, d, a, b, block(16), int(z'1FA27CF8',kind=int32), 16)
      call R3_operation (b, c, d, a, block( 3), int(z'C4AC5665',kind=int32), 23)

      !! Round 4
      call R4_operation (a, b, c, d, block( 1), int(z'F4292244',kind=int32),  6)
      call R4_operation (d, a, b, c, block( 8), int(z'432AFF97',kind=int32), 10)
      call R4_operation (c, d, a, b, block(15), int(z'AB9423A7',kind=int32), 15)
      call R4_operation (b, c, d, a, block( 6), int(z'FC93A039',kind=int32), 21)
      call R4_operation (a, b, c, d, block(13), int(z'655B59C3',kind=int32),  6)
      call R4_operation (d, a, b, c, block( 4), int(z'8F0CCC92',kind=int32), 10)
      call R4_operation (c, d, a, b, block(11), int(z'FFEFF47D',kind=int32), 15)
      call R4_operation (b, c, d, a, block( 2), int(z'85845DD1',kind=int32), 21)
      call R4_operation (a, b, c, d, block( 9), int(z'6FA87E4F',kind=int32),  6)
      call R4_operation (d, a, b, c, block(16), int(z'FE2CE6E0',kind=int32), 10)
      call R4_operation (c, d, a, b, block( 7), int(z'A3014314',kind=int32), 15)
      call R4_operation (b, c, d, a, block(14), int(z'4E0811A1',kind=int32), 21)
      call R4_operation (a, b, c, d, block( 5), int(z'F7537E82',kind=int32),  6)
      call R4_operation (d, a, b, c, block(12), int(z'BD3AF235',kind=int32), 10)
      call R4_operation (c, d, a, b, block( 3), int(z'2AD7D2BB',kind=int32), 15)
      call R4_operation (b, c, d, a, block(10), int(z'EB86D391',kind=int32), 21)

      this%a = this%a + a
      this%b = this%b + b
      this%c = this%c + c
      this%d = this%d + d

      first = first + 64
    end do

  contains

    pure subroutine R1_operation (a, b, c, d, w, t, s)
      integer(int32), intent(inout) :: a
      !integer(int32), intent(in)    :: b, c, d, w, t
      !integer,     intent(in)    :: s
      integer(int32), intent(in), value    :: b, c, d, w, t
      integer,     intent(in), value    :: s
      a = ishftc(((ieor(iand(ieor(c,d),b),d) + a) + w) + t, s) + b
    end subroutine R1_operation

    pure subroutine R2_operation (a, b, c, d, w, t, s)
      integer(int32), intent(inout) :: a
      integer(int32), intent(in)    :: b, c, d, w, t
      integer,     intent(in)    :: s
      a = ishftc(((ieor(iand(ieor(b,c),d),c) + a) + w) + t, s) + b
    end subroutine R2_operation

    pure subroutine R3_operation (a, b, c, d, w, t, s)
      integer(int32), intent(inout) :: a
      integer(int32), intent(in)    :: b, c, d, w, t
      integer,     intent(in)    :: s
      a = ishftc(((ieor(ieor(b,c),d) + a) + w) + t, s) + b
    end subroutine R3_operation

    pure subroutine R4_operation (a, b, c, d, w, t, s)
      integer(int32), intent(inout) :: a
      integer(int32), intent(in)    :: b, c, d, w, t
      integer, intent(in) :: s
      a = ishftc(((ieor(ior(not(d),b),c) + a) + w) + t, s) + b
    end subroutine R4_operation

  end subroutine md5_process_blocks

end module md5_hash_type
