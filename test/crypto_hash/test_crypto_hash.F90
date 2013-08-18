!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright © 2013  Neil N. Carlson
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

#ifdef __INTEL_COMPILER
#define INTEL_WORKAROUND
#endif

module valid_hash_function

  use md5_hash_type
  use sha1_hash_type
  use crypto_hash_factory
  use,intrinsic :: iso_fortran_env
  implicit none
  private

  public :: valid_hash

  interface valid_hash
    module procedure valid_hash_0, valid_hash_1, valid_hash_2, valid_hash_3
  end interface

contains

  logical function valid_hash_aux (h)
#ifdef INTEL_WORKAROUND
    use ifport, only: system
#endif
    class(crypto_hash), intent(inout) :: h
    integer :: unit, comstat
    character(:), allocatable :: command
    !! Write the computed hash sum of the data (checksum format)
    open(newunit=unit,file='file.sum',status='replace',action='write')
    write(unit,'(2a)') h%hexdigest(), '  file.tmp'
    close(unit)
    !! Run the system checksum command to verify the result.
    select type (h)
    type is (md5_hash)
      command = 'md5sum --quiet -c file.sum'
    type is (sha1_hash)
      command = 'sha1sum --quiet -c file.sum'
    end select
#ifdef INTEL_WORKAROUND
    comstat = system(command)
#else
    call execute_command_line (command, exitstat=comstat)
#endif
    valid_hash_aux = (comstat == 0)
  end function valid_hash_aux


  logical function valid_hash_0 (a, hashtype)

    character(*), intent(in) :: hashtype
    class(*), intent(in) :: a

    class(crypto_hash), allocatable :: h
    integer :: unit

    !! Write the data to disk.
    call new_crypto_hash (h, hashtype)
    open(newunit=unit,file='file.tmp',status='replace',action='write',access='stream')
    select type (a)
    type is (integer(int8))
      write(unit) a
      call h%update (a)
    type is (integer(int16))
      write(unit) a
      call h%update (a)
    type is (integer(int32))
      write(unit) a
      call h%update (a)
    type is (integer(int64))
      write(unit) a
      call h%update (a)
    type is (real(real32))
      write(unit) a
      call h%update (a)
    type is (real(real64))
      write(unit) a
      call h%update (a)
    type is (real(real128))
      write(unit) a
      call h%update (a)
    type is (character(*))
      write(unit) a
      call h%update (a)
    type is (logical(int8))
      write(unit) a
      call h%update (a)
    type is (logical(int16))
      write(unit) a
      call h%update (a)
    type is (logical(int32))
      write(unit) a
      call h%update (a)
    type is (logical(int64))
      write(unit) a
      call h%update (a)
    end select
    close(unit)

    valid_hash_0 = valid_hash_aux(h)

  end function valid_hash_0


  logical function valid_hash_1 (a, hashtype)

    character(*), intent(in) :: hashtype
    class(*), intent(in) :: a(:)

    class(crypto_hash), allocatable :: h
    integer :: unit

    !! Write the data to disk.
    call new_crypto_hash (h, hashtype)
    open(newunit=unit,file='file.tmp',status='replace',action='write',access='stream')
    select type (a)
    type is (integer(int8))
      write(unit) a
      call h%update (a)
    type is (integer(int16))
      write(unit) a
      call h%update (a)
    type is (integer(int32))
      write(unit) a
      call h%update (a)
    type is (integer(int64))
      write(unit) a
      call h%update (a)
    type is (real(real32))
      write(unit) a
      call h%update (a)
    type is (real(real64))
      write(unit) a
      call h%update (a)
    type is (real(real128))
      write(unit) a
      call h%update (a)
    type is (character(*))
      write(unit) a
      call h%update (a)
    type is (logical(int8))
      write(unit) a
      call h%update (a)
    type is (logical(int16))
      write(unit) a
      call h%update (a)
    type is (logical(int32))
      write(unit) a
      call h%update (a)
    type is (logical(int64))
      write(unit) a
      call h%update (a)
    end select
    close(unit)

    valid_hash_1 = valid_hash_aux(h)

  end function valid_hash_1


  logical function valid_hash_2 (a, hashtype)

    character(*), intent(in) :: hashtype
    class(*), intent(in) :: a(:,:)

    class(crypto_hash), allocatable :: h
    integer :: unit

    !! Write the data to disk.
    call new_crypto_hash (h, hashtype)
    open(newunit=unit,file='file.tmp',status='replace',action='write',access='stream')
    select type (a)
    type is (integer(int8))
      write(unit) a
      call h%update (a)
    type is (integer(int16))
      write(unit) a
      call h%update (a)
    type is (integer(int32))
      write(unit) a
      call h%update (a)
    type is (integer(int64))
      write(unit) a
      call h%update (a)
    type is (real(real32))
      write(unit) a
      call h%update (a)
    type is (real(real64))
      write(unit) a
      call h%update (a)
    type is (real(real128))
      write(unit) a
      call h%update (a)
    type is (character(*))
      write(unit) a
      call h%update (a)
    type is (logical(int8))
      write(unit) a
      call h%update (a)
    type is (logical(int16))
      write(unit) a
      call h%update (a)
    type is (logical(int32))
      write(unit) a
      call h%update (a)
    type is (logical(int64))
      write(unit) a
      call h%update (a)
    end select
    close(unit)

    valid_hash_2 = valid_hash_aux(h)

  end function valid_hash_2


  logical function valid_hash_3 (a, hashtype)

    character(*), intent(in) :: hashtype
    class(*), intent(in) :: a(:,:,:)

    class(crypto_hash), allocatable :: h
    integer :: unit

    !! Write the data to disk.
    call new_crypto_hash (h, hashtype)
    open(newunit=unit,file='file.tmp',status='replace',action='write',access='stream')
    select type (a)
    type is (integer(int8))
      write(unit) a
      call h%update (a)
    type is (integer(int16))
      write(unit) a
      call h%update (a)
    type is (integer(int32))
      write(unit) a
      call h%update (a)
    type is (integer(int64))
      write(unit) a
      call h%update (a)
    type is (real(real32))
      write(unit) a
      call h%update (a)
    type is (real(real64))
      write(unit) a
      call h%update (a)
    type is (real(real128))
      write(unit) a
      call h%update (a)
    type is (character(*))
      write(unit) a
      call h%update (a)
    type is (logical(int8))
      write(unit) a
      call h%update (a)
    type is (logical(int16))
      write(unit) a
      call h%update (a)
    type is (logical(int32))
      write(unit) a
      call h%update (a)
    type is (logical(int64))
      write(unit) a
      call h%update (a)
    end select
    close(unit)

    valid_hash_3 = valid_hash_aux(h)

  end function valid_hash_3

end module valid_hash_function


program test_crypto_hash

  use crypto_hash_factory
  use valid_hash_function
  use,intrinsic :: iso_fortran_env
#ifdef NAGFOR
  use,intrinsic :: f90_unix, only: exit
#endif
  implicit none

  integer :: stat = 0

  call test_md5_blocking
  call test_md5_int8
  call test_md5_int16
  call test_md5_int32
  call test_md5_int64
  call test_md5_real32
  call test_md5_real64
  call test_md5_real128  ! real128 not portable between compilers
  call test_md5_character
  call test_md5_log8     !logical types not portable between compilers
  call test_md5_log16
  call test_md5_log32
  call test_md5_log64

  call test_sha1_blocking
  call test_sha1_int8
  call test_sha1_int16
  call test_sha1_int32
  call test_sha1_int64
  call test_sha1_real32
  call test_sha1_real64
  call test_sha1_real128  ! real128 not portable between compilers
  call test_sha1_character
  call test_sha1_log8     !logical types not portable between compilers
  call test_sha1_log16
  call test_sha1_log32
  call test_sha1_log64

  call test_noncontiguous

  call exit (stat)

contains

  subroutine test_noncontiguous

    class(crypto_hash), allocatable :: h1, h2
    integer :: a1(3), a2(2,3), a3(4)
    character(5) :: s1(2)
    character(4) :: s2(2)

    call new_crypto_hash (h1, 'md5')
    call new_crypto_hash (h2, 'md5')

    a1 = [1,2,3]
    a2(1,:) = a1
    a2(2,:) = 0

    call h1%update (a1)
    call h2%update (a2(1,:))

    call test ('test_noncontiguous: numeric simple section', h1%hexdigest() == h2%hexdigest())

    a3 = a1([3,2,1,2])

    call h1%update (a1([3,2,1,2]))
    call h2%update (a3)

    call test ('test_noncontiguous: numeric vector subscript', h1%hexdigest() == h2%hexdigest())

    s1 = ['hello', 'world']
    s2 = ['hell', 'worl']

    call h1%update (s1(:)(1:4))
    call h2%update (s2)

    call test ('test_noncontiguous: character substring', h1%hexdigest() == h2%hexdigest())

  end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! MD5_HASH TESTS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_md5_blocking

    class(crypto_hash), allocatable :: h
    call new_crypto_hash (h, 'md5')

    call test ('md5_blocking: 0-length message', &
        h%hexdigest() == 'd41d8cd98f00b204e9800998ecf8427e')

    call h%update ('One fish, two fish, red fish, blue fish.')
    call test ('md5_blocking: message length < 56', &
        h%hexdigest() == 'acf28f8a54a02741214a66986f08372d')

    call h%update ('Dr Seuss wrote "One fish, two fish, red fish, blue fish"')
    call test ('md5_blocking: message length = 56', &
        h%hexdigest() == 'ac1eca9dc9deab513d3f1903df2e4197')

    call h%update ('Beware the Jubjub bird, and shun the frumious Bandersnatch!')
    call test ('md5_blocking: 56 < message length < 64', &
        h%hexdigest() == '23feaa1106e32c22239289f78204ecc1')

    call h%update ('Twas brillig, and the slithy toves did gyre and gimble in the...')
    !open(10,file='foo',access='stream'); write(10) 'Twas brillig, and the slithy toves did gyre and gimble in the...'; close(10)
    call test ('md5_blocking: message length = 64', &
        h%hexdigest() == '7e6dad7a3166d20581bb98deea1a3aef')

    call h%update ('In the beginning was the Word, and the Word was with God, and the Word was God.')
    call test ('md5_blocking: message length > 64', &
        h%hexdigest() == '260044ab467f9cfb2606e31a3732554f')

    call h%update ('')
    call h%update ('One fish, two fish, red fish, blue fish.')
    call test ('md5_blocking: empty + message', &
        h%hexdigest() == 'acf28f8a54a02741214a66986f08372d')

    call h%update ('One fish, two fish, red fish, blue fish.')
    call h%update ('')
    call test ('md5_blocking: message + empty', &
        h%hexdigest() == 'acf28f8a54a02741214a66986f08372d')

    call h%update ('One fish, two fish,')
    call h%update (' red fish, blue fish.')
    call test ('md5_blocking: message + message < 64', &
        h%hexdigest() == 'acf28f8a54a02741214a66986f08372d')

    call h%update ('Twas brillig,')
    call h%update (' and the slithy toves did gyre and gimble in the...')
    call test ('md5_blocking: message + message = 64', &
        h%hexdigest() == '7e6dad7a3166d20581bb98deea1a3aef')

    call h%update ('In the beginning was the Word,')
    call h%update (' and the Word was with God, and the Word was God.')
    call test ('md5_blocking: message + message > 64', &
        h%hexdigest() == '260044ab467f9cfb2606e31a3732554f')

    call h%update ('When in the Course of human events, it becomes necessary for one ')
    call h%update ('people to dissolve the political bands which have connected them with &
                   &another, and to assume among the powers of the earth, the separate and &
                   &equal station to which the Laws of Nature and of Nature''s God entitle &
                   &them, a decent respect to the opinions of mankind requires that they &
                   &should declare the causes which impel them to the separation.')
    call test ('md5_blocking: long message', &
        h%hexdigest() == '95d834252b11a2f840d29c3357212723')

  end subroutine test_md5_blocking

  subroutine test_md5_int8
    integer(int8) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = -91
    a1 = a0; a2 = a0; a3 = a0
    call test ('md5_test_int8: scalar', valid_hash(a0, 'md5'))
    call test ('md5_test_int8: rank-1 array', valid_hash(a1, 'md5'))
    call test ('md5_test_int8: rank-2 array', valid_hash(a2, 'md5'))
    call test ('md5_test_int8: rank-3 array', valid_hash(a3, 'md5'))
  end subroutine test_md5_int8

  subroutine test_md5_int16
    integer(int16) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = -13001
    a1 = a0; a2 = a0; a3 = a0
    call test ('md5_test_int16: scalar', valid_hash(a0, 'md5'))
    call test ('md5_test_int16: rank-1 array', valid_hash(a1, 'md5'))
    call test ('md5_test_int16: rank-2 array', valid_hash(a2, 'md5'))
    call test ('md5_test_int16: rank-3 array', valid_hash(a3, 'md5'))
  end subroutine test_md5_int16

  subroutine test_md5_int32
    integer(int32) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = -130010023
    a1 = a0; a2 = a0; a3 = a0
    call test ('md5_test_int32: scalar', valid_hash(a0, 'md5'))
    call test ('md5_test_int32: rank-1 array', valid_hash(a1, 'md5'))
    call test ('md5_test_int32: rank-2 array', valid_hash(a2, 'md5'))
    call test ('md5_test_int32: rank-3 array', valid_hash(a3, 'md5'))
  end subroutine test_md5_int32

  subroutine test_md5_int64
    integer(int64) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = -1300100230
    a1 = a0; a2 = a0; a3 = a0
    call test ('md5_test_int64: scalar', valid_hash(a0, 'md5'))
    call test ('md5_test_int64: rank-1 array', valid_hash(a1, 'md5'))
    call test ('md5_test_int64: rank-2 array', valid_hash(a2, 'md5'))
    call test ('md5_test_int64: rank-3 array', valid_hash(a3, 'md5'))
  end subroutine test_md5_int64

  subroutine test_md5_real32
    real(real32) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = -1300100230.0
    a1 = a0; a2 = a0; a3 = a0
    call test ('md5_test_real32: scalar', valid_hash(a0, 'md5'))
    call test ('md5_test_real32: rank-1 array', valid_hash(a1, 'md5'))
    call test ('md5_test_real32: rank-2 array', valid_hash(a2, 'md5'))
    call test ('md5_test_real32: rank-3 array', valid_hash(a3, 'md5'))
  end subroutine test_md5_real32

  subroutine test_md5_real64
    real(real64) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = -130010023087.0_real64
    a1 = a0; a2 = a0; a3 = a0
    call test ('md5_test_real64: scalar', valid_hash(a0, 'md5'))
    call test ('md5_test_real64: rank-1 array', valid_hash(a1, 'md5'))
    call test ('md5_test_real64: rank-2 array', valid_hash(a2, 'md5'))
    call test ('md5_test_real64: rank-3 array', valid_hash(a3, 'md5'))
  end subroutine test_md5_real64

  subroutine test_md5_real128
    real(real128) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = -130010023087234.0_real128
    a1 = a0; a2 = a0; a3 = a0
    call test ('md5_test_real128: scalar', valid_hash(a0, 'md5'))
    call test ('md5_test_real128: rank-1 array', valid_hash(a1, 'md5'))
    call test ('md5_test_real128: rank-2 array', valid_hash(a2, 'md5'))
    call test ('md5_test_real128: rank-3 array', valid_hash(a3, 'md5'))
  end subroutine test_md5_real128

  subroutine test_md5_character
    character(5) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = 'hello'
    a1 = a0; a2 = a0; a3 = a0
    call test ('md5_test_character: scalar', valid_hash(a0, 'md5'))
    call test ('md5_test_character: rank-1 array', valid_hash(a1, 'md5'))
    call test ('md5_test_character: rank-2 array', valid_hash(a2, 'md5'))
    call test ('md5_test_character: rank-3 array', valid_hash(a3, 'md5'))
  end subroutine test_md5_character

  subroutine test_md5_log8
    logical(int8) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = .true.
    a1 = a0; a2 = a0; a3 = a0
    call test ('md5_test_log8: scalar', valid_hash(a0, 'md5'))
    call test ('md5_test_log8: rank-1 array', valid_hash(a1, 'md5'))
    call test ('md5_test_log8: rank-2 array', valid_hash(a2, 'md5'))
    call test ('md5_test_log8: rank-3 array', valid_hash(a3, 'md5'))
  end subroutine test_md5_log8

  subroutine test_md5_log16
    logical(int16) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = .true.
    a1 = a0; a2 = a0; a3 = a0
    call test ('md5_test_log16: scalar', valid_hash(a0, 'md5'))
    call test ('md5_test_log16: rank-1 array', valid_hash(a1, 'md5'))
    call test ('md5_test_log16: rank-2 array', valid_hash(a2, 'md5'))
    call test ('md5_test_log16: rank-3 array', valid_hash(a3, 'md5'))
  end subroutine test_md5_log16

  subroutine test_md5_log32
    logical(int32) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = .true.
    a1 = a0; a2 = a0; a3 = a0
    call test ('md5_test_log32: scalar', valid_hash(a0, 'md5'))
    call test ('md5_test_log32: rank-1 array', valid_hash(a1, 'md5'))
    call test ('md5_test_log32: rank-2 array', valid_hash(a2, 'md5'))
    call test ('md5_test_log32: rank-3 array', valid_hash(a3, 'md5'))
  end subroutine test_md5_log32

  subroutine test_md5_log64
    logical(int64) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = .true.
    a1 = a0; a2 = a0; a3 = a0
    call test ('md5_test_log64: scalar', valid_hash(a0, 'md5'))
    call test ('md5_test_log64: rank-1 array', valid_hash(a1, 'md5'))
    call test ('md5_test_log64: rank-2 array', valid_hash(a2, 'md5'))
    call test ('md5_test_log64: rank-3 array', valid_hash(a3, 'md5'))
  end subroutine test_md5_log64

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! SHA1_HASH TESTS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_sha1_blocking

    class(crypto_hash), allocatable :: h
    call new_crypto_hash (h, 'sha1')

    call test ('sha1_blocking: 0-length message', &
        h%hexdigest() == 'da39a3ee5e6b4b0d3255bfef95601890afd80709')

    call h%update ('One fish, two fish, red fish, blue fish.')
    call test ('sha1_blocking: message length < 56', &
        h%hexdigest() == '88893138f53571c8d96f8b1a9401e6223f9dc95f')

    call h%update ('Dr Seuss wrote "One fish, two fish, red fish, blue fish"')
    call test ('sha1_blocking: message length = 56', &
        h%hexdigest() == '062e9eb643d4659875329d9633c08112ec0a0543')

    call h%update ('Beware the Jubjub bird, and shun the frumious Bandersnatch!')
    call test ('sha1_blocking: 56 < message length < 64', &
        h%hexdigest() == 'dac9484c7ccee93dad7b36502727de82962a0d7e')

    call h%update ('Twas brillig, and the slithy toves did gyre and gimble in the...')
    call test ('sha1_blocking: message length = 64', &
        h%hexdigest() == '64c84c5565310bf88a647d39273f08490b9ebbdb')

    call h%update ('In the beginning was the Word, and the Word was with God, and the Word was God.')
    call test ('sha1_blocking: message length > 64', &
        h%hexdigest() == 'b391c34413b48e878645674da4389cda6653d54f')
    !open(10,file='foo',status='replace',access='stream')
    !write(10) 'In the beginning was the Word, and the Word was with God, and the Word was God.'
    !close(10)

    call h%update ('')
    call h%update ('One fish, two fish, red fish, blue fish.')
    call test ('sha1_blocking: empty + message', &
        h%hexdigest() == '88893138f53571c8d96f8b1a9401e6223f9dc95f')

    call h%update ('One fish, two fish, red fish, blue fish.')
    call h%update ('')
    call test ('sha1_blocking: message + empty', &
        h%hexdigest() == '88893138f53571c8d96f8b1a9401e6223f9dc95f')

    call h%update ('One fish, two fish,')
    call h%update (' red fish, blue fish.')
    call test ('sha1_blocking: message + message < 64', &
        h%hexdigest() == '88893138f53571c8d96f8b1a9401e6223f9dc95f')

    call h%update ('Twas brillig,')
    call h%update (' and the slithy toves did gyre and gimble in the...')
    call test ('sha1_blocking: message + message = 64', &
        h%hexdigest() == '64c84c5565310bf88a647d39273f08490b9ebbdb')

    call h%update ('In the beginning was the Word,')
    call h%update (' and the Word was with God, and the Word was God.')
    call test ('sha1_blocking: message + message > 64', &
        h%hexdigest() == 'b391c34413b48e878645674da4389cda6653d54f')

    call h%update ('When in the Course of human events, it becomes necessary for one ')
    call h%update ('people to dissolve the political bands which have connected them with &
                   &another, and to assume among the powers of the earth, the separate and &
                   &equal station to which the Laws of Nature and of Nature''s God entitle &
                   &them, a decent respect to the opinions of mankind requires that they &
                   &should declare the causes which impel them to the separation.')
    call test ('sha1_blocking: long message', &
        h%hexdigest() == '9e58f1b9ea88ffff917b3f65ae8f26f10d428266')

  end subroutine test_sha1_blocking


  subroutine test_sha1_int8
    integer(int8) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = -91
    a1 = a0; a2 = a0; a3 = a0
    call test ('sha1_test_int8: scalar', valid_hash(a0, 'sha1'))
    call test ('sha1_test_int8: rank-1 array', valid_hash(a1, 'sha1'))
    call test ('sha1_test_int8: rank-2 array', valid_hash(a2, 'sha1'))
    call test ('sha1_test_int8: rank-3 array', valid_hash(a3, 'sha1'))
  end subroutine test_sha1_int8

  subroutine test_sha1_int16
    integer(int16) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = -13001
    a1 = a0; a2 = a0; a3 = a0
    call test ('sha1_test_int16: scalar', valid_hash(a0, 'sha1'))
    call test ('sha1_test_int16: rank-1 array', valid_hash(a1, 'sha1'))
    call test ('sha1_test_int16: rank-2 array', valid_hash(a2, 'sha1'))
    call test ('sha1_test_int16: rank-3 array', valid_hash(a3, 'sha1'))
  end subroutine test_sha1_int16

  subroutine test_sha1_int32
    integer(int32) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = -130010023
    a1 = a0; a2 = a0; a3 = a0
    call test ('sha1_test_int32: scalar', valid_hash(a0, 'sha1'))
    call test ('sha1_test_int32: rank-1 array', valid_hash(a1, 'sha1'))
    call test ('sha1_test_int32: rank-2 array', valid_hash(a2, 'sha1'))
    call test ('sha1_test_int32: rank-3 array', valid_hash(a3, 'sha1'))
  end subroutine test_sha1_int32

  subroutine test_sha1_int64
    integer(int64) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = -1300100230
    a1 = a0; a2 = a0; a3 = a0
    call test ('sha1_test_int64: scalar', valid_hash(a0, 'sha1'))
    call test ('sha1_test_int64: rank-1 array', valid_hash(a1, 'sha1'))
    call test ('sha1_test_int64: rank-2 array', valid_hash(a2, 'sha1'))
    call test ('sha1_test_int64: rank-3 array', valid_hash(a3, 'sha1'))
  end subroutine test_sha1_int64

  subroutine test_sha1_real32
    real(real32) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = -1300100230.0
    a1 = a0; a2 = a0; a3 = a0
    call test ('sha1_test_real32: scalar', valid_hash(a0, 'sha1'))
    call test ('sha1_test_real32: rank-1 array', valid_hash(a1, 'sha1'))
    call test ('sha1_test_real32: rank-2 array', valid_hash(a2, 'sha1'))
    call test ('sha1_test_real32: rank-3 array', valid_hash(a3, 'sha1'))
  end subroutine test_sha1_real32

  subroutine test_sha1_real64
    real(real64) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = -130010023087.0_real64
    a1 = a0; a2 = a0; a3 = a0
    call test ('sha1_test_real64: scalar', valid_hash(a0, 'sha1'))
    call test ('sha1_test_real64: rank-1 array', valid_hash(a1, 'sha1'))
    call test ('sha1_test_real64: rank-2 array', valid_hash(a2, 'sha1'))
    call test ('sha1_test_real64: rank-3 array', valid_hash(a3, 'sha1'))
  end subroutine test_sha1_real64

  subroutine test_sha1_real128
    real(real128) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = -130010023087234.0_real128
    a1 = a0; a2 = a0; a3 = a0
    call test ('sha1_test_real128: scalar', valid_hash(a0, 'sha1'))
    call test ('sha1_test_real128: rank-1 array', valid_hash(a1, 'sha1'))
    call test ('sha1_test_real128: rank-2 array', valid_hash(a2, 'sha1'))
    call test ('sha1_test_real128: rank-3 array', valid_hash(a3, 'sha1'))
  end subroutine test_sha1_real128

  subroutine test_sha1_character
    character(5) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = 'hello'
    a1 = a0; a2 = a0; a3 = a0
    call test ('sha1_test_character: scalar', valid_hash(a0, 'sha1'))
    call test ('sha1_test_character: rank-1 array', valid_hash(a1, 'sha1'))
    call test ('sha1_test_character: rank-2 array', valid_hash(a2, 'sha1'))
    call test ('sha1_test_character: rank-3 array', valid_hash(a3, 'sha1'))
  end subroutine test_sha1_character

  subroutine test_sha1_log8
    logical(int8) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = .true.
    a1 = a0; a2 = a0; a3 = a0
    call test ('sha1_test_log8: scalar', valid_hash(a0, 'sha1'))
    call test ('sha1_test_log8: rank-1 array', valid_hash(a1, 'sha1'))
    call test ('sha1_test_log8: rank-2 array', valid_hash(a2, 'sha1'))
    call test ('sha1_test_log8: rank-3 array', valid_hash(a3, 'sha1'))
  end subroutine test_sha1_log8

  subroutine test_sha1_log16
    logical(int16) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = .true.
    a1 = a0; a2 = a0; a3 = a0
    call test ('sha1_test_log16: scalar', valid_hash(a0, 'sha1'))
    call test ('sha1_test_log16: rank-1 array', valid_hash(a1, 'sha1'))
    call test ('sha1_test_log16: rank-2 array', valid_hash(a2, 'sha1'))
    call test ('sha1_test_log16: rank-3 array', valid_hash(a3, 'sha1'))
  end subroutine test_sha1_log16

  subroutine test_sha1_log32
    logical(int32) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = .true.
    a1 = a0; a2 = a0; a3 = a0
    call test ('sha1_test_log32: scalar', valid_hash(a0, 'sha1'))
    call test ('sha1_test_log32: rank-1 array', valid_hash(a1, 'sha1'))
    call test ('sha1_test_log32: rank-2 array', valid_hash(a2, 'sha1'))
    call test ('sha1_test_log32: rank-3 array', valid_hash(a3, 'sha1'))
  end subroutine test_sha1_log32

  subroutine test_sha1_log64
    logical(int64) :: a0, a1(2), a2(2,3), a3(2,3,4)
    a0 = .true.
    a1 = a0; a2 = a0; a3 = a0
    call test ('sha1_test_log64: scalar', valid_hash(a0, 'sha1'))
    call test ('sha1_test_log64: rank-1 array', valid_hash(a1, 'sha1'))
    call test ('sha1_test_log64: rank-2 array', valid_hash(a2, 'sha1'))
    call test ('sha1_test_log64: rank-3 array', valid_hash(a3, 'sha1'))
  end subroutine test_sha1_log64


  subroutine test (string, expr)
    character(*), intent(in) :: string
    logical, intent(in) :: expr
    if (expr) then
      write(output_unit,fmt='(3a)') string, ': okay'
    else
      write(output_unit,fmt='(3a)') string, ': FAIL'
      stat = 1
    end if
  end subroutine

!!
!! What follows is an earlier version of some tests that compared to hardwired
!! digests.  All the integer, character, and real32 and real64 checksums were
!! invariant between Intel and NAG compilers.  However, the real128 and logical
!! checksums were not.  The real128 types differ between NAG and Intel, as does
!! the representation of logical values.  Thus this approach was not robust.
!! So instead we compare against the results of the system md5sum and sha1sum
!! commands.
!!

!  subroutine test_md5_int8
!
!    class(crypto_hash), allocatable :: h
!    integer(int8) :: a0, a1(2), a2(2,3), a3(2,3,4)
!
!    call new_crypto_hash (h, 'md5')
!
!    a0 = -91; a1 = -91; a2 = -91; a3 = -91
!
!    call h%update (a0)
!    call test ('md5_test_int8: scalar', &
!        h%hexdigest() == 'ab3af8566ddd20d7efc9b314abe90755')
!
!    call h%update (a1)
!    call test ('md5_test_int8: rank-1 array', &
!        h%hexdigest() == 'c2191f9bf4ab9740c7f9f3b676a85695')
!
!    call h%update (a2)
!    call test ('md5_test_int8: rank-2 array', &
!        h%hexdigest() == '9265a33dd4b89a379e13c8bc8d66a473')
!
!    call h%update (a3)
!    call test ('md5_test_int8: rank-3 array', &
!        h%hexdigest() == 'ffa4b08fd0c4447934bd0340e2d942ee')
!
!    !open(10,file='foo',status='replace',access='stream'); write(10) a3; close(10)
!
!  end subroutine test_md5_int8
!
!  subroutine test_md5_int16
!
!    class(crypto_hash), allocatable :: h
!    integer(int16) :: a0, a1(2), a2(2,3), a3(2,3,4)
!
!    call new_crypto_hash (h, 'md5')
!
!    a0 = -13001; a1 = -13001; a2 = -13001; a3 = -13001
!
!    call h%update (a0)
!    call test ('md5_test_int16: scalar', &
!        h%hexdigest() == '21e61934cce600a6fe8e804c2d15f0f1')
!
!    call h%update (a1)
!    call test ('md5_test_int16: rank-1 array', &
!        h%hexdigest() == 'a9b3314254c1ee61692ed8ef4fea1037')
!
!    call h%update (a2)
!    call test ('md5_test_int16: rank-2 array', &
!        h%hexdigest() == '1ffe77d4a5e9541062b06550b09b4a2d')
!
!    call h%update (a3)
!    call test ('md5_test_int16: rank-3 array', &
!        h%hexdigest() == '4843b41341a5d63126ede7b8fb27e2f6')
!
!    !! Run the system md5sum on these files to get the reference hash sums.
!    !open(10,file='foo0',status='replace',access='stream'); write(10) a0; close(10)
!    !open(10,file='foo1',status='replace',access='stream'); write(10) a1; close(10)
!    !open(10,file='foo2',status='replace',access='stream'); write(10) a2; close(10)
!    !open(10,file='foo3',status='replace',access='stream'); write(10) a3; close(10)
!
!  end subroutine test_md5_int16
!
!  subroutine test_md5_int32
!
!    class(crypto_hash), allocatable :: h
!    integer(int32) :: a0, a1(2), a2(2,3), a3(2,3,4)
!
!    call new_crypto_hash (h, 'md5')
!
!    a0 = -130010023; a1 = -130010023; a2 = -130010023; a3 = -130010023
!
!    call h%update (a0)
!    call test ('md5_test_int32: scalar', &
!        h%hexdigest() == '73c2846a1162fd8c67c9505d1e52db99')
!
!    call h%update (a1)
!    call test ('md5_test_int32: rank-1 array', &
!        h%hexdigest() == '86915407a1a8c1ff006f24cc9552fa94')
!
!    call h%update (a2)
!    call test ('md5_test_int32: rank-2 array', &
!        h%hexdigest() == '0365e3d8418883a7ee60c8132e9e9ddc')
!
!    call h%update (a3)
!    call test ('md5_test_int32: rank-3 array', &
!        h%hexdigest() == 'f5df2cc1a4ade78ae4e47f3c70779280')
!
!    !! Run the system md5sum on these files to get the reference hash sums.
!    !open(10,file='foo0',status='replace',access='stream'); write(10) a0; close(10)
!    !open(10,file='foo1',status='replace',access='stream'); write(10) a1; close(10)
!    !open(10,file='foo2',status='replace',access='stream'); write(10) a2; close(10)
!    !open(10,file='foo3',status='replace',access='stream'); write(10) a3; close(10)
!
!  end subroutine test_md5_int32
!
!  subroutine test_md5_int64
!
!    class(crypto_hash), allocatable :: h
!    integer(int64) :: a0, a1(2), a2(2,3), a3(2,3,4)
!
!    call new_crypto_hash (h, 'md5')
!
!    a0 = -1300100230; a1 = -1300100230; a2 = -1300100230; a3 = -1300100230
!
!    call h%update (a0)
!    call test ('md5_test_int64: scalar', &
!        h%hexdigest() == 'fa6d6f857571a17b285344ff703979be')
!
!    call h%update (a1)
!    call test ('md5_test_int64: rank-1 array', &
!        h%hexdigest() == 'a491731a065d3fcee8f35974233261fe')
!
!    call h%update (a2)
!    call test ('md5_test_int64: rank-2 array', &
!        h%hexdigest() == '341f2c21b4e98233022b626b40719063')
!
!    call h%update (a3)
!    call test ('md5_test_int64: rank-3 array', &
!        h%hexdigest() == '89a926c03a2ecd82db5c014d6e7fb06b')
!
!    !! Run the system md5sum on these files to get the reference hash sums.
!    !open(10,file='foo0',status='replace',access='stream'); write(10) a0; close(10)
!    !open(10,file='foo1',status='replace',access='stream'); write(10) a1; close(10)
!    !open(10,file='foo2',status='replace',access='stream'); write(10) a2; close(10)
!    !open(10,file='foo3',status='replace',access='stream'); write(10) a3; close(10)
!
!  end subroutine test_md5_int64
!
!  subroutine test_md5_real32
!
!    class(crypto_hash), allocatable :: h
!    real(real32) :: a0, a1(2), a2(2,3), a3(2,3,4)
!
!    call new_crypto_hash (h, 'md5')
!
!    a0 = -1300100230.; a1 = -1300100230.; a2 = -1300100230.; a3 = -1300100230.
!
!    call h%update (a0)
!    call test ('md5_test_real32: scalar', &
!        h%hexdigest() == 'd67906b3436bc2ea785c133a4ce7ef40')
!
!    call h%update (a1)
!    call test ('md5_test_real32: rank-1 array', &
!        h%hexdigest() == '8841cefb23b22b88b65f9a3a30112028')
!
!    call h%update (a2)
!    call test ('md5_test_real32: rank-2 array', &
!        h%hexdigest() == 'e615b9e77a3a2de7c41f28a695a3b8f5')
!
!    call h%update (a3)
!    call test ('md5_test_real32: rank-3 array', &
!        h%hexdigest() == '45251ff077f524496b20c4606fb9de49')
!
!    !! Run the system md5sum on these files to get the reference hash sums.
!    !open(10,file='foo0',status='replace',access='stream'); write(10) a0; close(10)
!    !open(10,file='foo1',status='replace',access='stream'); write(10) a1; close(10)
!    !open(10,file='foo2',status='replace',access='stream'); write(10) a2; close(10)
!    !open(10,file='foo3',status='replace',access='stream'); write(10) a3; close(10)
!
!  end subroutine test_md5_real32
!
!  subroutine test_md5_real64
!
!    class(crypto_hash), allocatable :: h
!    real(real64) :: a0, a1(2), a2(2,3), a3(2,3,4)
!
!    call new_crypto_hash (h, 'md5')
!
!    a0 = -130010023087.0_real64
!    a1 = a0; a2 = a0; a3 = a0
!
!    call h%update (a0)
!    call test ('md5_test_real64: scalar', &
!        h%hexdigest() == '8d552c83dc984a7870f48bbe7ad31b6c')
!
!    call h%update (a1)
!    call test ('md5_test_real64: rank-1 array', &
!        h%hexdigest() == 'b7348ec3b0f4f155e5b3fed951406904')
!
!    call h%update (a2)
!    call test ('md5_test_real64: rank-2 array', &
!        h%hexdigest() == '45f572ec1a3984be88da738981b919c6')
!
!    call h%update (a3)
!    call test ('md5_test_real64: rank-3 array', &
!        h%hexdigest() == '8fc85706ca13de41aaf466a514061ff1')
!
!    !! Run the system md5sum on these files to get the reference hash sums.
!    !open(10,file='foo0',status='replace',access='stream'); write(10) a0; close(10)
!    !open(10,file='foo1',status='replace',access='stream'); write(10) a1; close(10)
!    !open(10,file='foo2',status='replace',access='stream'); write(10) a2; close(10)
!    !open(10,file='foo3',status='replace',access='stream'); write(10) a3; close(10)
!
!  end subroutine test_md5_real64
!
!  subroutine test_md5_real128
!
!    class(crypto_hash), allocatable :: h
!    real(real128) :: a0, a1(2), a2(2,3), a3(2,3,4)
!
!    call new_crypto_hash (h, 'md5')
!
!    a0 = -4
!    a1 = a0; a2 = a0; a3 = a0
!
!    call h%update (a0)
!    call test ('md5_test_real128: scalar', &
!        h%hexdigest() == 'df7fdf6d0c9109c991a03ea21cf33877')
!
!    call h%update (a1)
!    call test ('md5_test_real128: rank-1 array', &
!        h%hexdigest() == '0ad99ceb878ccd02bae7203b752c9239')
!
!    call h%update (a2)
!    call test ('md5_test_real128: rank-2 array', &
!        h%hexdigest() == '9a8fcd5d2934b5956dd0f8cd3aaebb84')
!
!    call h%update (a3)
!    call test ('md5_test_real128: rank-3 array', &
!        h%hexdigest() == 'f140920bd2f506436d0cb1a837de870b')
!
!    !! Run the system md5sum on these files to get the reference hash sums.
!    open(10,file='foo0',status='replace',access='stream'); write(10) a0; close(10)
!    open(10,file='foo1',status='replace',access='stream'); write(10) a1; close(10)
!    open(10,file='foo2',status='replace',access='stream'); write(10) a2; close(10)
!    open(10,file='foo3',status='replace',access='stream'); write(10) a3; close(10)
!
!  end subroutine test_md5_real128
!
!  subroutine test_md5_character
!
!    class(crypto_hash), allocatable :: h
!    character(5) :: a0, a1(2), a2(2,3), a3(2,3,4)
!
!    call new_crypto_hash (h, 'md5')
!
!    a0 = 'hello'
!    a1 = a0; a2 = a0; a3 = a0
!
!    call h%update (a0)
!    call test ('md5_test_character: scalar', &
!        h%hexdigest() == '5d41402abc4b2a76b9719d911017c592')
!
!    call h%update (a1)
!    call test ('md5_test_character: rank-1 array', &
!        h%hexdigest() == '23b431acfeb41e15d466d75de822307c')
!
!    call h%update (a2)
!    call test ('md5_test_character: rank-2 array', &
!        h%hexdigest() == '12b35aa5d5210c534801bd96bc77a81f')
!
!    call h%update (a3)
!    call test ('md5_test_character: rank-3 array', &
!        h%hexdigest() == 'f23d3aca877088dcb40bc5f178bff2a2')
!
!  end subroutine test_md5_character
!
!  subroutine test_md5_log8
!
!    class(crypto_hash), allocatable :: h
!    logical(int8) :: a0, a1(2), a2(2,3), a3(2,3,4)
!
!    call new_crypto_hash (h, 'md5')
!
!    a0 = .true.
!    a1 = a0; a2 = a0; a3 = a0
!
!    call h%update (a0)
!    call test ('md5_test_log8: scalar', &
!        h%hexdigest() == '55a54008ad1ba589aa210d2629c1df41')
!
!    call h%update (a1)
!    call test ('md5_test_log8: rank-1 array', &
!        h%hexdigest() == '249ba6277758050695e8f5909bacd6d3')
!
!    call h%update (a2)
!    call test ('md5_test_log8: rank-2 array', &
!        h%hexdigest() == 'b590b685cec782f1527a015d04c3c686')
!
!    call h%update (a3)
!    call test ('md5_test_log8: rank-3 array', &
!        h%hexdigest() == '14548bed5b3ce76eb53b75a859e1280c')
!
!    !! Run the system md5sum on these files to get the reference hash sums.
!    open(10,file='foo0',status='replace',access='stream'); write(10) a0; close(10)
!    open(10,file='foo1',status='replace',access='stream'); write(10) a1; close(10)
!    open(10,file='foo2',status='replace',access='stream'); write(10) a2; close(10)
!    open(10,file='foo3',status='replace',access='stream'); write(10) a3; close(10)
!
!  end subroutine test_md5_log8

end program test_crypto_hash
