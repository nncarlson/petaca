!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2018, Neil N. Carlson
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

use json
implicit none

class(json_value), allocatable :: val
type(json_object_iterator) :: iter
character(:), allocatable :: errmsg
integer :: stat

call json_from_string('{"abc":42,"pi":3.14,"x":"foo","y":true,"z":null}', val, stat, errmsg)
if (stat /= 0) stop 1

select type (val)
type is (json_object)

  iter = json_object_iterator(val)
  do while (.not.iter%at_end())
    select type (ival => iter%value())
    type is (json_integer)
      if (iter%name() /= 'abc') stop 11
      if (ival%value /= 42) stop 12
    type is (json_real)
      if (iter%name() /= 'pi') stop 21
      if (ival%value /= 3.14d0) stop 22
    type is (json_string)
      if (iter%name() /= 'x') stop 31
      if (ival%value /= 'foo') stop 32
    type is (json_boolean)
      if (iter%name() /= 'y') stop 41
      if (.not.ival%value) stop 42
    type is (json_null)
      if (iter%name() /= 'z') stop 51
    class default
      stop 3
    end select
    call iter%next
  end do

class default
  stop 2
end select

end
