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
character(:), allocatable :: errmsg
integer :: stat

call json_from_string('42', val, stat, errmsg)
if (stat /= 0) stop 11
select type (val)
type is (json_integer)
  if (val%value /= 42) stop 12
class default
  stop 13
end select

call json_from_string('3.14', val, stat, errmsg)
if (stat /= 0) stop 21
select type (val)
type is (json_real)
  if (val%value /= 3.14d0) stop 22
class default
  stop 23
end select

call json_from_string('"foo"', val, stat, errmsg)
if (stat /= 0) stop 31
select type (val)
type is (json_string)
  if (val%value /= 'foo') stop 32
class default
  stop 33
end select

call json_from_string('true', val, stat, errmsg)
if (stat /= 0) stop 41
select type (val)
type is (json_boolean)
  if (.not.val%value) stop 42
class default
  stop 43
end select

call json_from_string('false', val, stat, errmsg)
if (stat /= 0) stop 51
select type (val)
type is (json_boolean)
  if (val%value) stop 52
class default
  stop 53
end select

call json_from_string('null', val, stat, errmsg)
if (stat /= 0) stop 61
select type (val)
type is (json_null)
class default
  stop 63
end select

end
