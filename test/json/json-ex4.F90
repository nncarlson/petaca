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
type(json_array_iterator) :: iter1, iter2, iter3
character(:), allocatable :: errmsg
integer :: stat, n1, n2, n3

call json_from_string('[1,[2,[3]],[]]', val, stat, errmsg)
if (stat /= 0) stop 1

select type (val)
type is (json_array)

  n1 = 0
  iter1 = json_array_iterator(val)
  do while (.not.iter1%at_end())
    n1 = n1 + 1
    select type (ival1 => iter1%value())
    type is (json_integer)
      if (n1 /= 1) stop 11
      if (ival1%value /= 1) stop 12
    type is (json_array)
      select case (n1)
      case (2)
        n2 = 0
        iter2 = json_array_iterator(ival1)
        do while (.not.iter2%at_end())
          n2 = n2 + 1
          select type (ival2 => iter2%value())
          type is (json_integer)
            if (n2 /= 1) stop 21
            if (ival2%value /= 2) stop 22
          type is (json_array)
            if (n2 /= 2) stop 21
            n3 = 0
            iter3 = json_array_iterator(ival2)
            do while (.not.iter3%at_end())
              n3 = n3 + 1
              select type (ival3 => iter3%value())
              type is (json_integer)
                if (n3 /= 1) stop 31
                if (ival3%value /= 3) stop 32
              class default
                stop 10
              end select
              call iter3%next
            end do
            if (n3 /= 1) stop 9
          class default
            stop 8
          end select
          call iter2%next
        end do
        if (n2 /= 2) stop 7
      case (3)
        iter2 = json_array_iterator(ival1)
        if (.not.iter2%at_end()) stop 6
      case default
        stop 5
      end select
    class default
      stop 4
    end select
    call iter1%next
  end do
  if (n1 /= 3) stop 3
class default
  stop 2
end select

end
