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
type(json_object_iterator) :: iter1, iter3
type(json_array_iterator)  :: iter2
character(:), allocatable :: errmsg
integer :: stat, n2

call json_from_string('{"a":[{"b":42}]}', val, stat, errmsg)
if (stat /= 0) stop 1

select type (val)
type is (json_object)

  iter1 = json_object_iterator(val)
  do while (.not.iter1%at_end())
    select case (iter1%name())
    case ('a')
      select type (ival1 => iter1%value())
      type is (json_array)
        n2 = 0
        iter2 = json_array_iterator(ival1)
        do while (.not.iter2%at_end())
          n2 = n2 + 1
          select type (ival2 => iter2%value())
          type is (json_object)
            if (n2 /= 1) stop 10
            iter3 = json_object_iterator(ival2)
            do while (.not.iter3%at_end())
              select case (iter3%name())
              case ('b')
                select type (ival3 => iter3%value())
                type is (json_integer)
                  if (ival3%value /= 42) stop 9
                class default
                  stop 8
                end select
              case default
                stop 7
              end select
              call iter3%next
            end do
          class default
            stop 6
          end select
          call iter2%next
        end do
        if (n2 /= 1) stop 5
      class default
        stop 4
      end select
    case default
      stop 3
    end select
    call iter1%next
  end do

class default
  stop 2
end select

end
