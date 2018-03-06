!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2013 Neil N. Carlson
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

program test_parameter_list_json

  use parameter_list_type
  use parameter_list_json

  integer :: stat = 0

  call test_vector_valid
  call test_vector_invalid
  call test_string_array

  if (stat /= 0) stop 1

contains

  !! This is meant to test the MY_SAME_TYPE_AS function which replaced earlier
  !! non-portable/non-standard use of the SAME_TYPE_AS intrinsic function.
  !! Here we are processing valid vectors (for a parameter list) of the JSON
  !! values integer, real, boolean, and string.

  subroutine test_vector_valid
    type(parameter_list), pointer :: plist
    character(:), allocatable :: errmsg
    call parameter_list_from_json_string('{"array":[1,2]}', plist, errmsg)
    if (.not.associated(plist)) call write_fail('test_vector_valid failed test 1')
    call parameter_list_from_json_string('{"array":[1.0,2.0]}', plist, errmsg)
    if (.not.associated(plist)) call write_fail('test_vector_valid failed test 2')
    call parameter_list_from_json_string('{"array":[true,false]}', plist, errmsg)
    if (.not.associated(plist)) call write_fail('test_vector_valid failed test 3')
    call parameter_list_from_json_string('{"array":["boy","girl"]}', plist, errmsg)
    if (.not.associated(plist)) call write_fail('test_vector_valid failed test 4')
  end subroutine

  !! This is meant to test the MY_SAME_TYPE_AS function which replaced earlier
  !! non-portable/non-standard use of the SAME_TYPE_AS intrinsic function.
  !! Here we are processing invalid vectors (for a parameter list) which mix
  !! different JSON types.

  subroutine test_vector_invalid
    type(parameter_list), pointer :: plist
    character(:), allocatable :: errmsg
    call parameter_list_from_json_string('{"array":[1,2.0]}', plist, errmsg)
    if (associated(plist)) call write_fail('test_vector_invalid failed test 1')
    call parameter_list_from_json_string('{"array":[1,true]}', plist, errmsg)
    if (associated(plist)) call write_fail('test_vector_invalid failed test 2')
    call parameter_list_from_json_string('{"array":[1,"girl"]}', plist, errmsg)
    if (associated(plist)) call write_fail('test_vector_invalid failed test 3')
    call parameter_list_from_json_string('{"array":[1.0,false]}', plist, errmsg)
    if (associated(plist)) call write_fail('test_vector_invalid failed test 4')
    call parameter_list_from_json_string('{"array":[1.0,"boy"]}', plist, errmsg)
    if (associated(plist)) call write_fail('test_vector_invalid failed test 5')
    call parameter_list_from_json_string('{"array":[true,"boy"]}', plist, errmsg)
    if (associated(plist)) call write_fail('test_vector_invalid failed test 6')
  end subroutine

  !! This tests the construction of string-valued arrays, which trips bugs in
  !! some compilers.

  subroutine test_string_array
    type(parameter_list), pointer :: plist
    character(:), allocatable :: errmsg, array(:)
    integer :: stat
    call parameter_list_from_json_string('{"array":["foo","bar"]}', plist, errmsg)
    if (.not.associated(plist)) call write_fail('test_string_array failed test 1')
    call plist%get('array', array, stat=stat, errmsg=errmsg)
    if (stat /= 0) call write_fail('test_string_array failed test 2: ' // errmsg)
    if (size(array) /= 2) call write_fail('test_string_array failed test3')
    if (array(1) /= 'foo') call write_fail('test_string_array failed test4: "' // array(1) // '"')
    if (array(2) /= 'bar') call write_fail('test_string_array failed test5: "' // array(2) // '"')
  end subroutine

  subroutine write_fail(errmsg)
    use,intrinsic :: iso_fortran_env, only: error_unit
    character(*), intent(in) :: errmsg
    stat = 1
    write(error_unit,'(a)') errmsg
  end subroutine

end program test_parameter_list_json
