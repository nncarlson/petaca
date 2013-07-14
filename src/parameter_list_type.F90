!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Copyright (c) 2011, Neil N. Carlson
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

#include "f90_assert.fpp"

module parameter_list_type

  use parameter_entry_type
  use map_any_type
  implicit none
  private
  
  !! The real kind for real parameter values.
  integer, parameter :: rk = kind(1.0d0)
  
  type, extends(parameter_entry), public :: parameter_list
    private
    character(len=:), allocatable :: name
    type(map_any) :: params = map_any()
  contains
    private
    procedure :: set_scalar
    procedure :: set_vector
    generic, public :: set => set_scalar, set_vector
    procedure :: get_real_scalar
    procedure :: get_integer_scalar
    procedure :: get_logical_scalar
    procedure :: get_string_scalar
    procedure :: get_integer_vector
    procedure :: get_real_vector
    procedure :: get_logical_vector
    procedure :: get_string_vector
    generic, public :: get => get_integer_scalar, get_real_scalar, get_logical_scalar, get_string_scalar, &
                              get_integer_vector, get_real_vector, get_logical_vector, get_string_vector
    procedure, public :: sublist
    procedure, public :: print
    procedure, public :: is_parameter
    procedure, public :: is_sublist
  end type
  
  type, public :: parameter_list_iterator
    private
    type(map_any_iterator) :: mapit
    logical :: sublists_only = .false.
  contains
    procedure :: next => iter_next
    procedure :: at_end => iter_at_end
    procedure :: name => iter_name
    procedure :: value => iter_value
    procedure :: is_list => iter_is_list
    procedure :: sublist => iter_sublist
    procedure :: count => iter_count
  end type parameter_list_iterator
  
  interface parameter_list_iterator
    procedure iter_begin
  end interface
  
contains

  function iter_begin (plist, sublists_only) result (iter)
    class(parameter_list), intent(in) :: plist
    logical, intent(in), optional :: sublists_only
    type(parameter_list_iterator) :: iter
#ifdef INTEL_WORKAROUND
    class(*), pointer :: uptr
#endif
    if (present(sublists_only)) iter%sublists_only = sublists_only
    iter%mapit = map_any_iterator(plist%params)
    if (iter%sublists_only) then
      do while (.not.iter%mapit%at_end())
#ifdef INTEL_WORKAROUND
        uptr => iter%mapit%value()
        select type (uptr)
#else
        select type (uptr => iter%mapit%value())
#endif
        class is (parameter_list)
          exit
        end select
        call iter%mapit%next
      end do
    end if
  end function iter_begin

  subroutine iter_next (this)
    class(parameter_list_iterator), intent(inout) :: this
#ifdef INTEL_WORKAROUND
    class(*), pointer :: uptr
#endif
    call this%mapit%next
    if (this%sublists_only) then
      do while (.not.this%mapit%at_end())
#ifdef INTEL_WORKAROUND
        uptr => this%mapit%value()
        select type (uptr)
#else
        select type (uptr => this%mapit%value())
#endif
        class is (parameter_list)
          exit
        end select
        call this%mapit%next
      end do
    end if
  end subroutine iter_next
  
  logical function iter_at_end (this) result (at_end)
    class(parameter_list_iterator), intent(in) :: this
    at_end = this%mapit%at_end()
  end function iter_at_end
  
  function iter_name (this)
    class(parameter_list_iterator), intent(in) :: this
    character(:), allocatable :: iter_name
    iter_name = this%mapit%key()
  end function iter_name
  
  function iter_value (this)
    class(parameter_list_iterator), intent(in) :: this
    class(*), pointer :: iter_value
    iter_value => this%mapit%value()
  end function iter_value
  
  logical function iter_is_list (this) result (is_list)
    class(parameter_list_iterator), intent(in) :: this
    is_list = associated(iter_sublist(this))
  end function iter_is_list
  
  function iter_sublist (this) result (sublist)
    class(parameter_list_iterator), intent(in) :: this
    class(parameter_list), pointer :: sublist
#ifdef INTEL_WORKAROUND
    class(*), pointer :: uptr
    uptr => this%mapit%value()
    select type (uptr)
#else
    select type (uptr => this%mapit%value())
#endif
    class is (parameter_list)
      sublist => uptr
    class default
      sublist => null()
    end select
  end function iter_sublist
  
  integer function iter_count (this) result (n)
    class(parameter_list_iterator), intent(in) :: this
    class(map_any_iterator), allocatable :: itemp
    n = 0
    allocate(itemp, source=this%mapit)
    do while (.not.itemp%at_end())
      n = n + 1
      call itemp%next
    end do
  end function iter_count

 !!
 !! Returns a pointer to the parameter entry in the parameter list that
 !! has the given name, or a null pointer if it is none is found.
 !!
 
  function find_entry (map, name) result (pentry)
    class(map_any), intent(in) :: map
    character(*), intent(in) :: name
    class(parameter_entry), pointer :: pentry
    class(*), pointer :: map_value
    pentry => null()
    map_value => map%value(name)
    if (associated(map_value)) then
      !! Cast the pointer to a parameter_entry
      select type (map_value)
      class is (parameter_entry)
        pentry => map_value
      end select
      ASSERT(associated(pentry))
    end if
  end function find_entry
  
  function cast_to_parameter_list (pentry) result (cast)
    class(parameter_entry), pointer, intent(in) :: pentry
    class(parameter_list), pointer :: cast
    cast => null()
    if (associated(pentry)) then
      select type (pentry)
      type is (parameter_list)
        cast => pentry
      end select
    end if
  end function cast_to_parameter_list
  
  function cast_to_any_scalar (pentry) result (cast)
    class(parameter_entry), pointer, intent(in) :: pentry
    class(any_scalar), pointer :: cast
    cast => null()
    if (associated(pentry)) then
      select type (pentry)
      type is (any_scalar)
        cast => pentry
      end select
    end if
  end function cast_to_any_scalar
  
  function cast_to_any_vector (pentry) result (cast)
    class(parameter_entry), pointer, intent(in) :: pentry
    class(any_vector), pointer :: cast
    cast => null()
    if (associated(pentry)) then
      select type (pentry)
      type is (any_vector)
        cast => pentry
      end select
    end if
  end function cast_to_any_vector
    
  function find_any_scalar_entry (this, name) result (any_scalar_entry)
    class(map_any), intent(in) :: this
    character(*), intent(in) :: name
    class(any_scalar), pointer :: any_scalar_entry
    any_scalar_entry => cast_to_any_scalar(find_entry(this, name))
  end function find_any_scalar_entry
    
  function find_any_vector_entry (this, name) result (any_vector_entry)
    class(map_any), intent(in) :: this
    character(*), intent(in) :: name
    class(any_vector), pointer :: any_vector_entry
    any_vector_entry => cast_to_any_vector(find_entry(this, name))
  end function find_any_vector_entry

 !!
 !! Returns a pointer to the parameter sublist having the
  
  function sublist (this, name, stat, errmsg)
  
    class(parameter_list), intent(inout) :: this
    character(len=*), intent(in) :: name
    class(parameter_list), pointer :: sublist
    integer, intent(out), optional :: stat
    character(len=:), allocatable, optional :: errmsg
    
    class(parameter_entry), pointer :: pentry
    
    call error_clear (stat, errmsg)
    
    pentry => find_entry(this%params, name)
    if (.not.associated(pentry)) then
      call this%params%insert (name, parameter_list(name))
      !! IMPORTANT: pentry must point to the one in the map which is a copy!
      pentry => find_entry(this%params, name)
      ASSERT(associated(pentry))
    end if
    
    sublist => cast_to_parameter_list(pentry)
    if (.not.associated(sublist)) then
      call error ('parameter is not a sublist: "' // trim(name) // '"', stat, errmsg)
    end if
    
  end function sublist
  
  
  logical function is_parameter (this, name)
    class(parameter_list), intent(in) :: this
    character(*), intent(in) :: name
    class(parameter_entry), pointer :: pentry
    pentry => find_entry(this%params, name)
    is_parameter = associated(pentry)
  end function is_parameter
  
  logical function is_sublist (this, name)
    class(parameter_list), intent(in) :: this
    character(*), intent(in) :: name
    class(parameter_entry), pointer :: pentry
    pentry => find_entry(this%params, name)
    is_sublist = associated(cast_to_parameter_list(pentry))
  end function is_sublist
      
  
  subroutine set_scalar (this, name, value, stat, errmsg)
  
    class(parameter_list), intent(inout) :: this
    character(len=*), intent(in) :: name
    class(*), intent(in) :: value
    integer, intent(out), optional :: stat
    character(len=:), allocatable, optional :: errmsg
    
    class(parameter_entry), pointer :: pentry
    
    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (parameter_list)
        call error ('parameter is a sublist: "' // trim(name) // '"', stat, errmsg)
      type is (any_scalar)
        call pentry%set_value (value)
      class default
        call error ('not a scalar parameter: "' // trim(name) // '"', stat, errmsg)
      end select
    else
      call this%params%insert(name, any_scalar(value))
    end if
    
  end subroutine set_scalar
      
  
  subroutine set_vector (this, name, value, stat, errmsg)
  
    class(parameter_list), intent(inout) :: this
    character(len=*), intent(in) :: name
    class(*), intent(in) :: value(:)
    integer, intent(out), optional :: stat
    character(len=:), allocatable, optional :: errmsg
    
    class(parameter_entry), pointer :: pentry
    
    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (parameter_list)
        call error ('parameter is a sublist: "' // trim(name) // '"', stat, errmsg)
      type is (any_vector)
        call pentry%set_value (value)
      class default
        call error ('not a vector parameter: "' // trim(name) // '"', stat, errmsg)
      end select
    else
      call this%params%insert(name, any_vector(value))
    end if
    
  end subroutine set_vector
    
  subroutine get_scalar (this, name, value, stat, errmsg)
  
    class(parameter_list), intent(in) :: this
    character(*), intent(in) :: name
    class(*), allocatable, intent(out) :: value
    integer, intent(out), optional :: stat
    character(len=:), allocatable, optional :: errmsg
    
    class(any_scalar), pointer :: scalar
    
    call error_clear (stat, errmsg)
    scalar => find_any_scalar_entry(this%params, name)
    if (associated(scalar)) then
      call scalar%get_value (value)
    else
      call error ('no such scalar parameter "' // trim(name) // '"', stat, errmsg)
    end if 
    
  end subroutine get_scalar
  
  subroutine get_integer_scalar (this, name, value, default, stat, errmsg)
  
    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    integer, intent(out) :: value
    integer, intent(in), optional :: default
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    
    logical :: errc
    class(parameter_entry), pointer :: pentry
    
    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_scalar)
        call pentry%get_value (value, errc)
        if (errc) call error ('not an integer parameter: "' // trim(name) // '"', stat, errmsg)
      class default
        call error ('not a scalar parameter: "' // trim(name) // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_scalar (this, name, default)
        value = default
      else
        call error ('no such parameter: "' // trim(name) // '"', stat, errmsg)
      end if
    end if
    
  end subroutine get_integer_scalar
  
  subroutine get_real_scalar (this, name, value, default, stat, errmsg)
  
    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    real(rk), intent(out) :: value
    real(rk), intent(in), optional :: default
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    
    logical :: errc
    class(parameter_entry), pointer :: pentry
    
    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_scalar)
        call pentry%get_value (value, errc)
        if (errc) call error ('not a real parameter "' // trim(name) // '"', stat, errmsg)
      class default
        call error ('not a scalar parameter "' // trim(name) // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_scalar (this, name, default)
        value = default
      else
        call error ('no such parameter "' // trim(name) // '"', stat, errmsg)
      end if
    end if
    
  end subroutine get_real_scalar
  
  subroutine get_logical_scalar (this, name, value, default, stat, errmsg)
  
    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    logical, intent(out) :: value
    logical, intent(in), optional :: default
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    
    logical :: errc
    class(parameter_entry), pointer :: pentry
    
    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_scalar)
        call pentry%get_value (value, errc)
        if (errc) call error ('not a logical parameter "' // trim(name) // '"', stat, errmsg)
      class default
        call error ('not a scalar parameter "' // trim(name) // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_scalar (this, name, default)
        value = default
      else
        call error ('no such parameter "' // trim(name) // '"', stat, errmsg)
      end if
    end if
    
  end subroutine get_logical_scalar
  
  subroutine get_string_scalar (this, name, value, default, stat, errmsg)
  
    class(parameter_list), intent(inout) :: this
    character(*), intent(in) :: name
    character(:), allocatable, intent(out) :: value
    character(*), intent(in), optional :: default
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    
    logical :: errc
    class(parameter_entry), pointer :: pentry
    
    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_scalar)
        call pentry%get_value (value, errc)
        if (errc) call error ('not a string parameter "' // trim(name) // '"', stat, errmsg)
      class default
        call error ('not a scalar parameter "' // trim(name) // '"', stat, errmsg)
      end select
    else
      if (present(default)) then
        call set_scalar (this, name, default)
        value = default
      else
        call error ('no such parameter "' // trim(name) // '"', stat, errmsg)
      end if
    end if
    
  end subroutine get_string_scalar
  
  
  subroutine get_integer_vector (this, name, value, stat, errmsg)
  
    class(parameter_list), intent(in) :: this
    character(*), intent(in) :: name
    integer, allocatable, intent(out) :: value(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    
    class(parameter_entry), pointer :: pentry
    logical :: errc
    
    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_vector)
        call pentry%get_value(value, errc)
        if (errc) call error ('not an integer parameter: "' // trim(name) // '"', stat, errmsg)
      class default
        call error ('not a vector parameter: "' // trim(name) // '"', stat, errmsg)
      end select
    else
      call error ('no such parameter: "' // trim(name) // '"', stat, errmsg)
    end if
    
  end subroutine get_integer_vector

  subroutine get_real_vector (this, name, value, stat, errmsg)
  
    class(parameter_list), intent(in) :: this
    character(*), intent(in) :: name
    real(rk), allocatable, intent(out) :: value(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    
    class(parameter_entry), pointer :: pentry
    logical :: errc
    
    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_vector)
        call pentry%get_value(value, errc)
        if (errc) call error ('not an real parameter: "' // trim(name) // '"', stat, errmsg)
      class default
        call error ('not a vector parameter: "' // trim(name) // '"', stat, errmsg)
      end select
    else
      call error ('no such parameter: "' // trim(name) // '"', stat, errmsg)
    end if
    
  end subroutine get_real_vector

  subroutine get_logical_vector (this, name, value, stat, errmsg)
  
    class(parameter_list), intent(in) :: this
    character(*), intent(in) :: name
    logical, allocatable, intent(out) :: value(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    
    logical :: errc
    class(parameter_entry), pointer :: pentry
    
    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_vector)
        call pentry%get_value (value, errc)
        if (errc) call error ('not a logical parameter "' // trim(name) // '"', stat, errmsg)
      class default
        call error ('not a vector parameter "' // trim(name) // '"', stat, errmsg)
      end select
    else
      call error ('no such parameter "' // trim(name) // '"', stat, errmsg)
    end if
    
  end subroutine get_logical_vector
  
  subroutine get_string_vector (this, name, value, stat, errmsg)
  
    class(parameter_list), intent(in) :: this
    character(*), intent(in) :: name
    character(:), allocatable, intent(out) :: value(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    
    logical :: errc
    class(parameter_entry), pointer :: pentry
    
    call error_clear (stat, errmsg)
    pentry => find_entry(this%params, name)
    if (associated(pentry)) then
      select type (pentry)
      type is (any_vector)
        call pentry%get_value (value, errc)
        if (errc) call error ('not a string parameter "' // trim(name) // '"', stat, errmsg)
      class default
        call error ('not a vector parameter "' // trim(name) // '"', stat, errmsg)
      end select
    else
      call error ('no such parameter "' // trim(name) // '"', stat, errmsg)
    end if
    
  end subroutine get_string_vector
  
  subroutine error_clear (stat, errmsg)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    if (present(stat)) stat = 0
    if (present(errmsg)) errmsg = ''
  end subroutine error_clear
    
  subroutine error (errmsg_, stat, errmsg)
    character(*), intent(in) :: errmsg_
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    if (present(stat)) then
      stat = 1
      if (present(errmsg)) errmsg = errmsg_
    else
      write(0,*) 'ERROR: '// errmsg_
      stop
    end if
  end subroutine error

  recursive subroutine print (this, unit, prefix)
    class(parameter_list), intent(in) :: this
    integer, intent(in) :: unit
    character(*), intent(in) :: prefix
    type(map_any_iterator) :: item
#ifdef INTEL_WORKAROUND
    class(*), pointer :: pentry
#endif
    item = map_any_iterator(this%params)
    if (item%at_end()) then
      write(unit,'(2a)') prefix, '[empty list]'
    else
      do while (.not.item%at_end())
        write(unit,'(a)',advance='no') prefix // '"' // item%key() // '"'
#ifdef INTEL_WORKAROUND
        pentry => item%value()
        select type (pentry)
#else
        select type (pentry => item%value())
#endif
        type is (parameter_list)
          write(unit,'(a)') ' -->'
          call print (pentry, unit, prefix // '  ')
        type is (any_scalar)
          write(unit,'(a)',advance='no') ' = '
          call pentry%write(unit)
          write(unit,*)
        type is (any_vector)
          write(unit,'(a)',advance='no') ' = '
          call pentry%write(unit)
          write(unit,*)
        end select
        call item%next()
      end do
    end if
  end subroutine print

end module parameter_list_type
