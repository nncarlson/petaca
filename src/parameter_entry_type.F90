module parameter_entry_type

  implicit none
  private
  
  type, abstract, public :: parameter_entry
    !! meta data associated with the parameter will go here
  end type
  
  type, extends(parameter_entry), public :: any_scalar
    private
    class(*), allocatable :: value
  contains
    private
    procedure, public :: set_value => set_scalar_value
    generic,   public :: get_value => get_cs0, get_i0, get_d0, get_c0, get_l0
    procedure, public :: value_ptr => scalar_value_ptr
    procedure, public :: write => write_scalar
    procedure :: get_cs0
    procedure :: get_i0
    procedure :: get_d0
    procedure :: get_c0
    procedure :: get_l0
  end type
  
  type, extends(parameter_entry), public :: any_vector
    private
    class(*), allocatable :: value(:)
  contains
    private
    procedure, public :: set_value => set_vector_value
    generic,   public :: get_value => get_cs1, get_i1, get_d1, get_l1, get_c1
    procedure, public :: value_ptr => vector_value_ptr
    procedure, public :: write => write_vector
    procedure :: get_cs1
    procedure :: get_i1
    procedure :: get_d1
    procedure :: get_l1
    procedure :: get_c1
  end type
  
  !! Generic user-defined constructors.
  interface any_scalar
    procedure any_scalar_value
  end interface
  interface any_vector
    procedure any_vector_value
  end interface
  
contains

  !! User-defined constructors.
  function any_vector_value (value) result (obj)
    class(*), intent(in) :: value(:)
    type(any_vector) :: obj
    call set_vector_value (obj, value)
  end function any_vector_value
  function any_scalar_value (value) result (obj)
    class(*), intent(in) :: value
    type(any_scalar) :: obj
    call set_scalar_value (obj, value)
  end function any_scalar_value

  subroutine write_scalar (this, unit)
    class(any_scalar), intent(in) :: this
    integer, intent(in) :: unit
    character(len=31) :: string
    select type (uptr => this%value)
    type is (integer)
      write(unit,'(i0)',advance='no') uptr
    type is (double precision)
      write(string,fmt=*) uptr
      write(unit,'(a)',advance='no') trim(adjustl(string))
    type is (logical)
      if (uptr) then
        write(unit,'(a)',advance='no') 'true'
      else
        write(unit,'(a)',advance='no') 'false'
      end if
    type is (character(*))
      write(unit,'(3a)',advance='no') '"', uptr, '"'
    class default
      write(unit,'(a)',advance='no') '???'
    end select
  end subroutine write_scalar

  subroutine write_vector (this, unit)
    class(any_vector), intent(in) :: this
    integer, intent(in) :: unit
    integer :: n
    character(len=31) :: string
    write(unit,'(a)',advance='no') '['
    do n = 1, size(this%value)
      select type (uptr => this%value)
      type is (integer)
        write(unit,'(i0)',advance='no') uptr(n)
      type is (double precision)
        write(string,fmt=*) uptr(n)
        write(unit,'(a)',advance='no') trim(adjustl(string))
      type is (logical)
        if (uptr(n)) then
          write(unit,'(a)',advance='no') 'true'
        else
          write(unit,'(a)',advance='no') 'false'
        end if
      type is (character(*))
        write(unit,'(3a)',advance='no') '"', uptr(n), '"'
      class default
        write(unit,'(a)',advance='no') '???'
      end select
      if (n < size(this%value)) write(unit,'(a)',advance='no') ', '
    end do
    write(unit,'(a)',advance='no') ']'
  end subroutine write_vector

  function scalar_value_ptr (this) result (uptr)
    class(any_scalar), target, intent(in) :: this
    class(*), pointer :: uptr
    uptr => this%value
  end function scalar_value_ptr

  function vector_value_ptr (this) result (uptr)
    class(any_vector), target, intent(in) :: this
    class(*), pointer :: uptr(:)
    uptr => this%value
  end function vector_value_ptr

  subroutine get_cs0 (this, value)
    class(any_scalar), intent(in) :: this
    class(*), allocatable :: value
    allocate(value, source=this%value)
  end subroutine get_cs0

  subroutine get_cs1 (this, value)
    class(any_vector), intent(in) :: this
    class(*), allocatable :: value(:)
    allocate(value(lbound(this%value,1):ubound(this%value,1)), source=this%value)
  end subroutine get_cs1

  subroutine set_scalar_value (this, value)
    class(any_scalar), intent(out) :: this
    class(*), intent(in) :: value
    allocate(this%value, source=value)
  end subroutine set_scalar_value

  subroutine set_vector_value (this, value)
    class(any_vector), intent(out) :: this
    class(*), intent(in) :: value(:)
    allocate(this%value(lbound(value,1):ubound(value,1)), source=value)
  end subroutine set_vector_value
  
  subroutine get_i0 (this, value, errc)
    class(any_scalar), intent(in) :: this
    integer, intent(out) :: value
    logical, intent(out) :: errc
    select type (uptr => this%value)
    type is (integer)
      value = uptr
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine

  subroutine get_d0 (this, value, errc)
    class(any_scalar), intent(in) :: this
    double precision, intent(out) :: value
    logical, intent(out) :: errc
    select type (uptr => this%value)
    type is (double precision)
      value = uptr
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine get_d0

  subroutine get_c0 (this, value, errc)
    class(any_scalar), intent(in) :: this
    character(len=:), allocatable, intent(out) :: value
    logical, intent(out) :: errc
    select type (uptr => this%value)
    type is (character(*))
      value = uptr
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine get_c0

  subroutine get_l0 (this, value, errc)
    class(any_scalar), intent(in) :: this
    logical, intent(out) :: value
    logical, intent(out) :: errc
    select type (uptr => this%value)
    type is (logical)
      value = uptr
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine get_l0
  
  subroutine get_i1 (this, value, errc)
    class(any_vector), intent(in) :: this
    integer, allocatable, intent(out) :: value(:)
    logical, intent(out) :: errc
    select type (uptr => this%value)
    type is (integer)
      value = uptr
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine get_i1
  
  subroutine get_d1 (this, value, errc)
    class(any_vector), intent(in) :: this
    double precision, allocatable, intent(out) :: value(:)
    logical, intent(out) :: errc
    select type (uptr => this%value)
    type is (double precision)
      value = uptr
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine get_d1
  
  subroutine get_l1 (this, value, errc)
    class(any_vector), intent(in) :: this
    logical, allocatable, intent(out) :: value(:)
    logical, intent(out) :: errc
    select type (uptr => this%value)
    type is (logical)
      value = uptr
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine get_l1
  
  subroutine get_c1 (this, value, errc)
    class(any_vector), intent(in) :: this
    character(:), allocatable, intent(out) :: value(:)
    logical, intent(out) :: errc
    select type (uptr => this%value)
    type is (character(*))
      value = uptr
      errc = .false.
    class default
      errc = .true.
    end select
  end subroutine get_c1
  
end module parameter_entry_type

#ifdef UNITTEST
program test

  use parameter_entry_type
  implicit none
  
  call run
  
contains

  subroutine run
  
  type(any_scalar) :: x
  double precision :: r
  character(len=:), allocatable :: c
  
  
  
  call x%set_value (1.0d0)
  call x%get_value (r)
  print *, r
  call x%set_value ('foo')
  call x%get_value (c)
  print *, c
  
  end subroutine
  
end program test
#endif
