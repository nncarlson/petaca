program test

  use parameter_list_type
  implicit none
  
  call run()
  call test_iter()
  
contains

  subroutine run ()
  
  type(parameter_list), pointer :: params, bdf2_params, foo_params
  
  integer :: stat, n
  character(:), allocatable :: errmsg, bc_type
  double precision :: x
  logical :: enable
  double precision, allocatable :: axis(:)
  integer, allocatable :: array(:)
  
  type :: my_type
    integer :: n
  end type
  type(my_type) :: obj
  
  allocate(params)
  call params%set ("number of nodes", 100)
  call params%set ('type', 'dirichlet')
  bdf2_params => params%sublist("BDF2")
  call bdf2_params%set ("Max_AIN_Itr", 5)
  call bdf2_params%set ("Max_AIN_Vec", 4)
  call bdf2_params%set ("AIN_tol", 0.1d0)
  call bdf2_params%set ("AIN_tol", 0.05d0)
  foo_params => bdf2_params%sublist("foo params")
  call foo_params%set ("stuff", 1.0d0)
  !call foo_params%get ("stuff", n)
  call foo_params%set ("axis", [1, 0, 0])
  !call foo_params%get ("axis", axis)
  !print *, axis
  call foo_params%set ("explicit", .true.)
  call foo_params%set ("shift", [1.0d0, 2.0d0])
  call foo_params%set ("enable", [.true., .false., .false.])
  allocate(array(0))
  call foo_params%set ("empty", array)
  call foo_params%set ("junk", obj)
 
  call params%print(6,'')
  
  deallocate(params)
  end subroutine
  
  subroutine test_iter ()
  
    type(parameter_list) :: toplist
    type(parameter_list), pointer :: sublist
    type(parameter_list_iterator) :: piter
    
    call toplist%set ('p1', 0)
    call toplist%set ('p2', 0)
    sublist => toplist%sublist ('l1')
    call toplist%set ('p3', 0)
    sublist => toplist%sublist ('l2')
    
    !call piter%init (toplist)!, sublists_only=.true.)
    piter = parameter_list_iterator(toplist)!, sublists_only=.true.)
    print *, 'number of parameter list items =', piter%count()
    do while (.not.piter%at_end())
      print *, piter%name(), '; is a list =', piter%is_list()
      call piter%next()
    end do
    
    sublist => toplist%sublist ('l1')
    !call piter%init (sublist)
    piter = parameter_list_iterator(sublist)
    print *, 'number of sublist items =', piter%count()
    do while (.not.piter%at_end())
      print *, piter%name(), '; is a list =', piter%is_list()
      call piter%next()
    end do
    
  end subroutine
  
end program test
