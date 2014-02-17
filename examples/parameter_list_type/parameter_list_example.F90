program parameter_list_example

  use parameter_list_type
  
  type(parameter_list) :: plist
  type(parameter_list), pointer :: sublist
  type(parameter_list_iterator) :: iter
  integer :: p
  character(:), allocatable :: f
  class(*), allocatable :: origin
  
  type point
    real x, y
  end type
  
  !! Parameter lists come into existence well-defined and empty.
  !! Define some parameters; note the different types and ranks.
  call plist%set ('page', 3)
  call plist%set ('size', 1.4)
  call plist%set ('color', 'blue')
  call plist%set ('boundingbox', [10, 10, 90, 90])
  call plist%set ('crop', .true.)
  
  !! Replace an existing parameter value with a different value of different type.
  call plist%set ('size', 'default')
  
  !! Retrieve a specific parameter value; its type must match p.
  call plist%get ('page', p)
  
  !! Retrieve a parameter value that doesn't exist;
  !! it is created with the specified default value.
  call plist%get ('font', f, default='courier')
  
  !! Create a sublist parameter named 'picture'.
  sublist => plist%sublist('picture')
  
  !! Define a parameter in the sublist; note the derived-type value.
  call sublist%set ('origin', point(1.0,2.0))
  
  !! Now retrieve the derived type value
  call sublist%get ('origin', origin)
  select type (origin)
  type is (point)
    print *, origin%x, origin%y
  end select
  
  !! Walk
  iter = parameter_list_iterator(plist)
  do while (.not.piter%at_end())
    select case 
  end do
  
  
