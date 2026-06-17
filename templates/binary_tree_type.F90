module binary_tree_type

  type binary_tree
    type(binary_tree_node), pointer :: root => null()
  end type
  
  type :: binary_tree_node
    character(:), allocatable :: key
    type(binary_tree_node), pointer :: parent => null()
    type(binary_tree_node), pointer :: left   => null()
    type(binary_tree_node), pointer :: right  => null()
  end type
  
contains

  subroutine insert (this, key)
    
  end subroutine insert
  

  
end module binary_tree_type
