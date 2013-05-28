function square (x) result (xsq) bind (c)
  real, value :: x
  real :: xsq
  xsq = x*x
end function

module global_data
  integer, bind(c,name='FORTYTWO') :: FORTYTWO = 42
end module global_data
