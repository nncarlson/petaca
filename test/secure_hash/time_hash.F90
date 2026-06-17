program main

  use secure_hash_factory
  implicit none
  
  class(secure_hash), allocatable :: h
  real :: array (1024*1024), cpu0, cpu
  integer :: n
  
  call new_secure_hash (h, 'sha1')
  
  !call random_number (array)
  array = 0.0
  
  call cpu_time (cpu0)
  do n = 1, 128
    call h%update (array)
  end do
  call cpu_time (cpu)
  print *, cpu-cpu0, h%hexdigest()
  
end program
