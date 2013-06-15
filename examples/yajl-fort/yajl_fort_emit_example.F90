program yajl_fort_emit_example

  use yajl_fort
  
  type(fyajl_emitter) :: emitter
  character(:), pointer :: buffer
  
  call emitter%init
  
  call emitter%emit_map_open
  call emitter%emit_map_key ('foo')
  call emitter%emit_value (1.1d0)
  call emitter%emit_map_key ('bar')
  call emitter%emit_value ('albatross')
  call emitter%emit_map_key ('dog')
  call emitter%emit_map_open
  
  call emitter%get_buffer (buffer)
  print *, buffer
  call emitter%clear_buffer

  call emitter%emit_map_key ('wild')
  call emitter%emit_value (.true.)
  call emitter%emit_array_close
  call emitter%emit_map_close
  call emitter%emit_map_key ('red')
  call emitter%emit_array_open
  call emitter%emit_value (0.5d0)
  call emitter%emit_value (0.5d0)
  call emitter%emit_value (0.1d0)
  call emitter%emit_array_close
  call emitter%emit_map_close
  
  call emitter%get_buffer (buffer)
  print *, buffer
  call emitter%clear_buffer
  
end program
