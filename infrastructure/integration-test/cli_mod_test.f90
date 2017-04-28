!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

program cli_mod_test

  use iso_fortran_env, only : output_unit
  use cli_mod,         only : get_initial_filename

  implicit none

  character(:), allocatable :: filename

  call get_initial_filename( filename )
  write( output_unit, '(A)' ) filename

end program cli_mod_test
