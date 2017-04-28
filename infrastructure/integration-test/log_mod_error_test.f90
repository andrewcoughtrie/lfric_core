!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

program log_mod_error_test

  use ESMF,    only : ESMF_Initialize, ESMF_Finalize
  use log_mod, only : log_event, LOG_LEVEL_ERROR

  call ESMF_Initialize()
  call log_event( 'An error was logged.', LOG_LEVEL_ERROR )
  call ESMF_Finalize()

end program log_mod_error_test
