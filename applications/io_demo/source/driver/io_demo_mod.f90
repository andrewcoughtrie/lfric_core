!-----------------------------------------------------------------------------
! (c) Crown copyright 2017 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> io_demo knows what configuration it needs.
!>
module io_demo_mod

  implicit none

  private

  character(*), public, parameter ::                                &
      io_demo_required_namelists(5) =  [ 'base_mesh     ', &
                                                  'extrusion     ', &
                                                  'finite_element', &
                                                  'partitioning  ', &
                                                  'planet        ']

end module io_demo_mod
