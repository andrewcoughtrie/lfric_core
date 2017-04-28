!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!> @brief Define indices for the prognostic field vectors
!>
!> @details Define and set indices for the prognostic field vectors
module field_indices_mod

  implicit none

  ! For gungho model
  integer, parameter :: igh_u = 1  ! wind
  integer, parameter :: igh_t = 2  ! potential temperature
  integer, parameter :: igh_d = 3  ! density
  integer, parameter :: igh_p = 4  ! exner pressure

  ! For gravity wave mini app
  integer, parameter :: igw_u = 1  ! wind
  integer, parameter :: igw_p = 2  ! pressure
  integer, parameter :: igw_b = 3  ! buoyancy

end module field_indices_mod
