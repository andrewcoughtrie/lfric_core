!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!> @brief Defines various constants.

!> @details Various physical and geometrical constants are defined in this module.
!! Their values are also set here.
module constants_mod
implicit none

!Working precision
integer,       parameter :: dp=8              !< working precision

!Numerical constants
real(kind=dp), parameter :: pi=3.141592654    !< pi value
real(kind=dp), parameter :: eps=3.0E-15_dp    !< relative precision

integer,       parameter :: max_iter = 10 ! maximum iteration number for solver

end module constants_mod

