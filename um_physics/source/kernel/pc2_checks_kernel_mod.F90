!-----------------------------------------------------------------------------
! (c) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Interface to cloud scheme.

module pc2_checks_kernel_mod

use argument_mod, only: arg_type,                     &
                        GH_FIELD, GH_READ, GH_WRITE,  &
                        GH_READWRITE, CELLS
use fs_continuity_mod, only: WTHETA

use kernel_mod,   only: kernel_type

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer

type, public, extends(kernel_type) :: pc2_checks_kernel_type
  private
  type(arg_type) :: meta_args(15) = (/                                 &
       arg_type(GH_FIELD,   GH_READ,    WTHETA), & ! mv_wth
       arg_type(GH_FIELD,   GH_READ,    WTHETA), & ! ml_wth
       arg_type(GH_FIELD,   GH_READ,    WTHETA), & ! mi_wth
       arg_type(GH_FIELD,   GH_READ,    WTHETA), & ! cfl_wth
       arg_type(GH_FIELD,   GH_READ,    WTHETA), & ! cff_wth
       arg_type(GH_FIELD,   GH_READ,    WTHETA), & ! bcf_wth
       arg_type(GH_FIELD,   GH_READ,    WTHETA), & ! theta_wth
       arg_type(GH_FIELD,   GH_READ,    WTHETA), & ! exner_wth
       arg_type(GH_FIELD,   GH_WRITE,   WTHETA), & ! dtheta_response
       arg_type(GH_FIELD,   GH_WRITE,   WTHETA), & ! dqv_response_wth
       arg_type(GH_FIELD,   GH_WRITE,   WTHETA), & ! dqcl_response_wth
       arg_type(GH_FIELD,   GH_WRITE,   WTHETA), & ! dqcf_response_wth
       arg_type(GH_FIELD,   GH_WRITE,   WTHETA), & ! dcfl_response_wth
       arg_type(GH_FIELD,   GH_WRITE,   WTHETA), & ! dcff_response_wth
       arg_type(GH_FIELD,   GH_WRITE,   WTHETA)  & ! dbcf_response_wth
       /)
   integer :: iterates_over = CELLS
contains
  procedure, nopass :: pc2_checks_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! Overload the default structure constructor for function space
interface pc2_checks_kernel_type
   module procedure pc2_checks_kernel_constructor
end interface

public pc2_checks_code
contains

type(pc2_checks_kernel_type) function pc2_checks_kernel_constructor() result(self)
  return
end function pc2_checks_kernel_constructor

!> @brief Interface to the pc2 checks code
!> @param[in]  nlayers              Number of layers
!> @param[in]  mv_wth               vapour mass mixing ratio
!> @param[in]  ml_wth               liquid cloud mass mixing ratio
!> @param[in]  mi_wth               liquid cloud mass mixing ratio
!> @param[in]  cfl_wth              liquid cloud fraction
!> @param[in]  cff_wth              ice cloud fraction
!> @param[in]  bcf_wth              bulk cloud fraction
!> @param[in]  theta_wth            Potential temperature field
!> @param[in]  exner_wth            Exner pressure in potential temperature space
!> @param[out] dtheta_response_wth  Change in theta
!> @param[out] dqv_response_wth     Change in water vapour
!> @param[out] dqcl_response_wth    Change in liquid water content
!> @param[out] dqcf_response_wth    Change in ice    water content
!> @param[out] dcfl_response_wth    Change in liquid cloud fraction
!> @param[out] dcff_response_wth    Change in ice    cloud fraction
!> @param[out] dbcf_response_wth    Change in bulk   cloud fraction
!> @param[in]  ndf_wth  Number of degrees of freedom per cell for potential temperature space
!> @param[in]  undf_wth Number unique of degrees of freedom  for potential temperature space
!> @param[in]  map_wth  Dofmap for the cell at the base of the column for potential temperature space

subroutine pc2_checks_code( nlayers,                   &
                            mv_wth,                    &
                            ml_wth,                    &
                            mi_wth,                    &
                            cfl_wth,                   &
                            cff_wth,                   &
                            bcf_wth,                   &
                            theta_wth,                 &
                            exner_wth,                 &
                                           ! Responses
                            dtheta_response_wth,       &
                            dqv_response_wth,          &
                            dqcl_response_wth,         &
                            dqcf_response_wth,         &
                            dcfl_response_wth,         &
                            dcff_response_wth,         &
                            dbcf_response_wth,         &
                                           ! Other
                            ndf_wth, undf_wth, map_wth )

    use constants_mod, only: r_def, i_def, r_um, i_um

    use log_mod,       only: log_event, LOG_LEVEL_ERROR

    !---------------------------------------
    ! UM modules
    !---------------------------------------

    use nlsizes_namelist_mod,       only: row_length, rows, model_levels
    use pc2_checks_mod,             only: pc2_checks
    use level_heights_mod,          only: r_rho_levels, r_theta_levels
    use planet_constants_mod,       only: p_zero, kappa, planet_radius
    use gen_phys_inputs_mod,        only: l_mr_physics
    use atm_step_local,             only: rhc_row_length, rhc_rows

    implicit none

    ! Arguments

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_wth
    integer(kind=i_def), intent(in) :: undf_wth

    real(kind=r_def), intent(in),  dimension(undf_wth) :: mv_wth
    real(kind=r_def), intent(in),  dimension(undf_wth) :: ml_wth
    real(kind=r_def), intent(in),  dimension(undf_wth) :: mi_wth
    real(kind=r_def), intent(in),  dimension(undf_wth) :: bcf_wth
    real(kind=r_def), intent(in),  dimension(undf_wth) :: cfl_wth
    real(kind=r_def), intent(in),  dimension(undf_wth) :: cff_wth
    real(kind=r_def), intent(in),  dimension(undf_wth) :: theta_wth
    real(kind=r_def), intent(in),  dimension(undf_wth) :: exner_wth

    integer(kind=i_def), intent(in), dimension(ndf_wth) :: map_wth

    ! The changes to the fields as a result
    real(kind=r_def), intent(inout), dimension(undf_wth) :: dtheta_response_wth
    real(kind=r_def), intent(inout), dimension(undf_wth) :: dqv_response_wth
    real(kind=r_def), intent(inout), dimension(undf_wth) :: dqcl_response_wth
    real(kind=r_def), intent(inout), dimension(undf_wth) :: dqcf_response_wth
    real(kind=r_def), intent(inout), dimension(undf_wth) :: dcfl_response_wth
    real(kind=r_def), intent(inout), dimension(undf_wth) :: dcff_response_wth
    real(kind=r_def), intent(inout), dimension(undf_wth) :: dbcf_response_wth

    real(r_um), dimension(row_length,rows,model_levels) :: &
                  qv_work, qcl_work, qcf_work,             &
                  cfl_work, cff_work, bcf_work,            &
                  t_work, theta_work, pressure

    real(r_um), dimension(row_length,rows,model_levels) :: &
         dtdt, dqdt, dldt, dpdt,rhcpt

    integer(i_um) :: k

    integer(i_um), parameter :: nSCMDpkgs=15
    logical, parameter :: l_scmdiags(nscmdpkgs)=.false.

    !-----------------------------------------------------------------------
    ! Initialisation of prognostic variables and arrays
    !-----------------------------------------------------------------------

    do k = 1, model_levels

      t_work(1,1,k)   = theta_wth(map_wth(1) + k) *            &
                        exner_wth(map_wth(1) + k)

      ! Pressure at centre of theta levels
      pressure(1,1,k) = p_zero*(exner_wth(map_wth(1) + k))     &
                                **(1.0_r_um/kappa)

      ! Moist prognostics
      qv_work(1,1,k)  = mv_wth(map_wth(1) + k)
      qcl_work(1,1,k) = ml_wth(map_wth(1) + k)
      qcf_work(1,1,k) = mi_wth(map_wth(1) + k)

      ! Cast LFRic cloud fractions onto cloud fraction work arrays.
      bcf_work(1,1,k) = bcf_wth(map_wth(1) + k)
      cfl_work(1,1,k) = cfl_wth(map_wth(1) + k)
      cff_work(1,1,k) = cff_wth(map_wth(1) + k)
    end do

    CALL pc2_checks( pressure,                                 &
                     t_work, bcf_work, cfl_work, cff_work,     &
                     qv_work, qcl_work, qcf_work, l_mr_physics,&
                     row_length, rows, model_levels,           &
                     0_i_um, 0_i_um, 0_i_um, 0_i_um)

    ! Recast back to LFRic space
    do k = 1, model_levels
      ! *_work arrays have been updated

      ! New theta found from new temperature.
      theta_work(1,1,k) = t_work(1,1,k) /             &
                          exner_wth(map_wth(1) + k)

      ! All increments found from difference between the updated *_work values
      ! and the values that were intent in.
      dtheta_response_wth(map_wth(1) + k) = theta_work(1,1,k) &
                                          - theta_wth(map_wth(1) + k)
      !
      dqv_response_wth (map_wth(1)+k) = qv_work (1,1,k) - mv_wth (map_wth(1) + k)
      dqcl_response_wth(map_wth(1)+k) = qcl_work(1,1,k) - ml_wth (map_wth(1) + k)
      dqcf_response_wth(map_wth(1)+k) = qcf_work(1,1,k) - mi_wth (map_wth(1) + k)
      dcfl_response_wth(map_wth(1)+k) = cfl_work(1,1,k) - cfl_wth(map_wth(1) + k)
      dcff_response_wth(map_wth(1)+k) = cff_work(1,1,k) - cff_wth(map_wth(1) + k)
      dbcf_response_wth(map_wth(1)+k) = bcf_work(1,1,k) - bcf_wth(map_wth(1) + k)
    end do

    dtheta_response_wth(map_wth(1)+0) = dtheta_response_wth(map_wth(1)+1)
    dqv_response_wth   (map_wth(1)+0) = dqv_response_wth   (map_wth(1)+1)
    dqcl_response_wth  (map_wth(1)+0) = dqcl_response_wth  (map_wth(1)+1)
    dqcf_response_wth  (map_wth(1)+0) = dqcf_response_wth  (map_wth(1)+1)
    dcfl_response_wth  (map_wth(1)+0) = dcfl_response_wth  (map_wth(1)+1)
    dcff_response_wth  (map_wth(1)+0) = dcff_response_wth  (map_wth(1)+1)
    dbcf_response_wth  (map_wth(1)+0) = dbcf_response_wth  (map_wth(1)+1)

end subroutine pc2_checks_code

end module pc2_checks_kernel_mod
