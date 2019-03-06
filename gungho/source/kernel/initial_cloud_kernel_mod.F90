!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!> @brief Kernel computes the initial cloud field

!> @detail Computes initial cloud fields and rh_crit
!>         in the same space as that of theta

module initial_cloud_kernel_mod

    use argument_mod,                  only: arg_type,  &
        GH_FIELD, GH_READWRITE,CELLS
    use fs_continuity_mod,             only : Wtheta
    use constants_mod,                 only: r_def, i_def
    use kernel_mod,                    only: kernel_type

    !physics routines
    use physics_config_mod,          only :cloud_scheme, &
                                           physics_cloud_scheme_smith
    use cloud_um_config_mod,         only :rh_crit

    implicit none

    !> Kernel metadata for Psyclone 
    type, public, extends(kernel_type) :: initial_cloud_kernel_type
        private
        type(arg_type) :: meta_args(5) = (/              &
            arg_type(GH_FIELD,   GH_READWRITE,  WTHETA), &
            arg_type(GH_FIELD,   GH_READWRITE,  WTHETA), &
            arg_type(GH_FIELD,   GH_READWRITE,  WTHETA), &
            arg_type(GH_FIELD,   GH_READWRITE,  WTHETA), &
            arg_type(GH_FIELD,   GH_READWRITE,  WTHETA)  &
            /)
        integer :: iterates_over = CELLS

    contains
        procedure, nopass :: initial_cloud_code
    end type

    ! overload the default structure constructor for function space
    interface initial_cloud_kernel_type
        module procedure initial_cloud_kernel_constructor
    end interface

    public initial_cloud_code
contains

    type(initial_cloud_kernel_type) function initial_cloud_kernel_constructor() result(self)
        return
    end function initial_cloud_kernel_constructor

    !> @brief Implements the cloud field initialisation kernel 
    !! @param[in]     nlayers       The number of layers
    !! @param[in,out] cf_area       Area cloud fraction
    !! @param[in,out] cf_ice        Ice cloud fraction
    !! @param[in,out] cf_liq        Liquid cloud fraction
    !! @param[in,out] cf_bulk       Combined cloud fraction
    !! @param[in,out] rh_crit_wth   Critical relative humidity
    !! @param[in] ndf_wth Number of degrees of freedom per cell for wtheta
    !! @param[in] undf_wth Number of total degrees of freedom for wtheta
    !! @param[in] map_wth Dofmap for the cell at the base of the column
    subroutine initial_cloud_code(nlayers,       &
                                  cf_area,       &
                                  cf_ice,        &
                                  cf_liq,        &
                                  cf_bulk,       &
                                  rh_crit_wth,   &
                                  ndf_wth,       &
                                  undf_wth,      &
                                  map_wth)

        implicit none

        !Arguments
        integer(kind=i_def), intent(in)     :: nlayers
        integer(kind=i_def), intent(in)     :: ndf_wth
        integer(kind=i_def), intent(in)     :: undf_wth
        integer(kind=i_def), intent(in),    dimension(ndf_wth)  :: map_wth
        real(kind=r_def),    intent(inout), dimension(undf_wth) :: cf_area, cf_ice, &
                                                                   cf_liq, cf_bulk, &
                                                                   rh_crit_wth

        !Internal variables
        integer(kind=i_def)                 :: k, df

        ! compute the pointwise cloud profile
        ! should probably use builtins for this, but we leave the routine
        ! for now in case we want to initialise a more elaborate profile
        do k = 0, nlayers
          cf_area(map_wth(1) + k) = 0.0_r_def
          cf_ice(map_wth(1) + k)  = 0.0_r_def
          cf_liq(map_wth(1) + k)  = 0.0_r_def
          cf_bulk(map_wth(1) + k) = 0.0_r_def
        end do
        if (cloud_scheme == physics_cloud_scheme_smith) then
          ! can only initialise rh_crit if the cloud namelist has been read
          rh_crit_wth(map_wth(1)) = rh_crit(1)
          do k = 1, nlayers
            rh_crit_wth(map_wth(1) + k) = rh_crit(k)
          end do
        end if

    end subroutine initial_cloud_code

end module initial_cloud_kernel_mod
