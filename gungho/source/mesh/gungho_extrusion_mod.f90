!-----------------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!>
!> @brief Sets up vertical extrusion of mesh
!> @details This code contains two functions, create_extrusion() and
!>          create_shifted_extrusion(). The function create_extrusion() generates
!>          the standard vertical extrusion with different options, including
!>          uniform, quadratic, geometric and dcmip spacing.
!>          create_shifted_extrusion() creates a vertical extrusion with the same
!>          options as create_extrusion() but the top and bottom layers are
!>          half the normal cell height.
!>          There are technical infrastructure limitations which mean two different
!>          functions have been used to create the normal vertical mesh and the shifted
!>          vertical mesh. Tickets #1645 and #1659 deal with this issue of multiple
!>          instances of the mesh with different vertical extrusion.

module gungho_extrusion_mod

  use base_mesh_config_mod, only : geometry,          &
                                   key_from_geometry, &
                                   geometry_planar,   &
                                   geometry_spherical
  use constants_mod,        only : r_def, i_def
  use extrusion_mod,        only : extrusion_type,             &
                                   uniform_extrusion_type,     &
                                   quadratic_extrusion_type,   &
                                   geometric_extrusion_type,   &
                                   shifted_extrusion_type,     &
                                   double_level_extrusion_type
  use extrusion_config_mod, only : method,                     &
                                   key_from_method,            &
                                   method_uniform,             &
                                   method_quadratic,           &
                                   method_geometric,           &
                                   method_dcmip,               &
                                   method_um_L38_29t_9s_40km,  &
                                   method_um_L85_50t_35s_85km, &
                                   method_um_L70_50t_20s_80km, &
                                   domain_top,                 &
                                   number_of_layers
  use log_mod,              only : log_event,       &
                                   log_level_error, &
                                   log_scratch_space
  use planet_config_mod,    only : scaled_radius

  implicit none

  private
  public create_extrusion, create_shifted_extrusion, create_double_level_extrusion

  character(*), parameter :: module_name = 'gungho_extrusion_mod'

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes with specific UM configuration L38_29t_9s_40km
  !>
  type, public, extends(extrusion_type) :: um_L38_29t_9s_40km_extrusion_type
    private
  contains
    private
    procedure, public :: extrude => um_L38_29t_9s_40km_extrude
  end type um_L38_29t_9s_40km_extrusion_type

  interface um_L38_29t_9s_40km_extrusion_type
    module procedure um_L38_29t_9s_40km_extrusion_constructor
  end interface um_L38_29t_9s_40km_extrusion_type

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes with specific UM configuration L70_50t_20s_80km
  !>
  type, public, extends(extrusion_type) :: um_L70_50t_20s_80km_extrusion_type
    private
  contains
    private
    procedure, public :: extrude => um_L70_50t_20s_80km_extrude
  end type um_L70_50t_20s_80km_extrusion_type

  interface um_L70_50t_20s_80km_extrusion_type
    module procedure um_L70_50t_20s_80km_extrusion_constructor
  end interface um_L70_50t_20s_80km_extrusion_type

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes with specific UM configuration L85_50t_35s_85km
  !>
  type, public, extends(extrusion_type) :: um_L85_50t_35s_85km_extrusion_type
    private
  contains
    private
    procedure, public :: extrude => um_L85_50t_35s_85km_extrude
  end type um_L85_50t_35s_85km_extrusion_type

  interface um_L85_50t_35s_85km_extrusion_type
    module procedure um_L85_50t_35s_85km_extrusion_constructor
  end interface um_L85_50t_35s_85km_extrusion_type

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes using DCMIP scheme.
  !>
  type, public, extends(extrusion_type) :: dcmip_extrusion_type
    private
  contains
    private
    procedure, public :: extrude => dcmip_extrude
  end type dcmip_extrusion_type

  interface dcmip_extrusion_type
    module procedure dcmip_extrusion_constructor
  end interface dcmip_extrusion_type

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Creates a um_L38_29t_9s_40km_extrusion_type object.
  !>
  !> @param[in] atmosphere_bottom Bottom of the atmosphere in meters.
  !> @param[in] atmosphere_top Top of the atmosphere in meters.
  !> @param[in] number_of_layers Number of layers in the atmosphere.
  !>
  !> @return New uniform_extrusion_type object.
  !>
  function um_L38_29t_9s_40km_extrusion_constructor( atmosphere_bottom, &
                                                     atmosphere_top,    &
                                                     number_of_layers ) result(new)

    implicit none

    real(r_def),    intent(in) :: atmosphere_bottom
    real(r_def),    intent(in) :: atmosphere_top
    integer(i_def), intent(in) :: number_of_layers

    type(um_L38_29t_9s_40km_extrusion_type) :: new

    call new%extrusion_constructor( atmosphere_bottom, atmosphere_top, &
                                    number_of_layers )

  end function um_L38_29t_9s_40km_extrusion_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes the mesh with specific UM configuration L38_29t_9s_40km
  !>
  !> @param[out] eta Nondimensional vertical coordinate.
  !>
  subroutine um_L38_29t_9s_40km_extrude( this, eta )

    implicit none

    class(um_L38_29t_9s_40km_extrusion_type), intent(in)  :: this
    real(r_def),                   intent(out) :: eta(0:)

    integer(i_def) :: k

    if (this%get_number_of_layers() /= 38)then
      call log_event( "Extrusion L38_29t_9s_40km reqires 38 levels", log_level_error )
    end if

    do k = 0, this%get_number_of_layers()
      eta(k) = um_L38_29t_9s_40km_func(k)
    end do

  end subroutine um_L38_29t_9s_40km_extrude

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Creates a um_L85_50t_35s_85km_extrusion_type object.
  !>
  !> @param[in] atmosphere_bottom Bottom of the atmosphere in meters.
  !> @param[in] atmosphere_top Top of the atmosphere in meters.
  !> @param[in] number_of_layers Number of layers in the atmosphere.
  !>
  !> @return New uniform_extrusion_type object.
  !>
  function um_L85_50t_35s_85km_extrusion_constructor( atmosphere_bottom, &
                                                     atmosphere_top,    &
                                                     number_of_layers ) result(new)

    implicit none

    real(r_def),    intent(in) :: atmosphere_bottom
    real(r_def),    intent(in) :: atmosphere_top
    integer(i_def), intent(in) :: number_of_layers

    type(um_L85_50t_35s_85km_extrusion_type) :: new

    call new%extrusion_constructor( atmosphere_bottom, atmosphere_top, &
                                    number_of_layers )

  end function um_L85_50t_35s_85km_extrusion_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes the mesh with specific UM configuration L85_50t_35s_85km
  !>
  !> @param[out] eta Nondimensional vertical coordinate.
  !>
  subroutine um_L85_50t_35s_85km_extrude( this, eta )

    implicit none

    class(um_L85_50t_35s_85km_extrusion_type), intent(in)  :: this
    real(r_def),                   intent(out) :: eta(0:)

    if (this%get_number_of_layers() /= 85)then
      call log_event( "Extrusion L85_50t_35s_85km reqires 85 levels", log_level_error )
    end if

    eta(0:this%get_number_of_layers()) = (/ &
       0.0000000E+00,   0.2352941E-03,   0.6274510E-03,   0.1176471E-02,   0.1882353E-02, &
       0.2745098E-02,   0.3764706E-02,   0.4941176E-02,   0.6274510E-02,   0.7764705E-02, &
       0.9411764E-02,   0.1121569E-01,   0.1317647E-01,   0.1529412E-01,   0.1756863E-01, &
       0.2000000E-01,   0.2258823E-01,   0.2533333E-01,   0.2823529E-01,   0.3129411E-01, &
       0.3450980E-01,   0.3788235E-01,   0.4141176E-01,   0.4509804E-01,   0.4894118E-01, &
       0.5294117E-01,   0.5709804E-01,   0.6141176E-01,   0.6588235E-01,   0.7050980E-01, &
       0.7529411E-01,   0.8023529E-01,   0.8533333E-01,   0.9058823E-01,   0.9600001E-01, &
       0.1015687E+00,   0.1072942E+00,   0.1131767E+00,   0.1192161E+00,   0.1254127E+00, &
       0.1317666E+00,   0.1382781E+00,   0.1449476E+00,   0.1517757E+00,   0.1587633E+00, &
       0.1659115E+00,   0.1732221E+00,   0.1806969E+00,   0.1883390E+00,   0.1961518E+00, &
       0.2041400E+00,   0.2123093E+00,   0.2206671E+00,   0.2292222E+00,   0.2379856E+00, &
       0.2469709E+00,   0.2561942E+00,   0.2656752E+00,   0.2754372E+00,   0.2855080E+00, &
       0.2959203E+00,   0.3067128E+00,   0.3179307E+00,   0.3296266E+00,   0.3418615E+00, &
       0.3547061E+00,   0.3682416E+00,   0.3825613E+00,   0.3977717E+00,   0.4139944E+00, &
       0.4313675E+00,   0.4500474E+00,   0.4702109E+00,   0.4920571E+00,   0.5158098E+00, &
       0.5417201E+00,   0.5700686E+00,   0.6011688E+00,   0.6353697E+00,   0.6730590E+00, &
       0.7146671E+00,   0.7606701E+00,   0.8115944E+00,   0.8680208E+00,   0.9305884E+00, &
       0.1000000E+01 &
       /)

  end subroutine um_L85_50t_35s_85km_extrude

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Creates a um_L70_50t_20s_80km_extrusion_type object.
  !>
  !> @param[in] atmosphere_bottom Bottom of the atmosphere in meters.
  !> @param[in] atmosphere_top Top of the atmosphere in meters.
  !> @param[in] number_of_layers Number of layers in the atmosphere.
  !>
  !> @return New uniform_extrusion_type object.
  !>
  function um_L70_50t_20s_80km_extrusion_constructor( atmosphere_bottom, &
                                                     atmosphere_top,    &
                                                     number_of_layers ) result(new)

    implicit none

    real(r_def),    intent(in) :: atmosphere_bottom
    real(r_def),    intent(in) :: atmosphere_top
    integer(i_def), intent(in) :: number_of_layers

    type(um_L70_50t_20s_80km_extrusion_type) :: new

    call new%extrusion_constructor( atmosphere_bottom, atmosphere_top, &
                                    number_of_layers )

  end function um_L70_50t_20s_80km_extrusion_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes the mesh with specific UM configuration L70_50t_20s_80km
  !>
  !> @param[out] eta Nondimensional vertical coordinate.
  !>
  subroutine um_L70_50t_20s_80km_extrude( this, eta )

    implicit none

    class(um_L70_50t_20s_80km_extrusion_type), intent(in)  :: this
    real(r_def),                   intent(out) :: eta(0:)


    if (this%get_number_of_layers() /= 70)then
      call log_event( "Extrusion L70_50t_20s_80km reqires 70 levels", log_level_error )
    end if

    eta(0:this%get_number_of_layers()) = (/  &
       .0000000,  .0002500,  .0006667,  .0012500,  .0020000, &
       .0029167,  .0040000,  .0052500,  .0066667,  .0082500, &
       .0100000,  .0119167,  .0140000,  .0162500,  .0186667, &
       .0212500,  .0240000,  .0269167,  .0300000,  .0332500, &
       .0366667,  .0402500,  .0440000,  .0479167,  .0520000, &
       .0562500,  .0606667,  .0652500,  .0700000,  .0749167, &
       .0800000,  .0852500,  .0906668,  .0962505,  .1020017, &
       .1079213,  .1140113,  .1202745,  .1267154,  .1333406, &
       .1401592,  .1471838,  .1544313,  .1619238,  .1696895, &
       .1777643,  .1861929,  .1950307,  .2043451,  .2142178, &
       .2247466,  .2360480,  .2482597,  .2615432,  .2760868, &
       .2921094,  .3098631,  .3296378,  .3517651,  .3766222, &
       .4046373,  .4362943,  .4721379,  .5127798,  .5589045, &
       .6112759,  .6707432,  .7382500,  .8148403,  .9016668, &
       1.0000000 &
       /)

  end subroutine um_L70_50t_20s_80km_extrude

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Creates a dcmip_extrusion_type object.
  !>
  !> @param[in] atmosphere_bottom Bottom of the atmosphere in meters.
  !> @param[in] atmosphere_top Top of the atmosphere in meters.
  !> @param[in] number_of_layers Number of layers in the atmosphere.
  !>
  !> @return New dcmip_extrusion_type object.
  !>
  function dcmip_extrusion_constructor( atmosphere_bottom, &
                                        atmosphere_top,    &
                                        number_of_layers ) result(new)

    implicit none

    real(r_def),    intent(in) :: atmosphere_bottom
    real(r_def),    intent(in) :: atmosphere_top
    integer(i_def), intent(in) :: number_of_layers

    type(dcmip_extrusion_type) :: new

    call new%extrusion_constructor( atmosphere_bottom, atmosphere_top, &
                                    number_of_layers )

  end function dcmip_extrusion_constructor

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Extrudes the mesh using the DCMIP scheme.
  !>
  !> For more information see DCMIP-TestCaseDocument_v1.7.pdf,
  !> Appendix F.2. - Eq. 229.
  !>
  !> @param[out] eta Nondimensional vertical coordinate.
  !>
  subroutine dcmip_extrude( this, eta )

    implicit none

    class(dcmip_extrusion_type), intent(in)  :: this
    real(r_def),                 intent(out) :: eta(0:)

    real(r_def), parameter :: phi_flatten = 15.0_r_def

    integer(i_def) :: k

    do k = 0, this%get_number_of_layers()
      eta(k) = dcmip_func(real(k,r_def)/real(this%get_number_of_layers(),r_def))
    end do

  end subroutine dcmip_extrude

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Helper function for generating um_L38_29t_9s_40km extrusion
  !>
  !> @param[in] i         index for level
  !> @return    eta       Vertical eta coordinate
  !>
  function um_L38_29t_9s_40km_func(i) result(eta)
    implicit none

    integer(i_def) :: i
    real(r_def) :: eta
    real(r_def) :: um_eta(0:38) = (/ 0.0, &
        .0005095,  .0020380,  .0045854,  .0081519,  .0127373, &
        .0183417,  .0249651,  .0326074,  .0412688,  .0509491, &
        .0616485,  .0733668,  .0861040,  .0998603,  .1146356, &
        .1304298,  .1472430,  .1650752,  .1839264,  .2037966, &
        .2246857,  .2465938,  .2695209,  .2934670,  .3184321, &
        .3444162,  .3714396,  .3998142,  .4298913,  .4620737, &
        .4968308,  .5347160,  .5763897,  .6230643,  .6772068, &
        .7443435,  .8383348, 1.0000000 /)

    eta = um_eta(i)

  end function um_L38_29t_9s_40km_func

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Helper function for generating DCMIP extrusion
  !>
  !> @param[in] eta_uni   Input value which increases incrementally with level number
  !> @return    eta       Vertical eta coordinate
  !>
  function dcmip_func(eta_uni) result(eta)
    implicit none

    real(r_def), intent(in) :: eta_uni
    real(r_def) :: eta

    real(r_def), parameter :: phi_flatten = 15.0_r_def

    eta = ( sqrt(phi_flatten*(eta_uni**2_i_def) + 1.0_r_def) &
                    - 1.0_r_def ) / &
                  ( sqrt(phi_flatten + 1.0_r_def) - 1.0_r_def )

  end function dcmip_func

  !> @brief Creates vertical mesh extrusion
  !> @details Creates vertical mesh with nlayers.
  !> @return new     Extrusion class
  function create_extrusion() result(new)

    implicit none

    class(extrusion_type), allocatable :: new

    real(r_def) :: atmosphere_bottom

    if (allocated(new)) deallocate(new)

    select case (geometry)
      case (geometry_planar)
        atmosphere_bottom = 0.0_r_def
      case (geometry_spherical)
        atmosphere_bottom = scaled_radius
      case default
        write( log_scratch_space,                      &
               '(A, ": Unrecognised geometry: ", A)' ) &
             module_name, key_from_geometry( geometry )
        call log_event( log_scratch_space, log_level_error )
    end select

    select case (method)
      case (method_uniform)
        allocate( new, source=uniform_extrusion_type( atmosphere_bottom, &
                                                      domain_top,        &
                                                      number_of_layers ) )
      case (method_um_L38_29t_9s_40km)
        allocate( new, source=um_L38_29t_9s_40km_extrusion_type( atmosphere_bottom, &
                                                                 domain_top,        &
                                                                 number_of_layers ) )
      case (method_um_L85_50t_35s_85km)
        allocate( new, source=um_L85_50t_35s_85km_extrusion_type( atmosphere_bottom, &
                                                                 domain_top,         &
                                                                 number_of_layers ) )
      case (method_um_L70_50t_20s_80km)
        allocate( new, source=um_L70_50t_20s_80km_extrusion_type( atmosphere_bottom, &
                                                                 domain_top,         &
                                                                 number_of_layers ) )
      case (method_quadratic)
        allocate( new, source=quadratic_extrusion_type( atmosphere_bottom, &
                                                        domain_top,        &
                                                        number_of_layers ) )
      case (method_geometric)
        allocate( new, source=geometric_extrusion_type( atmosphere_bottom, &
                                                        domain_top,        &
                                                        number_of_layers ) )
      case (method_dcmip)
        allocate( new, source=dcmip_extrusion_type( atmosphere_bottom, &
                                                    domain_top,        &
                                                    number_of_layers ) )
      case default
        write( log_scratch_space,                         &
               '(A, ": Unrecognised extrusion method: ", A)' ) &
             module_name, key_from_method( method )
        call log_event( log_scratch_space, log_level_error )
    end select

  end function create_extrusion

  !> @brief Creates vertical mesh extrusion for vertically shifted mesh.
  !> @details Creates vertically shifted mesh with nlayers+1 with the top and
  !>          bottom levels having half the cell height of the normal extrusion.
  !> @param[in] old The original extrusion.
  !> @return new     Extrusion class
  function create_shifted_extrusion(old) result(new)

    implicit none

    class(extrusion_type),  intent(in) :: old
    class(extrusion_type), allocatable :: new

    if (allocated(new)) deallocate(new)

    allocate(new, source=shifted_extrusion_type(old))

  end function create_shifted_extrusion

  !> @brief Creates vertical mesh extrusion for double level mesh.
  !> @details Creates vertically double level mesh from normal and shifted meshes
  !> @return new     Extrusion class
  function create_double_level_extrusion(old) result(new)

    implicit none

    class(extrusion_type),  intent(in) :: old
    class(extrusion_type), allocatable :: new

    if (allocated(new)) deallocate(new)

    allocate(new, source=double_level_extrusion_type(old))

  end function create_double_level_extrusion

end module gungho_extrusion_mod
