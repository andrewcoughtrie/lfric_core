!-----------------------------------------------------------------------------
! (C) Crown copyright 2025 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief Extracts one data layer from a field

module sci_level_extract_kernel_mod

  use argument_mod,            only: arg_type, GH_INTEGER,                    &
                                     GH_FIELD, GH_REAL, GH_SCALAR,            &
                                     GH_READ, GH_WRITE, CELL_COLUMN,          &
                                     ANY_DISCONTINUOUS_SPACE_1,               &
                                     ANY_DISCONTINUOUS_SPACE_2
  use constants_mod,           only: i_def, r_def
  use kernel_mod,              only: kernel_type

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------
  !> The type declaration for the kernel. Contains the metadata needed by the
  !> Psy layer.
  !>
  type, public, extends(kernel_type) :: level_extract_kernel_type
    private
    type(arg_type) :: meta_args(4) = (/                                       &
         arg_type(GH_FIELD,  GH_REAL,    GH_WRITE, ANY_DISCONTINUOUS_SPACE_1),&
         arg_type(GH_FIELD,  GH_REAL,    GH_READ,  ANY_DISCONTINUOUS_SPACE_2),&
         arg_type(GH_SCALAR, GH_INTEGER, GH_READ),                            &
         arg_type(GH_SCALAR, GH_INTEGER, GH_READ)                             &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, nopass :: level_extract_code
  end type level_extract_kernel_type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------

  public :: level_extract_code

contains

  !> @brief Extracts one layer and one data from a field
  !> @param[in]     nlayers                  Number of layers in a model column
  !!                                         in the fine grid
  !> @param[out]    field_dst                Selected 2D layer field
  !> @param[in]     field_src                Field from which extract layer
  !> @param[in]     layer                    Layer to extract from source field
  !> @param[in]     multi                    Data from the multidata source
  !!                                         field to extract from source field
  !> @param[in]     ndf_dst                  Num of DoFs per cell on 2D mesh
  !> @param[in]     undf_dst                 Total num of DoFs on 2D mesh
  !> @param[in]     map_dst                  DoFmap of cells on 2D mesh
  !> @param[in]     ndf_src                  Num of DoFs per cell on source mesh
  !> @param[in]     undf_src                 Total num of DoFs on source mesh
  !> @param[in]     map_src                  DoFmap of cells on source mesh
  subroutine level_extract_code( nlayers,                  &
                                 field_dst,                &
                                 field_src,                &
                                 layer,                    &
                                 multi,                    &
                                 ndf_dst,                  &
                                 undf_dst,                 &
                                 map_dst,                  &
                                 ndf_src,                  &
                                 undf_src,                 &
                                 map_src                   )

    implicit none

    integer(kind=i_def), intent(in)   :: nlayers
    integer(kind=i_def), intent(in)   :: ndf_dst, ndf_src
    integer(kind=i_def), intent(in)   :: undf_dst, undf_src

    ! Fields
    real(kind=r_def),    intent(out)  :: field_dst(undf_dst)
    real(kind=r_def),    intent(in)   :: field_src(undf_src)
    integer(kind=i_def), intent(in)   :: layer
    integer(kind=i_def), intent(in)   :: multi

    ! Maps
    integer(kind=i_def), intent(in)   :: map_dst(ndf_dst)
    integer(kind=i_def), intent(in)   :: map_src(ndf_src)

    ! Internal arguments
    ! Total number of layers, depends on function space and nlayers
    integer(kind=i_def) :: tlayers
    integer(kind=i_def) :: df

    ! Assume lowest-order, so only 1 dof per cell
    df = 1
    ! Find layers
    tlayers = nlayers + ndf_src - 1

    field_dst(map_dst(df)) = field_src(map_src(df)+multi*tlayers+layer)

  end subroutine level_extract_code

end module sci_level_extract_kernel_mod
