!-----------------------------------------------------------------------------
! (C) Crown copyright 2025 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief Copies one data layer to a field

module sci_level_insert_kernel_mod

  use argument_mod,            only: arg_type, func_type,                     &
                                     GH_FIELD, GH_REAL, GH_INTEGER, GH_SCALAR,&
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
  type, public, extends(kernel_type) :: level_insert_kernel_type
    private
    type(arg_type) :: meta_args(4) = (/                                       &
         arg_type(GH_FIELD,  GH_REAL,    GH_WRITE, ANY_DISCONTINUOUS_SPACE_1),&
         arg_type(GH_FIELD,  GH_REAL,    GH_READ,  ANY_DISCONTINUOUS_SPACE_2),&
         arg_type(GH_SCALAR, GH_INTEGER, GH_READ),                            &
         arg_type(GH_SCALAR, GH_INTEGER, GH_READ)                             &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, nopass :: level_insert_code
  end type level_insert_kernel_type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------

  public :: level_insert_code

contains

  !> @brief Inserts one layer and one data in a field
  !> @param[in]     nlayers                  Number of layers in a model column
  !!                                         in the fine grid
  !> @param[out]    field_dst                Field to which copy layer
  !> @param[in]     field_src                2D layer field to be copied
  !> @param[in]     layer                    Layer to copied from source field
  !> @param[in]     multi                    Data to copy from the source field
  !!                                         to the multidata destination field
  !> @param[in]     ndf_dst                  Num of DoFs per cell on the
  !!                                         destination mesh
  !> @param[in]     undf_dst                 Total num of DoFs on the
  !!                                         destination  mesh
  !> @param[in]     map_dst                  DoFmap of cells on the
  !!                                         destination mesh
  !> @param[in]     ndf_src                  Num of DoFs per cell on 2D mesh
  !> @param[in]     undf_src                 Total num of DoFs on 2D mesh
  !> @param[in]     map_src                  DoFmap of cells on 2D mesh
  subroutine level_insert_code( nlayers,                   &
                                field_dst,                 &
                                field_src,                 &
                                layer,                     &
                                multi,                     &
                                ndf_dst,                   &
                                undf_dst,                  &
                                map_dst,                   &
                                ndf_src,                   &
                                undf_src,                  &
                                map_src                    )

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

    field_dst(map_dst(df)+multi*tlayers+layer) = field_src(map_src(df))

  end subroutine level_insert_code

end module sci_level_insert_kernel_mod
