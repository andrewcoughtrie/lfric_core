!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief Split a W2 field into the component W2v and W2h fields
!>
module split_w2_field_kernel_mod
  use argument_mod,      only : arg_type, func_type,       &
                                GH_FIELD, GH_READ, GH_INC, GH_READWRITE, &
                                ANY_SPACE_1,               &
                                GH_BASIS, GH_DIFF_BASIS,   &
                                CELLS, GH_EVALUATOR
  use constants_mod,     only : r_def, i_def
  use fs_continuity_mod, only : W2, W2h, W2v
  use kernel_mod,        only : kernel_type
  implicit none
  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------
  !> The type declaration for the kernel. Contains the metadata needed by the
  !> Psy layer.
  !>
  type, public, extends(kernel_type) :: split_w2_field_kernel_type
    private
    type(arg_type) :: meta_args(3) = (/          &
        arg_type(GH_FIELD,   GH_INC, W2h),      &
        arg_type(GH_FIELD,   GH_READWRITE, W2v),       &
        arg_type(GH_FIELD,   GH_READ,  W2)       &
        /)
    integer :: iterates_over = CELLS
  contains
    procedure, nopass ::split_w2_field_code
  end type
  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public split_w2_field_code

contains

!> @brief Kernel to sample a flux at nodal points: F = u*q
!! @param[in] nlayers Number of layers
!! @param[in] ndf_f Number of degrees of freedom per cell for w2
!! @param[in] undf_f Number of unique degrees of freedom for w2
!! @param[in] map_f Dofmap for the cell at the base of the column for w2
!! @param[inout] flux Field to contain the right hand side to be computed
!! @param[in] rmultiplicity Reciprocal of How many times the dof has been visited in total
!! @param[in] u Advecting wind
!! @param[in] ndf_q Number of degrees of freedom per cell for the field to be advected
!! @param[in] undf_q  Number of unique degrees of freedom for the advected field
!! @param[in] map_q Dofmap for the cell at the base of the column for the field to be advected
!! @param[in] basis_q Basis functions evaluated at gaussian quadrature points
!! @param[in] q Advected field
subroutine split_w2_field_code(nlayers,                    &
                               uv, w, uvw,                 &
                               ndf_w2h, undf_w2h, map_w2h, &
                               ndf_w2v, undf_w2v, map_w2v, &
                               ndf_w2,  undf_w2,  map_w2 )
  implicit none
  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers
  integer(kind=i_def), intent(in) :: ndf_w2, ndf_w2h, ndf_w2v
  integer(kind=i_def), intent(in) :: undf_w2, undf_w2h, undf_w2v
  integer(kind=i_def), dimension(ndf_w2),  intent(in) :: map_w2
  integer(kind=i_def), dimension(ndf_w2h), intent(in) :: map_w2h
  integer(kind=i_def), dimension(ndf_w2v), intent(in) :: map_w2v
  real(kind=r_def), dimension(undf_w2h), intent(inout)    :: uv
  real(kind=r_def), dimension(undf_w2v), intent(inout)    :: w
  real(kind=r_def), dimension(undf_w2),  intent(in) :: uvw

  ! Internal variables
  integer(kind=i_def) :: df, k
  do k = 0, nlayers-1
    do df = 1,4
      uv(map_w2h(df) + k) = uvw(map_w2(df) + k)
    end do
    do df = 1,2
      w(map_w2v(df) + k) = uvw(map_w2(4+df) + k)
    end do
  end do
end subroutine split_w2_field_code
end module split_w2_field_kernel_mod
