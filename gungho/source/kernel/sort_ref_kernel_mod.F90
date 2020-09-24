!-----------------------------------------------------------------------------
! (c) Crown copyright 2018 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Performs a simple insert sort on reference theta field to remove any
!>        static instability.
!>
module sort_ref_kernel_mod

  use argument_mod,      only: arg_type, func_type,    &
                               GH_FIELD, GH_READWRITE, &
                               CELLS
  use constants_mod,     only: r_def, i_def
  use fs_continuity_mod, only: Wtheta
  use kernel_mod,        only: kernel_type

  implicit none

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------
  !> The type declaration for the kernel. Contains the metadata needed by the
  !> Psy layer.
  !>
  type, public, extends(kernel_type) :: sort_ref_kernel_type
    private
    type(arg_type) :: meta_args(1) = (/              &
        arg_type(GH_FIELD,   GH_READWRITE,   WTHETA) &
        /)
    integer :: iterates_over = CELLS
  contains
    procedure, nopass :: sort_ref_code
  end type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public sort_ref_code

contains

!> @brief The subroutine which is called directly by the psy layer
!! @param[in] nlayers Integer the number of layers
!! @param[inout] theta_ref Real array, theta reference state
!! @param[in] ndf_wth The number of degrees of freedom per cell for wth
!! @param[in] undf_wth The number of unique degrees of freedom for wth
!! @param[in] map_wth Integer array holding the dofmap for the cell at the
!>            base of the column for wth
subroutine sort_ref_code(nlayers,                   &
                         theta_ref,                 &
                         ndf_wth, undf_wth, map_wth &
                         )

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers

  integer(kind=i_def), intent(in) :: ndf_wth, undf_wth

  real(kind=r_def), dimension(undf_wth), intent(inout) :: theta_ref
  integer(kind=i_def), dimension(ndf_wth), intent(in)  :: map_wth

  ! Internal variables
  integer(kind=i_def)         :: k, kcnt

  real(kind=r_def)            :: theta_k

  do k = 1, nlayers

    theta_k=theta_ref(map_wth(1) + k)
    kcnt=k

    do while (theta_ref(map_wth(1) + kcnt -1) > theta_k)
      theta_ref(map_wth(1) + kcnt) = theta_ref(map_wth(1) + kcnt -1)
      kcnt = kcnt - 1
      if (kcnt == 0) exit

    end do
    theta_ref(map_wth(1) + kcnt) = theta_k
  end do

end subroutine sort_ref_code

end module sort_ref_kernel_mod
