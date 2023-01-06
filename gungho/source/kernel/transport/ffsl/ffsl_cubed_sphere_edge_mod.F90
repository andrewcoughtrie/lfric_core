!-----------------------------------------------------------------------------
! (c) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief  Contains routines for calculating the correct directional field
!!         when a stencil crosses a cubed sphere panel edge. This is needed
!!         for the flux-form semi-Lagrangian (FFSL) transport scheme.
!!
!> @details The latter steps of the FFSL transport scheme require a field that
!!          has already been transported in a given direction. However, due to the
!!          orientation of the cubed sphere panels, the x direction on one panel
!!          may correspond to the y direction on a neighbouring panel and vice versa.
!!          When using a stencil that crosses these panels we need to get the
!!          correct directional field.
!!          For example, let q_x and q_y denote a field already transported in
!!          x and y respectively. The FFSL scheme might require q_y in the next
!!          step, but on the neighbouring panel this is actually q_x. These routines
!!          get the correct values at cubed sphere panel edges.
!------------------------------------------------------------------------------
module ffsl_cubed_sphere_edge_mod

  use constants_mod, only : r_tran, i_def

  implicit none

  private

  ! Public subroutines
  public :: get_local_rho_x
  public :: get_local_rho_y

!------------------------------------------------------------------------------
! Contained functions / subroutines
!------------------------------------------------------------------------------
contains

  !----------------------------------------------------------------------------
  !> @brief  Returns the field previously advected in x relative to the
  !!         current cubed sphere panel.
  !!
  !> @param[out]  rho_out         The output of rho_x relative to current panel
  !> @param[in]   rho_x           Field previously transported in x
  !> @param[in]   rho_y           Field previously transported in y
  !> @param[in]   ipanel          Panel ID for cells in the stencil
  !> @param[in]   stencil_size    Size of 1D stencil being used
  !> @param[in]   stencil_half    Index of centre cell in stencil
  !----------------------------------------------------------------------------
  subroutine get_local_rho_x(rho_out, rho_x, rho_y, ipanel, stencil_size, stencil_half)

    implicit none

    integer(kind=i_def), intent(in)  :: stencil_size
    integer(kind=i_def), intent(in)  :: stencil_half
    real(kind=r_tran),   intent(in)  :: rho_x(1:stencil_size)
    real(kind=r_tran),   intent(in)  :: rho_y(1:stencil_size)
    integer(kind=i_def), intent(in)  :: ipanel(1:stencil_size)
    real(kind=r_tran),   intent(out) :: rho_out(1:stencil_size)

    integer(kind=i_def) :: ii, jj

    ! Note: cells have order 1 to stencil_size, e.g.
    !       | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | for size 9 / extent 4

    ! Set rho_out=rho_x for all cells in the stencil
    rho_out(1:stencil_size) = rho_x(1:stencil_size)

    ! Correct rho_out to use rho_y depending on panel ID
    do ii = 1, stencil_half-1
      if ( ipanel(stencil_half) == 1_i_def .AND. ipanel(ii) == 6_i_def ) then
        rho_out(ii) = rho_y(ii)
      end if
      if ( ipanel(stencil_half) == 4_i_def .AND. ipanel(ii) == 1_i_def ) then
        rho_out(ii) = rho_y(ii)
      end if
      if ( ipanel(stencil_half) == 6_i_def .AND. ipanel(ii) == 4_i_def ) then
        rho_out(ii) = rho_y(ii)
      end if
    end do

    do jj = stencil_half+1, stencil_size
      if ( ipanel(stencil_half) == 2_i_def .AND. ipanel(jj) == 5_i_def ) then
        rho_out(jj) = rho_y(jj)
      end if
      if ( ipanel(stencil_half) == 3_i_def .AND. ipanel(jj) == 2_i_def ) then
        rho_out(jj) = rho_y(jj)
      end if
      if ( ipanel(stencil_half) == 5_i_def .AND. ipanel(jj) == 3_i_def ) then
        rho_out(jj) = rho_y(jj)
      end if
    end do

  end subroutine get_local_rho_x

  !----------------------------------------------------------------------------
  !> @brief  Returns the field previously advected in y relative to the
  !!         current cubed sphere panel.
  !!
  !> @param[out]  rho_out         The output of rho_y relative to current panel
  !> @param[in]   rho_x           Field previously transported in x
  !> @param[in]   rho_y           Field previously transported in y
  !> @param[in]   ipanel          Panel ID for cells in the stencil
  !> @param[in]   stencil_size    Size of 1D stencil being used
  !> @param[in]   stencil_half    Index of centre cell in stencil
  !----------------------------------------------------------------------------
  subroutine get_local_rho_y(rho_out, rho_x, rho_y, ipanel, stencil_size, stencil_half)

    implicit none

    integer(kind=i_def), intent(in)  :: stencil_size
    integer(kind=i_def), intent(in)  :: stencil_half
    real(kind=r_tran),   intent(in)  :: rho_x(1:stencil_size)
    real(kind=r_tran),   intent(in)  :: rho_y(1:stencil_size)
    integer(kind=i_def), intent(in)  :: ipanel(1:stencil_size)
    real(kind=r_tran),   intent(out) :: rho_out(1:stencil_size)

    integer(kind=i_def) :: ii, jj

    ! Note: cells have order 1 to stencil_size, e.g.
    !       | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | for size 9 / extent 4

    ! Set rho_out=rho_y for all cells in the stencil
    rho_out(1:stencil_size) = rho_y(1:stencil_size)

    ! Correct rho_out to use rho_x depending on panel ID
    do ii = 1, stencil_half-1
      if ( ipanel(stencil_half) == 1_i_def .AND. ipanel(ii) == 4_i_def ) then
        rho_out(ii) = rho_x(ii)
      end if
      if ( ipanel(stencil_half) == 4_i_def .AND. ipanel(ii) == 6_i_def ) then
        rho_out(ii) = rho_x(ii)
      end if
      if ( ipanel(stencil_half) == 6_i_def .AND. ipanel(ii) == 1_i_def ) then
        rho_out(ii) = rho_x(ii)
      end if
    end do

    do jj = stencil_half+1, stencil_size
      if ( ipanel(stencil_half) == 2_i_def .AND. ipanel(jj) == 3_i_def ) then
        rho_out(jj) = rho_x(jj)
      end if
      if ( ipanel(stencil_half) == 3_i_def .AND. ipanel(jj) == 5_i_def ) then
        rho_out(jj) = rho_x(jj)
      end if
      if ( ipanel(stencil_half) == 5_i_def .AND. ipanel(jj) == 2_i_def ) then
        rho_out(jj) = rho_x(jj)
      end if
    end do

  end subroutine get_local_rho_y

end module ffsl_cubed_sphere_edge_mod
