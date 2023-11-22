!------------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!------------------------------------------------------------------------------
!> @brief   Routines for calculating coefficients for subgrid rho representation.
!!
!! @details This module contains functions and subroutines which allow quadratic
!!          representation of rho (known as PPM) to be computed.
!------------------------------------------------------------------------------
module subgrid_rho_mod

use constants_mod,                  only: i_def, r_tran, l_def, EPS_R_TRAN
use transport_enumerated_types_mod, only: horizontal_monotone_strict,  &
                                          horizontal_monotone_relaxed

implicit none

private

public :: second_order_vertical_edge
public :: second_order_vertical_gradient
public :: fourth_order_horizontal_edge
public :: fourth_order_vertical_edge
public :: fourth_order_vertical_edge_strict
public :: fourth_order_vertical_edge_relaxed
public :: horizontal_ppm_recon
public :: horizontal_nirvana_recon
public :: vertical_nirvana_recon
public :: vertical_nirvana_recon_strict
public :: vertical_nirvana_recon_relax
public :: vertical_ppm_recon
public :: vertical_ppm_strict
public :: vertical_ppm_relax

contains

  !----------------------------------------------------------------------------
  !> @brief Calculates the vertical edge values, taking into account the height
  !!        between layers, using a second-order interpolation.
  !> @details Uses a second-order interpolation to find the vertical cell edge
  !!          values of rho. The vertical grid spacing is used to compute the
  !!          mass, and a quadratic is fit through the cumulative
  !!          mass points. This polynomial is differentiated and evaluated
  !!          at the height of the cell edge, to give the cell edge rho value.
  !!
  !> @param[in]   rho        Density values of two cells which have the ordering
  !!                         | 1 | 2 |
  !> @param[in]   dz         Height of each layer, with index the same as rho
  !> @param[in]   edge_to_do Tells routine which edge to do based on
  !!                         cells       | 1 | 2 |
  !!                         with edges  0   1   2
  !> @param[out]  edge_value The interpolated edge value at edge_to_do
  !----------------------------------------------------------------------------
  subroutine second_order_vertical_edge(rho, dz, edge_to_do, edge_value)

    implicit none

    ! Arguments
    real(kind=r_tran),   intent(in)  :: rho(1:2)
    real(kind=r_tran),   intent(in)  :: dz(1:2)
    integer(kind=i_def), intent(in)  :: edge_to_do
    real(kind=r_tran),   intent(out) :: edge_value

    ! Internal Variables
    real(kind=r_tran) :: z(0:2), edge_height
    real(kind=r_tran) :: cmass(0:2)

    ! Get heights of edges starting at 0 for lowest edge in stencil
    z(0) = 0.0_r_tran
    z(1) = z(0) + dz(1)
    z(2) = z(1) + dz(2)

    ! Get edge height to interpolate rho
    edge_height = z(edge_to_do)

    ! Get cumulative mass
    cmass(0) = 0.0_r_tran
    cmass(1) = cmass(0) + dz(1)*rho(1)
    cmass(2) = cmass(1) + dz(2)*rho(2)

    ! Calculate derivative of the quadratic at z = edge_height
    edge_value =   ( 2.0_r_tran*edge_height - z(2) ) / ( z(1) * ( z(1)-z(2) ) ) * cmass(1) &
                 + ( 2.0_r_tran*edge_height - z(1) ) / ( z(2) * ( z(2)-z(1) ) ) * cmass(2)

  end subroutine second_order_vertical_edge

  !----------------------------------------------------------------------------
  !> @brief Calculates the vertical edge gradient, taking into account the height
  !!        between layers, using a second-order method.
  !> @details Uses a second-order method to find the vertical cell edge
  !!          gradient of rho. The vertical grid spacing is used to compute the
  !!          mass, and a quadratic is fit through the cumulative
  !!          mass points. This polynomial is differentiated twice
  !!          to give the cell edge gradient.
  !!
  !> @param[in]   rho           Density values of two cells which have the ordering
  !!                            | 1 | 2 |
  !> @param[in]   dz            Height of each layer, with index the same as rho
  !> @param[out]  edge_gradient The gradient at the edge
  !----------------------------------------------------------------------------
  subroutine second_order_vertical_gradient(rho, dz, edge_gradient)

    implicit none

    ! Arguments
    real(kind=r_tran), intent(in)  :: rho(1:2)
    real(kind=r_tran), intent(in)  :: dz(1:2)
    real(kind=r_tran), intent(out) :: edge_gradient

    ! Internal Variables
    real(kind=r_tran) :: z(0:2)
    real(kind=r_tran) :: cmass(0:2)

    ! Get heights of edges starting at 0 for lowest edge in stencil
    z(0) = 0.0_r_tran
    z(1) = z(0) + dz(1)
    z(2) = z(1) + dz(2)

    ! Get cumulative mass
    cmass(0) = 0.0_r_tran
    cmass(1) = cmass(0) + dz(1)*rho(1)
    cmass(2) = cmass(1) + dz(2)*rho(2)

    ! Calculate second derivative of the quadratic
    edge_gradient =   ( 2.0_r_tran ) / ( z(1) * ( z(1)-z(2) ) ) * cmass(1) &
                    + ( 2.0_r_tran ) / ( z(2) * ( z(2)-z(1) ) ) * cmass(2)

  end subroutine second_order_vertical_gradient

  !----------------------------------------------------------------------------
  !> @brief  Calculates the estimated density at the edge of a cell required for
  !!         using horizontal PPM to estimate the quadratic subgrid representation
  !!         of rho. The function is passed four density values from consecutive cells
  !!         (which all lie in the same direction) with the dofmap
  !!         | 1 | 2 | 3 | 4 | and returns the estimated density value between
  !!         cells 2 and 3. The cells are assumed to be uniform in spacing.
  !!         Monotonicity options are provided.
  !!
  !! @param[in]   density            Has dof map of the form | 1 | 2 | 3 | 4 |
  !! @param[in]   monotone           Monotone option to ensures no over/undershoots
  !! @return      density_at_edge    Interpolated density value at edge between
  !!                                 cells 2 and 3.
  !----------------------------------------------------------------------------
  function fourth_order_horizontal_edge(density,monotone) result(density_at_edge)

    implicit none

    real(kind=r_tran),   intent(in) :: density(1:4)
    integer(kind=i_def), intent(in) :: monotone

    real(kind=r_tran) :: density_at_edge
    real(kind=r_tran) :: t1, t2, t3, tmax, tmin

    ! As the cell widths are assumed to be constant the edge value reduces to that given in
    ! Colella and Woodward, JCP, 54, 1984, equation (1.9)
    density_at_edge = (7.0_r_tran/12.0_r_tran) * (density(2)+density(3)) &
                     -(1.0_r_tran/12.0_r_tran) * (density(1)+density(4))

    if ( monotone == horizontal_monotone_strict ) then
      ! Strict monotonicity
      t1 = ( density_at_edge - density(2) )*( density(3) - density_at_edge )
      if ( t1 < 0.0_r_tran ) then
         tmin = min(density(3),density(2))
         tmax = max(density(3),density(2))
         density_at_edge = min( tmax, max(density_at_edge,tmin) )
      end if
    else if ( monotone == horizontal_monotone_relaxed ) then
      ! Relaxed monotonicity
      t1 = ( density_at_edge - density(2) )*( density(3) - density_at_edge )
      t2 = ( density(2) - density(1) )*( density(4) - density(3) )
      t3 = ( density_at_edge - density(2) )*( density(2) - density(1) )
      if ( t1 < 0.0_r_tran .AND. ( t2 >= 0.0_r_tran .OR. t3 <= 0.0_r_tran ) ) then
         tmin = min(density(3),density(2))
         tmax = max(density(3),density(2))
         density_at_edge = min( tmax, max(density_at_edge,tmin) )
      end if
    end if

  end function fourth_order_horizontal_edge

  !----------------------------------------------------------------------------
  !> @brief Calculates the vertical edge values, taking into account the height
  !!        between layers, using a fourth-order interpolation.
  !> @details Uses a fourth-order interpolation to find the vertical cell edge
  !!          values of rho. The vertical grid spacing is used to compute the
  !!          mass, and a high-order polynomial is fit through the cumulative
  !!          mass points. This polynomial is differentiated and evaluated
  !!          at the height of the cell edge, to give the cell edge value.
  !!
  !> @param[in]   rho        Density values of four cells which have the ordering
  !!                         | 1 | 2 | 3 | 4 |
  !> @param[in]   dz         Height of each layer, with index the same as rho
  !> @param[in]   edge_to_do Tells routine which edge to do based on
  !!                         cells       | 1 | 2 | 3 | 4 |
  !!                         with edges  0   1   2   3   4
  !> @param[out]  edge_below The edge value located below layer k
  !!                         (layer k corresponds to cell 3 index above)
  !----------------------------------------------------------------------------
  subroutine fourth_order_vertical_edge(rho, dz, edge_to_do, edge_below)

    implicit none

    real(kind=r_tran),    intent(in)    :: rho(1:4)
    real(kind=r_tran),    intent(in)    :: dz(1:4)
    integer(kind=i_def),  intent(in)    :: edge_to_do
    real(kind=r_tran),    intent(out)   :: edge_below

    real(kind=r_tran) :: z(0:4), dzs(1:4), dzsum, edge_height
    real(kind=r_tran) :: dmass(1:4)
    real(kind=r_tran) :: cmass(0:4)
    real(kind=r_tran) :: poly_mass(1:4)
    real(kind=r_tran) :: dl_dz(1:4)

    integer(kind=i_def) :: i

    ! Get scaling value
    dzsum = sum(dz)

    ! Get scaled dz
    dzs = dz / dzsum

    ! Get heights of edges starting at 0 for lowest edge in stencil
    z(0) = 0.0_r_tran
    do i = 1, 4
      z(i) = z(i-1) + dzs(i)
    end do

    ! Get edge height to interpolate rho to
    edge_height = z(edge_to_do)

    ! Get mass scaled by height
    dmass = rho * dzs

    ! Get cumulative mass
    cmass(0) = 0.0_r_tran
    do i = 1, 4
      cmass(i) = cmass(i-1) + dmass(i)
    end do

    ! Get cumulative mass divided by denominator of polynomial
    poly_mass(1) = cmass(1)/((z(1))*(z(1)-z(2))*(z(1)-z(3))*(z(1)-z(4)))
    poly_mass(2) = cmass(2)/((z(2))*(z(2)-z(1))*(z(2)-z(3))*(z(2)-z(4)))
    poly_mass(3) = cmass(3)/((z(3))*(z(3)-z(1))*(z(3)-z(2))*(z(3)-z(4)))
    poly_mass(4) = cmass(4)/((z(4))*(z(4)-z(1))*(z(4)-z(2))*(z(4)-z(3)))

    ! Calculate derivative of numerator of polynomial at edge height
    dl_dz    = 4.0_r_tran*edge_height**3
    dl_dz(1) = dl_dz(1) - 3.0_r_tran*(z(2)+z(3)+z(4))*edge_height**2 &
               + 2.0_r_tran*(z(3)*z(4) + z(2)*z(3) + z(2)*z(4))*edge_height - z(2)*z(3)*z(4)
    dl_dz(2) = dl_dz(2) - 3.0_r_tran*(z(1)+z(3)+z(4))*edge_height**2 &
               + 2.0_r_tran*(z(3)*z(4) + z(1)*z(3) + z(1)*z(4))*edge_height - z(1)*z(3)*z(4)
    dl_dz(3) = dl_dz(3) - 3.0_r_tran*(z(1)+z(2)+z(4))*edge_height**2 &
               + 2.0_r_tran*(z(2)*z(4) + z(1)*z(2) + z(1)*z(4))*edge_height - z(1)*z(2)*z(4)
    dl_dz(4) = dl_dz(4) - 3.0_r_tran*(z(1)+z(2)+z(3))*edge_height**2 &
               + 2.0_r_tran*(z(2)*z(3) + z(1)*z(2) + z(1)*z(3))*edge_height - z(1)*z(2)*z(3)

    ! Calculate value of edge below layer k
    edge_below = sum( poly_mass * dl_dz )

  end subroutine fourth_order_vertical_edge

  !----------------------------------------------------------------------------
  !> @brief Calculates the vertical edge values, taking into account the height
  !!        between layers, using a fourth-order interpolation, then applying
  !!        strict monotonicity.
  !> @details Uses a fourth-order interpolation to find the vertical cell edge
  !!          values of rho. The vertical grid spacing is used to compute the
  !!          mass, and a high-order polynomial is fit through the cumulative
  !!          mass points. This polynomial is differentiated and evaluated
  !!          at the height of the cell edge, to give the cell edge value.
  !!          Strict monotonicity constraints are then applied.
  !!
  !> @param[in]   rho        Density values of four cells which have the ordering
  !!                         | 1 | 2 | 3 | 4 |
  !> @param[in]   dz         Height of each layer, with index the same as rho
  !> @param[in]   edge_to_do Tells routine which edge to do based on
  !!                         cells       | 1 | 2 | 3 | 4 |
  !!                         with edges  0   1   2   3   4
  !> @param[in]   log_space  Switch to use natural logarithmic space
  !!                         for edge interpolation
  !> @param[out]  edge_below The edge value located below layer k
  !!                         (layer k corresponds to cell 3 index above)
  !----------------------------------------------------------------------------
  subroutine fourth_order_vertical_edge_strict(rho,        &
                                               dz,         &
                                               edge_to_do, &
                                               log_space,  &
                                               edge_below)

    implicit none

    real(kind=r_tran),    intent(in)    :: rho(1:4)
    real(kind=r_tran),    intent(in)    :: dz(1:4)
    integer(kind=i_def),  intent(in)    :: edge_to_do
    logical(kind=l_def),  intent(in)    :: log_space
    real(kind=r_tran),    intent(out)   :: edge_below

    real(kind=r_tran) :: t1, tmin, tmax
    real(kind=r_tran) :: log_rho(1:4)

    ! Get initial unlimited edge value
    if (log_space) then
      log_rho = log( max( EPS_R_TRAN, rho) )
      call fourth_order_vertical_edge(log_rho, dz, edge_to_do, edge_below)
      edge_below = exp(edge_below)
    else
      call fourth_order_vertical_edge(rho, dz, edge_to_do, edge_below)
    end if

    ! Strict Monotonicity
    if ( edge_to_do>0_i_def .AND. edge_to_do<4_i_def) then
      t1 = ( edge_below - rho(edge_to_do) )*( rho(edge_to_do+1) - edge_below )
      if ( t1 < 0.0_r_tran ) then
        tmin = min(rho(edge_to_do+1),rho(edge_to_do))
        tmax = max(rho(edge_to_do+1),rho(edge_to_do))
        edge_below = min( tmax, max(edge_below,tmin) )
      end if
    else if ( edge_to_do == 0_i_def ) then
      edge_below = min( max( rho(2), rho(1) ), max( edge_below, min( rho(2), rho(1) ) ) )
    else if ( edge_to_do == 4_i_def ) then
      edge_below = min( max( rho(4), rho(3) ), max( edge_below, min( rho(4), rho(3) ) ) )
    end if

  end subroutine fourth_order_vertical_edge_strict

  !----------------------------------------------------------------------------
  !> @brief Calculates the vertical edge values, taking into account the height
  !!        between layers, using a fourth-order interpolation, then applying
  !!        relaxed monotonicity.
  !> @details Uses a fourth-order interpolation to find the vertical cell edge
  !!          values of rho. The vertical grid spacing is used to compute the
  !!          mass, and a high-order polynomial is fit through the cumulative
  !!          mass points. This polynomial is differentiated and evaluated
  !!          at the height of the cell edge, to give the cell edge value.
  !!          Relaxed monotonicity constraints are then applied.
  !!
  !> @param[in]   rho        Density values of four cells which have the ordering
  !!                         | 1 | 2 | 3 | 4 |
  !> @param[in]   dz         Height of each layer, with index the same as rho
  !> @param[in]   edge_to_do Tells routine which edge to do based on
  !!                         cells       | 1 | 2 | 3 | 4 |
  !!                         with edges  0   1   2   3   4
  !> @param[in]   log_space  Switch to use natural logarithmic space
  !!                         for edge interpolation
  !> @param[out]  edge_below The edge value located below layer k
  !!                         (layer k corresponds to cell 3 index above)
  !----------------------------------------------------------------------------
  subroutine fourth_order_vertical_edge_relaxed(rho,        &
                                                dz,         &
                                                edge_to_do, &
                                                log_space,  &
                                                edge_below)

    implicit none

    real(kind=r_tran),    intent(in)    :: rho(1:4)
    real(kind=r_tran),    intent(in)    :: dz(1:4)
    integer(kind=i_def),  intent(in)    :: edge_to_do
    logical(kind=l_def),  intent(in)    :: log_space
    real(kind=r_tran),    intent(out)   :: edge_below

    real(kind=r_tran) :: t1, t2, t3, tmin, tmax
    real(kind=r_tran) :: log_rho(1:4)

    ! Get initial unlimited edge value
    if (log_space) then
      log_rho = log( max( EPS_R_TRAN, rho) )
      call fourth_order_vertical_edge(log_rho, dz, edge_to_do, edge_below)
      edge_below = exp(edge_below)
    else
      call fourth_order_vertical_edge(rho, dz, edge_to_do, edge_below)
    end if

    ! Relaxed Monotonicity
    if ( edge_to_do>0_i_def .AND. edge_to_do<4_i_def) then
      t1 = ( edge_below - rho(edge_to_do) )*( rho(edge_to_do+1) - edge_below )
      if ( edge_to_do == 2_i_def ) then
        t2 = ( rho(edge_to_do) - rho(edge_to_do-1) )*( rho(edge_to_do+2) - rho(edge_to_do+1) )
        t3 = ( edge_below - rho(edge_to_do) )*( rho(edge_to_do) - rho(edge_to_do-1) )
      else
        t2 = 1.0_r_tran
        t3 = -1.0_r_tran
      end if
      if ( t1 < 0.0_r_tran .AND. ( t2 >= 0.0_r_tran .OR. t3 <= 0.0_r_tran ) ) then
        tmin = min(rho(edge_to_do+1),rho(edge_to_do))
        tmax = max(rho(edge_to_do+1),rho(edge_to_do))
        edge_below = min( tmax, max(edge_below,tmin) )
      end if
    else if ( edge_to_do == 0_i_def ) then
      edge_below = min( max( rho(2), rho(1) ), max( edge_below, min( rho(2), rho(1) ) ) )
    else if ( edge_to_do == 4_i_def ) then
      edge_below = min( max( rho(4), rho(3) ), max( edge_below, min( rho(4), rho(3) ) ) )
    end if

  end subroutine fourth_order_vertical_edge_relaxed

  !----------------------------------------------------------------------------
  !> @brief  Returns the horizontal PPM reconstruction. This can be used to
  !!         compute the flux as:
  !!         flux = u * reconstruction
  !!         The dofmap for the field values is of the form
  !!         | 1 | 2 | 3 | 4 | 5 | where the reconstructions are being
  !!         computed for the edges of cell 3. The reconstruction is third-order,
  !!         and is based on the quadratic subgrid reconstruction of PPM.
  !!
  !! @param[out]  recon       The PPM reconstruction
  !! @param[in]   dep         The fractional departure distance for the reconstruction point.
  !!                          For dep>0 the recon lives between cells 3 and 4
  !!                          For dep<0 the recon lives between cells 2 and 3
  !! @param[in]   field       Field values in the 5 cells with ordering
  !!                          | 1 | 2 | 3 | 4 | 5 |
  !! @param[in]   monotone    Monotone option to ensures no over/undershoots
  !----------------------------------------------------------------------------
  subroutine horizontal_ppm_recon(recon,dep,field,monotone)

    implicit none

    real(kind=r_tran),    intent(out) :: recon
    real(kind=r_tran),    intent(in)  :: dep
    real(kind=r_tran),    intent(in)  :: field(1:5)
    integer(kind=i_def),  intent(in)  :: monotone

    real(kind=r_tran) :: edge_left
    real(kind=r_tran) :: edge_right
    real(kind=r_tran) :: cm, cc, cp
    real(kind=r_tran) :: t1, t2, t3

    ! Get PPM edge values
    edge_left = fourth_order_horizontal_edge(field(1:4),monotone)
    edge_right = fourth_order_horizontal_edge(field(2:5),monotone)

    ! Compute reconstruction weights
    if (dep >= 0.0_r_tran) then
      cp = 1.0_r_tran - 2.0_r_tran*dep + dep**2.0_r_tran
      cc = 3.0_r_tran*dep - 2.0_r_tran*dep**2.0_r_tran
      cm = - dep + dep**2.0_r_tran
    else
      cp = dep + dep**2.0_r_tran
      cc = -3.0_r_tran*dep - 2.0_r_tran*dep**2.0_r_tran
      cm = 1.0_r_tran + 2.0_r_tran*dep + dep**2.0_r_tran
    end if

    ! Apply weights to field and field edge values
    recon = cm*edge_left + cc*field(3) + cp*edge_right

    ! Apply monotonicity if needed
    if ( monotone == horizontal_monotone_strict ) then
      ! Strict monotonicity
      t1 = (2.0_r_tran*edge_left + edge_right - 3.0_r_tran*field(3)) &
           / (3.0_r_tran*edge_left + 3.0_r_tran*edge_right - 6.0_r_tran*field(3) + EPS_R_TRAN)
      if ( ( t1 + EPS_R_TRAN ) * ( 1.0_r_tran + EPS_R_TRAN - t1 ) > 0.0_r_tran ) then
        recon = field(3)
      end if
    else if ( monotone == horizontal_monotone_relaxed ) then
      ! Relaxed monotonicity
      t1 = (2.0_r_tran*edge_left + edge_right - 3.0_r_tran*field(3)) &
           / (3.0_r_tran*edge_left + 3.0_r_tran*edge_right - 6.0_r_tran*field(3) + EPS_R_TRAN)
      if ( ( t1 + EPS_R_TRAN ) * ( 1.0_r_tran + EPS_R_TRAN - t1 ) > 0.0_r_tran ) then
        t2 = (edge_right - field(3))*(field(3) - edge_left)
        t3 = abs(field(3) - edge_left) - abs(edge_right - field(3))
        if ( t2 < 0.0_r_tran ) then
          recon = field(3)
        else
          if ( t3 < 0.0_r_tran ) then
            if (dep >= 0.0_r_tran) then
              cp = 0.0_r_tran
              cc = 3.0_r_tran - 3.0_r_tran*dep + dep**2.0_r_tran
              cm = -2.0_r_tran + 3.0_r_tran*dep - dep**2.0_r_tran
            else
              cp = 0.0_r_tran
              cc = dep**2.0_r_tran
              cm = 1.0_r_tran - dep**2.0_r_tran
            end if
          else
            if (dep >= 0.0_r_tran) then
              cp = 1.0_r_tran - dep**2.0_r_tran
              cc = dep**2.0_r_tran
              cm = 0.0_r_tran
            else
              cp = -2.0_r_tran - 3.0_r_tran*dep - dep**2.0_r_tran
              cc = 3.0_r_tran + 3.0_r_tran*dep + dep**2.0_r_tran
              cm = 0.0_r_tran
            end if
          end if
          recon = cm*edge_left + cc*field(3) + cp*edge_right
        end if
      end if
    end if

  end subroutine horizontal_ppm_recon

  !----------------------------------------------------------------------------
  !> @brief  Returns the horizontal Nirvana reconstruction at a cell edge.
  !!         This can be used to compute the flux as:
  !!         flux = u * reconstruction
  !!         The reconstruction is third-order, and is based on a quadratic
  !!         subgrid reconstruction
  !!
  !! @param[out]  recon     The Nirvana reconstruction
  !! @param[in]   dep       The fractional departure distance for the reconstruction point.
  !!                        For dep>0 the recon lives between cells 2 and 3
  !!                        For dep<0 the recon lives between cells 1 and 2
  !! @param[in]   field     Field values of three cells which have the ordering
  !!                        | 1 | 2 | 3 |
  !! @param[in]   monotone  Monotone option to ensures no over/undershoots
  subroutine horizontal_nirvana_recon(recon, dep, field, monotone)

    implicit none

    ! Arguments
    real(kind=r_tran),   intent(out) :: recon
    real(kind=r_tran),   intent(in)  :: dep
    real(kind=r_tran),   intent(in)  :: field(1:3)
    integer(kind=i_def), intent(in)  :: monotone

    real(kind=r_tran) :: cm, cc, cp
    real(kind=r_tran) :: p0, p1, pmin0, pmin1, pmax0, pmax1, t1, t2, t3

    ! Compute reconstruction weights based on sign of fractional departure distance
    if (dep >= 0.0_r_tran) then
      cp = 1.0_r_tran/3.0_r_tran - dep/2.0_r_tran + dep**2.0_r_tran/6.0_r_tran
      cc = 5.0_r_tran/6.0_r_tran + dep/2.0_r_tran - dep**2.0_r_tran/3.0_r_tran
      cm = -1.0_r_tran/6.0_r_tran + dep**2.0_r_tran/6.0_r_tran
    else
      cp = -1.0_r_tran/6.0_r_tran + dep**2.0_r_tran/6.0_r_tran
      cc = 5.0_r_tran/6.0_r_tran - dep/2.0_r_tran - dep**2.0_r_tran/3.0_r_tran
      cm = 1.0_r_tran/3.0_r_tran + dep/2.0_r_tran + dep**2.0_r_tran/6.0_r_tran
    end if

    ! Apply reconstruction weights to the field
    recon = cm*field(1) + cc*field(2) + cp*field(3)

    ! Apply monotonicity if needed
    if ( monotone == horizontal_monotone_strict ) then
      ! Strict monotonicity
      t1 = -0.5_r_tran*( field(2)-field(1) ) / &
           ( (field(1)-2.0_r_tran*field(2)+field(3))/2.0_r_tran + EPS_R_TRAN )
      if ( ( t1 + EPS_R_TRAN ) * ( 1.0_r_tran + EPS_R_TRAN - t1 ) > 0.0_r_tran ) then
        recon = field(2)
      else
        p0 = (-field(3) + 5.0_r_tran * field(2) + 2.0_r_tran * field(1)) / 6.0_r_tran
        p1 = (2.0_r_tran*field(3) + 5.0_r_tran * field(2) -  field(2)) / 6.0_r_tran
        pmin0 = min( field(1), field(2))
        pmax0 = max( field(1), field(2))
        pmin1 = min( field(3), field(2))
        pmax1 = max( field(3), field(2))
        if ( p0 .gt. pmax0 .OR. p0 .lt. pmin0 .OR. p1 .gt. pmax1 .OR. p1 .lt. pmin1) then
          recon = field(2)
        end if
      end if
    else if ( monotone == horizontal_monotone_relaxed ) then
      ! Relaxed monotonicity
      t1 = -0.5_r_tran*( field(2)-field(1) ) / &
           ( (field(1)-2.0_r_tran*field(2)+field(3))/2.0_r_tran + EPS_R_TRAN )
      if ( ( t1 + EPS_R_TRAN ) * ( 1.0_r_tran + EPS_R_TRAN - t1 ) > 0.0_r_tran ) then
        p0 = 0.5_r_tran*(field(1)+field(2))
        p1 = 0.5_r_tran*(field(2)+field(3))
        t2 = (p1-field(2)) * (field(2)-p0)
        t3 = abs(field(2)-p0) - abs(p1-field(2))
        if (t2 < 0.0_r_tran) then
          recon = field(2)
        else
          if (t3 < 0.0_r_tran) then
            if (dep >= 0.0_r_tran) then
              cp = 0.0_r_tran
              cc = 3.0_r_tran - 3.0_r_tran*dep + dep**2.0_r_tran
              cm = -2.0_r_tran + 3.0_r_tran*dep - dep**2.0_r_tran
            else
              cp = 0.0_r_tran
              cc = dep**2.0_r_tran
              cm = 1.0_r_tran - dep**2.0_r_tran
            end if
          else
            if (dep >= 0.0_r_tran) then
              cp = 1.0_r_tran -  dep**2.0_r_tran
              cc = dep**2.0_r_tran
              cm = 0.0_r_tran
            else
              cp = -2.0_r_tran - 3.0_r_tran*dep - dep**2.0_r_tran
              cc = 3.0_r_tran + 3.0_r_tran*dep + dep**2.0_r_tran
              cm = 0.0_r_tran
            end if
          end if
          recon = cm*p0 + cc*field(2) + cp*p1
        end if
      end if
    end if

  end subroutine horizontal_nirvana_recon

  !----------------------------------------------------------------------------
  !> @brief  Returns the vertical Nirvana reconstruction. This can be used to
  !!         compute the flux as:
  !!         flux = u * reconstruction
  !!         The reconstruction is a third-order reconstruction at the cell's
  !!         vertical edges, and is based on a quadratic subgrid reconstruction.
  !!
  !! @param[out]  recon       The Nirvana reconstruction
  !! @param[in]   dep         The fractional departure distance for the reconstruction point.
  !!                          For dep>0 the recon lives above the cell
  !!                          For dep<0 the recon lives below the cell
  !! @param[in]   field       Field value of the cell
  !! @param[out]  grad_below  Estimate of gradient at bottom edge
  !! @param[out]  grad_above  Estimate of gradient at top edge
  !! @param[in]   dz          Height of cell
  !----------------------------------------------------------------------------
  subroutine vertical_nirvana_recon(recon,      &
                                    dep,        &
                                    field,      &
                                    grad_below, &
                                    grad_above, &
                                    dz)

    implicit none

    ! Arguments
    real(kind=r_tran),   intent(out) :: recon
    real(kind=r_tran),   intent(in)  :: dep
    real(kind=r_tran),   intent(in)  :: field
    real(kind=r_tran),   intent(in)  :: dz
    real(kind=r_tran),   intent(in)  :: grad_below
    real(kind=r_tran),   intent(in)  :: grad_above

    ! Reconstruction weights
    real(kind=r_tran) :: cm, cc, cp

    ! Compute reconstruction weights
    if (dep >= 0.0_r_tran) then
      cp = 1.0_r_tran/3.0_r_tran - dep/2.0_r_tran + dep**2.0_r_tran/6.0_r_tran
      cc = 1.0_r_tran
      cm = 1.0_r_tran/6.0_r_tran - dep**2.0_r_tran/6.0_r_tran
    else
      cp = -1.0_r_tran/6.0_r_tran + dep**2.0_r_tran/6.0_r_tran
      cc = 1.0_r_tran
      cm = -1.0_r_tran/3.0_r_tran - dep/2.0_r_tran - dep**2.0_r_tran/6.0_r_tran
    end if

    ! Apply weights to gradients and field
    recon = cm*grad_below*dz + cc*field + cp*grad_above*dz

  end subroutine vertical_nirvana_recon

  !----------------------------------------------------------------------------
  !> @brief  Returns the vertical Nirvana reconstruction. This can be used to
  !!         compute the flux as:
  !!         flux = u * reconstruction
  !!         The reconstruction is a third-order reconstruction at the cell's
  !!         vertical edges, and is based on a quadratic subgrid reconstruction
  !!         with a strict monotonic limiter applied.
  !!
  !! @param[out]  recon       The Nirvana reconstruction
  !! @param[in]   dep         The fractional departure distance for the reconstruction point.
  !!                          For dep>0 the recon lives between cells 2 and 3
  !!                          For dep<0 the recon lives between cells 1 and 2
  !! @param[in]   field       Field values of three cells which have the ordering
  !!                          | 1 | 2 | 3 |. Cells 1 and 2 are only used for monotonicity
  !! @param[out]  grad_below  Estimate of gradient at z = 0 of cell 2, i.e.
  !!                          at the edge between cells 1 and 2
  !! @param[in]   grad_above  Estimate of gradient at z = 1 of cell 2, i.e.
  !!                          at the edge between cells 2 and 3
  !! @param[in]   dz          Height of cell 2
  !----------------------------------------------------------------------------
  subroutine vertical_nirvana_recon_strict(recon,      &
                                           dep,        &
                                           field,      &
                                           grad_below, &
                                           grad_above, &
                                           dz)

    implicit none

    ! Arguments
    real(kind=r_tran),   intent(out) :: recon
    real(kind=r_tran),   intent(in)  :: dep
    real(kind=r_tran),   intent(in)  :: field(1:3)
    real(kind=r_tran),   intent(in)  :: dz
    real(kind=r_tran),   intent(in)  :: grad_below
    real(kind=r_tran),   intent(in)  :: grad_above

    ! Monotonicity variables
    real(kind=r_tran) :: p0, p1, pmin0, pmin1, pmax0, pmax1, t1

    ! Compute Nirvana reconstruction weights
    call vertical_nirvana_recon(recon, dep, field(2), &
                                grad_below, grad_above, dz)

    ! Strict monotonicity
    t1 = (grad_below*dz)/(grad_above*dz-grad_below*dz + EPS_R_TRAN)
    if ( ( t1 + EPS_R_TRAN ) * ( 1.0_r_tran + EPS_R_TRAN - t1 ) > 0.0_r_tran ) then
      recon = field(2)
    else
      p0 = field(2) - grad_below*dz / 3.0_r_tran - grad_above*dz / 6.0_r_tran
      p1 = field(2) + grad_above*dz / 3.0_r_tran + grad_below*dz / 6.0_r_tran
      pmin0 = min( field(1), field(2))
      pmax0 = max( field(1), field(2))
      pmin1 = min( field(3), field(2))
      pmax1 = max( field(3), field(2))
      if ( p0 .gt. pmax0 .OR. p0 .lt. pmin0 .OR. p1 .gt. pmax1 .OR. p1 .lt. pmin1) then
        recon = field(2)
      end if
    end if

  end subroutine vertical_nirvana_recon_strict

  !----------------------------------------------------------------------------
  !> @brief  Returns the vertical Nirvana reconstruction. This can be used to
  !!         compute the flux as:
  !!         flux = u * reconstruction
  !!         The reconstruction is a third-order reconstruction at the cell's
  !!         vertical edges, and is based on a quadratic subgrid reconstruction
  !!         with a relaxed monotonic limiter applied.
  !!
  !! @param[out]  recon       The Nirvana reconstruction
  !! @param[in]   dep         The fractional departure distance for the reconstruction point.
  !!                          For dep>0 the recon lives between cells 2 and 3
  !!                          For dep<0 the recon lives between cells 1 and 2
  !! @param[in]   field       Field values of three cells which have the ordering
  !!                          | 1 | 2 | 3 |. Cells 1 and 3 are only used for monotonicity
  !! @param[out]  grad_below  Estimate of gradient at z = 0 of cell 2, i.e.
  !!                          at the edge between cells 1 and 2
  !! @param[in]   grad_above  Estimate of gradient at z = 1 of cell 2, i.e.
  !!                          at the edge between cells 2 and 3
  !! @param[in]   dz          Height of cell 2
  !----------------------------------------------------------------------------
  subroutine vertical_nirvana_recon_relax(recon,      &
                                          dep,        &
                                          field,      &
                                          grad_below, &
                                          grad_above, &
                                          dz)

    implicit none

    ! Arguments
    real(kind=r_tran),   intent(out) :: recon
    real(kind=r_tran),   intent(in)  :: dep
    real(kind=r_tran),   intent(in)  :: field(1:3)
    real(kind=r_tran),   intent(in)  :: dz
    real(kind=r_tran),   intent(in)  :: grad_below
    real(kind=r_tran),   intent(in)  :: grad_above

    ! Internal variables
    real(kind=r_tran) :: cm, cc, cp
    real(kind=r_tran) :: p0, p1, t1, t2, t3

    ! Compute Nirvana reconstruction weights
    call vertical_nirvana_recon(recon, dep, field(2), &
                                grad_below, grad_above, dz)

    ! Relaxed monotonicity
    t1 = -0.5_r_tran*( field(2)-field(1) ) / &
         ( (field(1)-2.0_r_tran*field(2)+field(3))/2.0_r_tran + EPS_R_TRAN )
    if ( ( t1 + EPS_R_TRAN ) * ( 1.0_r_tran + EPS_R_TRAN - t1 ) > 0.0_r_tran ) then
      p0 = 0.5_r_tran*(field(1)+field(2))
      p1 = 0.5_r_tran*(field(2)+field(3))
      t2 = (p1-field(2)) * (field(2)-p0)
      t3 = abs(field(2)-p0) - abs(p1-field(2))
      if (t2 < 0.0_r_tran) then
        recon = field(2)
      else
        if (t3 < 0.0_r_tran) then
          if (dep >= 0.0_r_tran) then
            cp = 0.0_r_tran
            cc = 3.0_r_tran - 3.0_r_tran*dep + dep**2.0_r_tran
            cm = -2.0_r_tran + 3.0_r_tran*dep - dep**2.0_r_tran
          else
            cp = 0.0_r_tran
            cc = dep**2.0_r_tran
            cm = 1.0_r_tran - dep**2.0_r_tran
          end if
        else
          if (dep >= 0.0_r_tran) then
            cp = 1.0_r_tran - dep**2.0_r_tran
            cc = dep**2.0_r_tran
            cm = 0.0_r_tran
          else
            cp = -2.0_r_tran - 3.0_r_tran*dep - dep**2.0_r_tran
            cc = 3.0_r_tran + 3.0_r_tran*dep + dep**2.0_r_tran
            cm = 0.0_r_tran
          end if
        end if
        recon = cm*p0 + cc*field(2) + cp*p1
      end if
    end if

  end subroutine vertical_nirvana_recon_relax

  !----------------------------------------------------------------------------
  !> @brief  Returns the vertical PPM reconstruction (also used for the reversible
  !!         Nirvana reconstruction). This can be used to compute the flux as:
  !!         flux = u * reconstruction
  !!         The reconstruction is a third-order reconstruction at the cell's
  !!         vertical edges, and is based on a quadratic subgrid reconstruction that
  !!         uses the field interpolated to cell edges.
  !!
  !! @param[out]  recon       The PPM reconstruction
  !! @param[in]   dep         The fractional departure distance for the reconstruction point.
  !!                          For dep>0 the recon lives above the cell
  !!                          For dep<0 the recon lives below the cell
  !! @param[in]   field       Field values in the cell
  !! @param[out]  edge_below  Estimate of edge value below the cell
  !! @param[out]  edge_above  Estimate of edge value above the cell
  !----------------------------------------------------------------------------
  subroutine vertical_ppm_recon(recon,      &
                                dep,        &
                                field,      &
                                edge_below, &
                                edge_above)

    implicit none

    ! Arguments
    real(kind=r_tran),   intent(out) :: recon
    real(kind=r_tran),   intent(in)  :: dep
    real(kind=r_tran),   intent(in)  :: field
    real(kind=r_tran),   intent(in)  :: edge_below
    real(kind=r_tran),   intent(in)  :: edge_above

    ! Reconstruction weights
    real(kind=r_tran) :: cm, cc, cp

    ! Compute reconstruction weights
    if (dep >= 0.0_r_tran) then
      cp = 1.0_r_tran - 2.0_r_tran*dep + dep**2.0_r_tran
      cc = 3.0_r_tran*dep - 2.0_r_tran*dep**2.0_r_tran
      cm = - dep + dep**2.0_r_tran
    else
      cp = dep + dep**2.0_r_tran
      cc = -3.0_r_tran*dep - 2.0_r_tran*dep**2.0_r_tran
      cm = 1.0_r_tran + 2.0_r_tran*dep + dep**2.0_r_tran
    end if

    ! Apply weights to field and field edge values
    recon = cm*edge_below + cc*field + cp*edge_above

  end subroutine vertical_ppm_recon

  !----------------------------------------------------------------------------
  !> @brief  Returns the strict limited vertical PPM reconstruction (also used
  !!         for the strict limited reversible Nirvana reconstruction). This
  !!         can be used to compute the flux as:
  !!         flux = u * reconstruction
  !!         The reconstruction is a third-order reconstruction at the cell's
  !!         vertical edges, and is based on a quadratic subgrid reconstruction that
  !!         uses the field interpolated to cell edges, with a strict monotonic
  !!         limiter applied.
  !!
  !! @param[out]  recon       The PPM reconstruction
  !! @param[in]   dep         The fractional departure distance for the reconstruction point.
  !!                          For dep>0 the recon lives above the cell
  !!                          For dep<0 the recon lives below the cell
  !! @param[in]   field       Field values in the cell
  !! @param[out]  edge_below  Estimate of edge value below the cell
  !! @param[out]  edge_above  Estimate of edge value above the cell
  !----------------------------------------------------------------------------
  subroutine vertical_ppm_strict(recon,      &
                                 dep,        &
                                 field,      &
                                 edge_below, &
                                 edge_above)

    implicit none

    ! Arguments
    real(kind=r_tran),   intent(out) :: recon
    real(kind=r_tran),   intent(in)  :: dep
    real(kind=r_tran),   intent(in)  :: field
    real(kind=r_tran),   intent(in)  :: edge_below
    real(kind=r_tran),   intent(in)  :: edge_above

    ! Monotonicity variable
    real(kind=r_tran) :: t1

    ! Compute PPM reconstruction weights
    call vertical_ppm_recon(recon, dep, field, &
                            edge_below, edge_above)

    ! Strict monotonicity
    t1 = (2.0_r_tran*edge_below + edge_above - 3.0_r_tran*field) &
         / (3.0_r_tran*edge_below + 3.0_r_tran*edge_above - 6.0_r_tran*field + EPS_R_TRAN)
    if ( ( t1 + EPS_R_TRAN ) * ( 1.0_r_tran + EPS_R_TRAN - t1 ) > 0.0_r_tran ) then
      recon = field
    end if

  end subroutine vertical_ppm_strict

  !----------------------------------------------------------------------------
  !> @brief  Returns the relaxed limited vertical PPM reconstruction (also used
  !!         for the strict limited reversible Nirvana reconstruction). This
  !!         can be used to compute the flux as:
  !!         flux = u * reconstruction
  !!         The reconstruction is a third-order reconstruction at the cell's
  !!         vertical edges, and is based on a quadratic subgrid reconstruction that
  !!         uses the field interpolated to cell edges, with a relaxed monotonic
  !!         limiter applied.
  !!
  !! @param[out]  recon       The PPM reconstruction
  !! @param[in]   dep         The fractional departure distance for the reconstruction point.
  !!                          For dep>0 the recon lives above the cell
  !!                          For dep<0 the recon lives below the cell
  !! @param[in]   field       Field values in the cell
  !! @param[out]  edge_below  Estimate of edge value below the cell
  !! @param[out]  edge_above  Estimate of edge value above the cell
  !----------------------------------------------------------------------------
  subroutine vertical_ppm_relax(recon,      &
                                dep,        &
                                field,      &
                                edge_below, &
                                edge_above)

    implicit none

    ! Arguments
    real(kind=r_tran),   intent(out) :: recon
    real(kind=r_tran),   intent(in)  :: dep
    real(kind=r_tran),   intent(in)  :: field
    real(kind=r_tran),   intent(in)  :: edge_below
    real(kind=r_tran),   intent(in)  :: edge_above

    ! Internal variables
    real(kind=r_tran) :: cm, cc, cp
    real(kind=r_tran) :: t1, t2, t3

    ! Compute PPM reconstruction weights
    call vertical_ppm_recon(recon, dep, field, &
                            edge_below, edge_above)

    ! Relaxed monotonicity
    t1 = (2.0_r_tran*edge_below + edge_above - 3.0_r_tran*field) &
         / (3.0_r_tran*edge_below + 3.0_r_tran*edge_above - 6.0_r_tran*field + EPS_R_TRAN)
    if ( ( t1 + EPS_R_TRAN ) * ( 1.0_r_tran + EPS_R_TRAN - t1 ) > 0.0_r_tran ) then
      t2 = (edge_above - field)*(field - edge_below)
      t3 = abs(field - edge_below) - abs(edge_above - field)
      if ( t2 < 0.0_r_tran ) then
        recon = field
      else
        if ( t3 < 0.0_r_tran ) then
          if (dep >= 0.0_r_tran) then
            cp = 0.0_r_tran
            cc = 3.0_r_tran - 3.0_r_tran*dep + dep**2.0_r_tran
            cm = -2.0_r_tran + 3.0_r_tran*dep - dep**2.0_r_tran
          else
            cp = 0.0_r_tran
            cc = dep**2.0_r_tran
            cm = 1.0_r_tran - dep**2.0_r_tran
          end if
        else
          if (dep >= 0.0_r_tran) then
            cp = 1.0_r_tran - dep**2.0_r_tran
            cc = dep**2.0_r_tran
            cm = 0.0_r_tran
          else
            cp = -2.0_r_tran - 3.0_r_tran*dep - dep**2.0_r_tran
            cc = 3.0_r_tran + 3.0_r_tran*dep + dep**2.0_r_tran
            cm = 0.0_r_tran
          end if
        end if
        recon = cm*edge_below + cc*field + cp*edge_above
      end if
    end if

  end subroutine vertical_ppm_relax

end module subgrid_rho_mod
