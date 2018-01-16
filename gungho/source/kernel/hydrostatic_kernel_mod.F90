!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel which computes the pressure and geopotential 
!!        gradient for rhs of the momentum equation
!!        the exner pressure is computed from the equation of state using density
!!        and potential temperature


!> @details The kernel computes the pressure and geopotential gradient part of the 
!>         rhs of the momentum equation for the nonlinear equations,
!>         written in the vector invariant form
!>         This rhs consists of four terms:
!>         Pressure gradient: \f[ cp*\theta*\nabla(\Pi)\f]
!>         geopotential gradient: \f[ \nabla(\Phi) ( \equiv g for some domains)\f]
!>         gradient of kinetic energy: \f[ \nabla(1/2*u.u) \f] 
!>         vorticity advection: \f[ \xi/\rho \times F (with vorticity \xi and mass flux F) \f]
!>         This results in:
!>         \f[ r_u = -\xi/\rho \times F - \nabla(\Phi + 1/2*u.u) - cp*\theta*\nabla(\Pi) +  \nabla(\Phi) \f]
!>         This kernel only contains the volume integral parts of the pressure
!>         gradient and the full geopotential gradient. 
!>         The facet integrals are handled in the boundary kernal routine
module hydrostatic_kernel_mod

use argument_mod,      only : arg_type, func_type,                 &
                              GH_FIELD, GH_READ, GH_INC,           &
                              ANY_SPACE_1, W2, W3, W0,             &
                              GH_BASIS, GH_DIFF_BASIS,             &
                              CELLS, GH_QUADRATURE_XYoZ
use constants_mod,     only : r_def
use kernel_mod,        only : kernel_type
use planet_config_mod, only : cp

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: hydrostatic_kernel_type
  private
  type(arg_type) :: meta_args(4) = (/                                  &
       arg_type(GH_FIELD,   GH_INC,  W2),                              &
       arg_type(GH_FIELD,   GH_READ, W3),                              &
       arg_type(GH_FIELD,   GH_READ, ANY_SPACE_1),                     &
       arg_type(GH_FIELD,   GH_READ, W0)                               &
       /)
  type(func_type) :: meta_funcs(4) = (/                                &
       func_type(W2,          GH_BASIS, GH_DIFF_BASIS),                &
       func_type(W3,          GH_BASIS),                               &
       func_type(ANY_SPACE_1, GH_BASIS, GH_DIFF_BASIS),                &
       func_type(W0,                    GH_DIFF_BASIS)                 &
       /)
  integer :: iterates_over = CELLS
  integer :: gh_shape = GH_QUADRATURE_XYoZ
contains
  procedure, nopass ::hydrostatic_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! Overload the default structure constructor for function space
interface hydrostatic_kernel_type
   module procedure hydrostatic_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public hydrostatic_code
contains

type(hydrostatic_kernel_type) function hydrostatic_kernel_constructor() result(self)
  return
end function hydrostatic_kernel_constructor

!> @brief Compute the pressure and geopotential gradient components of the momentum equation
!! @param[in] nlayers Number of layers
!! @param[inout] r_u Momentum equation right hand side
!! @param[in] exner Exner pressure field
!! @param[in] theta Potential temperature field
!! @param[in] phi Geopotential field
!! @param[in] ndf_w2 Number of degrees of freedom per cell for w2
!! @param[in] undf_w2 Number unique of degrees of freedom  for w2
!! @param[in] map_w2 Dofmap for the cell at the base of the column for w2
!! @param[in] w2_basis Basis functions evaluated at quadrature points 
!! @param[in] w2_diff_basis Differential of the basis functions evaluated at quadrature points
!! @param[in] ndf_w3 Number of degrees of freedom per cell for w3
!! @param[in] undf_w3 Number unique of degrees of freedom  for w3
!! @param[in] map_w3 Dofmap for the cell at the base of the column for w3
!! @param[in] w3_basis Basis functions evaluated at gaussian quadrature points 
!! @param[in] ndf_wt Number of degrees of freedom per cell for wt
!! @param[in] undf_wt Number unique of degrees of freedom  for wt
!! @param[in] map_wt Dofmap for the cell at the base of the column for wt
!! @param[in] wt_basis Basis functions evaluated at gaussian quadrature points 
!! @param[in] wt_diff_basis Differential of the basis functions evaluated at quadrature points
!! @param[in] ndf_w0 Number of degrees of freedom per cell for w0
!! @param[in] undf_w0 Number unique of degrees of freedom  for w0
!! @param[in] map_w0 Dofmap for the cell at the base of the column for w0
!! @param[in] w0_diff_basis Differential of the basis functions evaluated at quadrature points
!! @param[in] nqp_h Number of quadrature points in the horizontal
!! @param[in] nqp_v Number of quadrature points in the vertical
!! @param[in] wqp_h Horizontal quadrature weights
!! @param[in] wqp_v Vertical quadrature weights
subroutine hydrostatic_code(nlayers,                                          &
                            r_u, exner, theta, phi,                           &
                            ndf_w2, undf_w2, map_w2, w2_basis, w2_diff_basis, &
                            ndf_w3, undf_w3, map_w3, w3_basis,                &
                            ndf_wt, undf_wt, map_wt, wt_basis, wt_diff_basis, &
                            ndf_w0, undf_w0, map_w0, w0_diff_basis,           &
                            nqp_h, nqp_v, wqp_h, wqp_v                        &
                            )
  implicit none
                         
  ! Arguments
  integer, intent(in) :: nlayers,nqp_h, nqp_v
  integer, intent(in) :: ndf_wt, ndf_w2, ndf_w3, ndf_w0
  integer, intent(in) :: undf_wt, undf_w2, undf_w3, undf_w0
  integer, dimension(ndf_wt), intent(in) :: map_wt
  integer, dimension(ndf_w2), intent(in) :: map_w2
  integer, dimension(ndf_w3), intent(in) :: map_w3
  integer, dimension(ndf_w0), intent(in) :: map_w0

  real(kind=r_def), dimension(1,ndf_w3,nqp_h,nqp_v), intent(in) :: w3_basis  
  real(kind=r_def), dimension(3,ndf_w2,nqp_h,nqp_v), intent(in) :: w2_basis 
  real(kind=r_def), dimension(1,ndf_wt,nqp_h,nqp_v), intent(in) :: wt_basis 
  real(kind=r_def), dimension(1,ndf_w2,nqp_h,nqp_v), intent(in) :: w2_diff_basis
  real(kind=r_def), dimension(3,ndf_wt,nqp_h,nqp_v), intent(in) :: wt_diff_basis   
  real(kind=r_def), dimension(3,ndf_w0,nqp_h,nqp_v), intent(in) :: w0_diff_basis   

  real(kind=r_def), dimension(undf_w2), intent(inout) :: r_u
  real(kind=r_def), dimension(undf_w3), intent(in)    :: exner
  real(kind=r_def), dimension(undf_wt), intent(in)    :: theta
  real(kind=r_def), dimension(undf_w0), intent(in)    :: phi

  real(kind=r_def), dimension(nqp_h), intent(in)      ::  wqp_h
  real(kind=r_def), dimension(nqp_v), intent(in)      ::  wqp_v

  ! Internal variables
  integer               :: df, k 
  integer               :: qp1, qp2
  
  real(kind=r_def), dimension(ndf_w3)          :: exner_e
  real(kind=r_def), dimension(ndf_w2)          :: ru_e
  real(kind=r_def), dimension(ndf_wt)          :: theta_e
  real(kind=r_def), dimension(ndf_w0)          :: phi_e

  real(kind=r_def) :: grad_theta_at_quad(3), v(3)
  real(kind=r_def) :: exner_at_quad, theta_at_quad, &
                      grad_term, dv
  real(kind=r_def) :: grad_phi_at_quad(3)
  real(kind=r_def) :: geo_term
  
  do k = 0, nlayers-1
    do df = 1, ndf_w3
      exner_e(df) = exner( map_w3(df) + k )
    end do    
    do df = 1, ndf_wt
      theta_e(df) = theta( map_wt(df) + k )
    end do   
    do df = 1, ndf_w2
      ru_e(df) = 0.0_r_def
    end do 
    do df = 1, ndf_w0
      phi_e(df)   = phi(map_w0(df) + k)
    end do   
    ! Compute the RHS integrated over one cell    
    do qp2 = 1, nqp_v
      do qp1 = 1, nqp_h

        ! Gradient of the geopotential
        grad_phi_at_quad(:) = 0.0_r_def
        do df = 1, ndf_w0
          grad_phi_at_quad(:)   = grad_phi_at_quad(:) &
                                + phi_e(df)*w0_diff_basis(:,df,qp1,qp2) 
        end do

        ! Pressure gradient
        exner_at_quad = 0.0_r_def 
        do df = 1, ndf_w3
          exner_at_quad  = exner_at_quad + exner_e(df)*w3_basis(1,df,qp1,qp2) 
        end do
        theta_at_quad = 0.0_r_def
        grad_theta_at_quad(:) = 0.0_r_def
        do df = 1, ndf_wt
          theta_at_quad   = theta_at_quad                                      &
                          + theta_e(df)*wt_basis(1,df,qp1,qp2)
          grad_theta_at_quad(:) = grad_theta_at_quad(:) &
                                + theta_e(df)*wt_diff_basis(:,df,qp1,qp2) 

        end do

        do df = 1, ndf_w2
          v  = w2_basis(:,df,qp1,qp2)
          dv = w2_diff_basis(1,df,qp1,qp2)

          ! Pressure gradient term
          grad_term = cp*exner_at_quad * (                           & 
                      theta_at_quad * dv                             &
                    + dot_product( grad_theta_at_quad(:),v)          &
                                         )
          ! Geopotential term                                         
          geo_term = dot_product( grad_phi_at_quad(:), v)

          ru_e(df) = ru_e(df) +  wqp_h(qp1)*wqp_v(qp2)*(grad_term-geo_term)

        end do
      end do
    end do
    do df = 1, ndf_w2
      r_u( map_w2(df) + k ) =  r_u( map_w2(df) + k ) + ru_e(df)
    end do 
  end do
  
end subroutine hydrostatic_code

end module hydrostatic_kernel_mod
