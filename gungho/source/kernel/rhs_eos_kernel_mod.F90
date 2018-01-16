!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Computes rhs of the equation of state for the nonlinear equations 

!> @details The kernel computes the lhs of the equation of state for the nonlinear equations, 
!>          That is: \f[ rhs_{\Pi} = 1 - p0/Rd * exner ^ (1-kappa)/kappa /(rho*theta) \f]
module rhs_eos_kernel_mod
use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                     &
                                    GH_FIELD, GH_READ, GH_WRITE,             &
                                    ANY_SPACE_1, W3, ANY_SPACE_9,            &
                                    GH_BASIS, GH_DIFF_BASIS,                 &
                                    CELLS, GH_QUADRATURE_XYoZ
use constants_mod,           only : r_def, i_def

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: rhs_eos_kernel_type
  private
  type(arg_type) :: meta_args(5) = (/                                  &
       arg_type(GH_FIELD,   GH_WRITE, W3),                             &
       arg_type(GH_FIELD,   GH_READ,  W3),                             &
       arg_type(GH_FIELD,   GH_READ,  W3),                             &
       arg_type(GH_FIELD,   GH_READ,  ANY_SPACE_1),                    &
       arg_type(GH_FIELD*3, GH_READ,  ANY_SPACE_9)                     &
       /)
  type(func_type) :: meta_funcs(3) = (/                                &
       func_type(W3,          GH_BASIS),                               &
       func_type(ANY_SPACE_1, GH_BASIS),                               &
       func_type(ANY_SPACE_9, GH_DIFF_BASIS)                           &
       /)
  integer :: iterates_over = CELLS
  integer :: gh_shape = GH_QUADRATURE_XYoZ
contains
  procedure, nopass ::rhs_eos_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! Overload the default structure constructor for function space
interface rhs_eos_kernel_type
   module procedure rhs_eos_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public rhs_eos_code
contains

type(rhs_eos_kernel_type) function rhs_eos_kernel_constructor() result(self)
  return
end function rhs_eos_kernel_constructor

!> @brief Computes lhs of the equation of state for the nonlinear equations 
!! @param[in] nlayers Number of layers
!! @param[inout] rhs_eos rhs array for the equation of state
!! @param[in] exner pressure 
!! @param[in] rho Density
!! @param[in] theta Potential temperature
!! @param[in] chi1 First coordinate array
!! @param[in] chi2 Second coordinate array
!! @param[in] chi3 Third coordinate array
!! @param[in] ndf_w3 Number of degrees of freedom per cell for w3
!! @param[in] undf_w3 Number of (local) unique degrees of freedom
!! @param[in] map_w3 Dofmap for the cell at the base of the column for w3
!! @param[in] w3_basis Basis functions evaluated at quadrature points 
!! @param[in] ndf_wt Number of degrees of freedom per cell for wt
!! @param[in] undf_wt Number of (local) unique degrees of freedom
!! @param[in] map_wt Dofmap for the cell at the base of the column for wt
!! @param[in] wt_basis Basis functions evaluated at quadrature points 
!! @param[in] ndf_chi Number of degrees of freedom per cell for chi
!! @param[in] undf_chi Number of (local) unique degrees of freedom for chi
!! @param[in] map_chi Dofmap for the cell at the base of the column for chi
!! @param[in] chi_diff_basis Differential basis functions evaluated at quadrature points 
!! @param[in] nqp_h Number of quadrature points in the horizontal
!! @param[in] nqp_v Number of quadrature points in the vertical
!! @param[in] wqp_h Horizontal quadrature weights
!! @param[in] wqp_v Vertical quadrature weights
subroutine rhs_eos_code(nlayers,                                         &
                        rhs_eos, exner, rho, theta,                      &
                        chi1, chi2, chi3,                                &
                        ndf_w3, undf_w3, map_w3, w3_basis,               &
                        ndf_wt, undf_wt, map_wt, wt_basis,               &
                        ndf_chi, undf_chi, map_chi, chi_diff_basis,      &
                        nqp_h, nqp_v, wqp_h, wqp_v)

  use coordinate_jacobian_mod,  only: coordinate_jacobian
  use planet_config_mod,        only: kappa, Rd, p_zero

  implicit none
  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers, nqp_h, nqp_v
  integer(kind=i_def), intent(in) :: ndf_wt, ndf_w3, ndf_chi
  integer(kind=i_def), intent(in) :: undf_wt, undf_w3, undf_chi
  integer(kind=i_def), dimension(ndf_w3),  intent(in) :: map_w3
  integer(kind=i_def), dimension(ndf_wt),  intent(in) :: map_wt
  integer(kind=i_def), dimension(ndf_chi), intent(in) :: map_chi

  real(kind=r_def), dimension(1,ndf_w3,nqp_h,nqp_v),  intent(in) :: w3_basis
  real(kind=r_def), dimension(1,ndf_wt,nqp_h,nqp_v),  intent(in) :: wt_basis
  real(kind=r_def), dimension(3,ndf_chi,nqp_h,nqp_v), intent(in) :: chi_diff_basis

  real(kind=r_def), dimension(undf_w3), intent(inout) :: rhs_eos
  real(kind=r_def), dimension(undf_wt), intent(in)    :: theta
  real(kind=r_def), dimension(undf_w3), intent(in)    :: rho, exner
  real(kind=r_def), dimension(undf_chi), intent(in)   :: chi1, chi2, chi3

  real(kind=r_def), dimension(nqp_h), intent(in)      ::  wqp_h
  real(kind=r_def), dimension(nqp_v), intent(in)      ::  wqp_v

  ! Internal variables
  integer(kind=i_def) :: df, k 
  integer(kind=i_def) :: qp1, qp2
  
  real(kind=r_def), dimension(ndf_wt)  :: theta_e
  real(kind=r_def), dimension(ndf_chi) :: chi1_e, chi2_e, chi3_e
  real(kind=r_def), dimension(ndf_w3)  :: rho_e, exner_e
  real(kind=r_def)                     :: rho_quad, theta_quad, exner_quad
  real(kind=r_def)                             :: eos, p0_over_rd, onemk_over_k
  real(kind=r_def), dimension(nqp_h,nqp_v)     :: dj
  real(kind=r_def), dimension(3,3,nqp_h,nqp_v) :: jac

  p0_over_rd = p_zero/Rd
  onemk_over_k = (1.0_r_def - kappa)/kappa


  do k = 0, nlayers-1
    do df = 1, ndf_chi
      chi1_e(df) = chi1(map_chi(df) + k)
      chi2_e(df) = chi2(map_chi(df) + k)
      chi3_e(df) = chi3(map_chi(df) + k)
    end do
    call coordinate_jacobian(ndf_chi, nqp_h, nqp_v, chi1_e, chi2_e, chi3_e,  &
                             chi_diff_basis, jac, dj)
    do df = 1, ndf_wt
      theta_e(df) = theta(map_wt(df) + k)
    end do
    do df = 1, ndf_w3
      exner_e(df)  = exner(map_w3(df) + k)
      rho_e(df)    = rho(map_w3(df) + k)
      rhs_eos(map_w3(df)+k) = 0.0_r_def
    end do
    do qp2 = 1, nqp_v
      do qp1 = 1, nqp_h
        theta_quad = 0.0_r_def
        do df = 1, ndf_wt
          theta_quad = theta_quad + theta_e(df)*wt_basis(1,df,qp1,qp2)
        end do
        exner_quad = 0.0_r_def
        rho_quad = 0.0_r_def
        do df = 1, ndf_w3
          exner_quad = exner_quad + exner_e(df)*w3_basis(1,df,qp1,qp2)
          rho_quad   = rho_quad   + rho_e(df)  *w3_basis(1,df,qp1,qp2)
        end do
        eos = 1.0_r_def - p0_over_rd * exner_quad**onemk_over_k &
            /(rho_quad*theta_quad)
        eos = wqp_h(qp1)*wqp_v(qp2)*dj(qp1,qp2)*eos

        do df = 1, ndf_w3          
          rhs_eos(map_w3(df)+k) = rhs_eos(map_w3(df)+k) &
                                - w3_basis(1,df,qp1,qp2)*eos
        end do
      end do
    end do
  end do
  
end subroutine rhs_eos_code

end module rhs_eos_kernel_mod
