!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief Computes boundary integral part of rhs of the momentum equation for
!>        the nonlinear equations.
!>
!> The kernel computes the boundary integral on rhs of the momentum equation
!> for the nonlinear equations, written in the vector invariant form.
!>
!> This consists of pressure_gradient_bd = -cp*theta*v*normal_vector*average(pi)
!>
!> where average(pi) needs to be considered as both exner and theta are
!> discontinuous in the horizontal direction.
!>
module pressure_gradient_bd_kernel_mod

  use argument_mod,             only : arg_type, func_type,       &
                                       mesh_data_type,            &
                                       GH_FIELD, GH_READ, GH_INC, &
                                       GH_BASIS,                  &
                                       GH_DIFF_BASIS, CELLS,      &
                                       GH_QUADRATURE_face,        &
                                       adjacent_face,             &
                                       reference_element_out_face_normal
  use constants_mod,            only : r_def, i_def
  use cross_product_mod,        only : cross_product
  use fs_continuity_mod,        only : W2, W3, Wtheta
  use kernel_mod,               only : kernel_type
  use planet_config_mod,        only : cp

  implicit none

  !-------------------------------------------------------------------------------
  ! Public types
  !-------------------------------------------------------------------------------
  !> The type declaration for the kernel. Contains the metadata needed by the Psy layer
  type, public, extends(kernel_type) :: pressure_gradient_bd_kernel_type
    private
    type(arg_type) :: meta_args(4) = (/                               &
      arg_type(GH_FIELD,   GH_INC,  W2),                              &
      arg_type(GH_FIELD,   GH_READ, W3),                              &
      arg_type(GH_FIELD,   GH_READ, Wtheta),                          &
      arg_type(GH_FIELD*3, GH_READ, Wtheta)                           &
      /)
    type(func_type) :: meta_funcs(3) = (/                             &
      func_type(W2, GH_BASIS, GH_DIFF_BASIS),                         &
      func_type(W3, GH_BASIS),                                        &
      func_type(Wtheta, GH_BASIS)                                     &
      /)
    integer :: iterates_over = CELLS
    integer :: gh_shape = GH_QUADRATURE_face
    type(mesh_data_type) :: meta_init(2) = (/               &
        mesh_data_type( adjacent_face ),                    &
        mesh_data_type( reference_element_out_face_normal ) &
      /)
  contains
    procedure, nopass ::pressure_gradient_bd_code
  end type

  !-------------------------------------------------------------------------------
  ! Constructors
  !-------------------------------------------------------------------------------

  ! Overload the default structure constructor for function space
  interface pressure_gradient_bd_kernel_type
    module procedure pressure_gradient_bd_kernel_constructor
  end interface

  !-------------------------------------------------------------------------------
  ! Contained functions/subroutines
  !-------------------------------------------------------------------------------
  public pressure_gradient_bd_code
contains

  type(pressure_gradient_bd_kernel_type) &
  function pressure_gradient_bd_kernel_constructor() result(self)
    implicit none
    return
  end function pressure_gradient_bd_kernel_constructor

  !> @brief Compute the boundary integral terms in the pressure gradient
  !! @param[in] nlayers Number of layers
  !! @param[in] ndf_w2 Number of degrees of freedom per cell for w2
  !! @param[in] undf_w2 Number unique of degrees of freedom  for w2
  !! @param[in] map_w2 Integer array holding the dofmap for the cell at the base of the column for w2
  !! @param[in] ndf_w3 Number of degrees of freedom per cell for w3
  !! @param[in] undf_w3 Number unique of degrees of freedom  for w3
  !! @param[in] stencil_w3_map W3 dofmaps for the stencil
  !! @param[in] stencil_w3_size Size of the W3 stencil (number of cells)
  !! @param[in] ndf_wtheta Number of degrees of freedom per cell for wtheta
  !! @param[in] undf_wtheta Number unique of degrees of freedom  for wtheta
  !! @param[in] wtheta_map Dofmap for the theta space
  !! @param[inout] r_u_bd Right hand side of the momentum equation
  !! @param[in] exner Exner pressure
  !! @param[in] theta Potential temperature    
  !! @param[in] moist_dyn_gas Gas factor (1 + m_v / epsilon)
  !! @param[in] moist_dyn_tot Total mass factor (1 + sum m_x)
  !! @param[in] moist_dyn_fac Water factor
  !! @param[in] nqp Number of quadrature points on each face
  !! @param[in] wqp quadrature weights
  !! @param[in] w2_basis_face Basis functions evaluated at gaussian quadrature points on horizontal faces
  !! @param[in] w3_basis_face Basis functions evaluated at gaussian quadrature points on horizontal faces
  !! @param[in] wtheta_basis_face Basis functions evaluated at gaussian quadrature points on horizontal faces
  !! @param[in] opposite_face Vector containing information on neighbouring face index for the current cell
  !! @param[in] out_face_normal Vector normal to the out faces of the
  !!                            reference element.
  !!
  subroutine pressure_gradient_bd_code( nlayers,                      &
                                        ndf_w2, undf_w2,              &
                                        map_w2,                       &
                                        ndf_w3, undf_w3,              &
                                        stencil_w3_map,               &
                                        stencil_w3_size,              &
                                        ndf_wtheta, undf_wtheta,      &
                                        wtheta_map,                   &
                                        r_u_bd,                       &
                                        exner, theta, moist_dyn_gas,  &
                                        moist_dyn_tot, moist_dyn_fac, &
                                        nqp, wqp,                     &
                                        w2_basis_face, w3_basis_face, &
                                        wtheta_basis_face,            &
                                        opposite_face, out_face_normal )

    implicit none

    ! Arguments
    integer(kind=i_def), intent(in) :: nlayers, nqp
    integer(kind=i_def), intent(in) :: ndf_w2, ndf_w3
    integer(kind=i_def), intent(in) :: undf_w2, undf_w3
    integer(kind=i_def), intent(in) :: ndf_wtheta, undf_wtheta
    integer(kind=i_def), dimension(ndf_w2), intent(in) :: map_w2

    integer(kind=i_def), intent(in) :: stencil_w3_size
    integer(kind=i_def), dimension(ndf_w3, stencil_w3_size), intent(in)  :: stencil_w3_map

    integer(kind=i_def), dimension(ndf_wtheta), intent(in)  :: wtheta_map

    real(kind=r_def), dimension(3,ndf_w2,nqp,4),     intent(in) :: w2_basis_face
    real(kind=r_def), dimension(1,ndf_w3,nqp,4),     intent(in) :: w3_basis_face
    real(kind=r_def), dimension(1,ndf_wtheta,nqp,4), intent(in) :: wtheta_basis_face

    integer(kind=i_def), intent(in) :: opposite_face(:)
    real(kind=r_def),    intent(in) :: out_face_normal(:,:)

    real(kind=r_def), dimension(undf_w2), intent(inout)     :: r_u_bd
    real(kind=r_def), dimension(undf_w3), intent(in)        :: exner
    real(kind=r_def), dimension(undf_wtheta), intent(in)    :: theta
    real(kind=r_def), dimension(undf_wtheta), intent(in)    :: moist_dyn_gas,       &
                                                               moist_dyn_tot,       &
                                                               moist_dyn_fac

    real(kind=r_def), dimension(nqp,4), intent(in)      ::  wqp

    ! Internal variables
    integer(kind=i_def)              :: df, k, face, face_next
    integer(kind=i_def)              :: qp

    real(kind=r_def), dimension(ndf_w3)     :: exner_e, exner_next_e
    real(kind=r_def), dimension(ndf_wtheta) :: theta_v_e
    real(kind=r_def), dimension(ndf_w2)     :: pressure_gradient_bd_e

    real(kind=r_def) :: v(3)
    real(kind=r_def) :: exner_av
    real(kind=r_def) :: theta_v_at_fquad, bdary_term

    do k = 0, nlayers-1

      do df = 1, ndf_w2
          pressure_gradient_bd_e(df) = 0.0_r_def
      end do
      do face = 1, size( opposite_face, 1 )

        ! Storing opposite face number on neighbouring cell
        face_next = opposite_face(face)

        ! Computing exner in local and adjacent cell
        do df = 1, ndf_w3
          exner_e(df)      = exner( stencil_w3_map(df, 1) + k )
          exner_next_e(df) = exner( stencil_w3_map(df, face+1) + k )
        end do

        ! Computing theta in local cell
        do df = 1, ndf_wtheta
          theta_v_e(df) = theta( wtheta_map(df) + k ) * moist_dyn_gas( wtheta_map(df) + k ) / &
                                                        moist_dyn_tot( wtheta_map(df) + k )
        end do

        ! Compute the boundary RHS integrated over one horizontal face
        do qp = 1, nqp
          exner_av = 0.0_r_def
          do df = 1, ndf_w3
            exner_av = exner_av + 0.5_r_def*(exner_e(df)     *w3_basis_face(1,df,qp,face) &
                                           + exner_next_e(df)*w3_basis_face(1,df,qp,face_next))
          end do

          theta_v_at_fquad = 0.0_r_def
          do df = 1, ndf_wtheta
            theta_v_at_fquad = theta_v_at_fquad + theta_v_e(df)*wtheta_basis_face(1,df,qp,face)
          end do

          do df = 1, ndf_w2
            v  = w2_basis_face(:,df,qp,face)

            bdary_term = - cp * dot_product(v, out_face_normal(:, face)) *  theta_v_at_fquad * exner_av
            pressure_gradient_bd_e(df) = pressure_gradient_bd_e(df) + wqp(qp,face) * bdary_term
          end do

        end do ! qp
      end do ! faces

      do df = 1, ndf_w2
        r_u_bd( map_w2(df) + k ) =  r_u_bd( map_w2(df) + k ) + pressure_gradient_bd_e(df)
      end do

    end do ! layers

  end subroutine pressure_gradient_bd_code

end module pressure_gradient_bd_kernel_mod
