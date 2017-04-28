!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!>@brief Abstract vector type for the new solver API and 
!! extended vector types for particular solvers.

module vector_mod
  use constants_mod, only : i_def, r_def 

  implicit none
  private

  !> @brief abstract vector type to define the interfaces for the solver API
  type, public, abstract :: abstract_vector_type
     private
   contains
     procedure (set_scalar_interface),    deferred :: set_scalar
     procedure (axpy_interface),          deferred :: axpy
     procedure (norm_interface),          deferred :: norm
     procedure (dot_interface),           deferred :: dot
     procedure (scale_interface),    deferred :: scale
  end type abstract_vector_type

  abstract interface
     subroutine set_scalar_interface(self, scalar)
       import :: abstract_vector_type
       import :: r_def
       class(abstract_vector_type), intent(inout) :: self
       real(kind=r_def),            intent(in)    :: scalar
     end subroutine set_scalar_interface
  end interface

  abstract interface
     subroutine axpy_interface(self, alpha, x)
       import :: abstract_vector_type
       import :: r_def
       class(abstract_vector_type), intent(inout) :: self
       real(kind=r_def),            intent(in)    :: alpha
       class(abstract_vector_type), intent(inout) :: x
     end subroutine axpy_interface
  end interface

  abstract interface
     function norm_interface(self) result(normal)
       import :: abstract_vector_type
       import :: r_def
       class(abstract_vector_type), intent(in) :: self
       real(kind=r_def) :: normal
     end function  norm_interface
  end interface

    abstract interface
     function dot_interface(self, x) result(dot_prod)
       import :: abstract_vector_type
       import :: r_def
       class(abstract_vector_type), intent(in) :: self
       class(abstract_vector_type), intent(in) :: x
       real(kind=r_def)                        :: dot_prod
     end function dot_interface
  end interface

    abstract interface
     subroutine scale_interface(self, scalar)
       import :: abstract_vector_type
       import :: r_def
       class(abstract_vector_type), intent(inout) :: self
       real(kind=r_def),            intent(in)    :: scalar
     end subroutine scale_interface
  end interface

  
end module vector_mod
