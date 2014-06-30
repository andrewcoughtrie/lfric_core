
!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!> @brief A module providing field related classes.
!>
!> @detail Both the full fat field representation and a proxy with access to the
!! field elements are defined here.

module field_mod

  use constants_mod,            only : dp
  use function_space_mod,       only : function_space_type
  use gaussian_quadrature_mod,  only : gaussian_quadrature_type

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------

  !> Algorithm layer representation of a field.
  !>
  !> Objects of this type hold all the data of the field privately.
  !> Unpacking the data is done via the proxy type accessed by the Psy layer
  !> alone.
  !>
  type, public :: field_type
    private
    !> The number of layers 
    integer :: nlayers
    !> The number of unique degrees of freedom
    integer :: undf
    !> The number of cells from the underlying function space
    integer :: ncell
    !> Each field has a pointer to the function space on which it lives
    type( function_space_type ), pointer         :: vspace => null( )
    !> Each field has a pointer to the gaussian quadrature rule which will be
    !! used to integrate over its values
    type( gaussian_quadrature_type ), pointer         &
                                      :: gaussian_quadrature => null( )
    !> Allocatable array of type real which holds the values of the field
    real(kind=dp), allocatable         :: data( : )

  contains

    !> Function to get a proxy with public pointers to the data in a
    !! field_type. 
    procedure, public :: get_proxy

    !> Sends the field contents to the log
    !! @param[in] title A title added to the log before the data is written out
    !>
    procedure, public :: print_field

    procedure         :: minus_field_data
    procedure         :: copy_field_data
    procedure         :: set_field_scalar
    procedure         :: axpy

  end type field_type

  interface field_type

    module procedure field_constructor, field_data_copy_constructor

  end interface

  !> Psy layer representation of a field.
  !>
  !> This is a proxy to the actual field description with each element accessed
  !> via a public pointer.
  !>
  type, public :: field_proxy_type 

    private

    !> The number of layers 
    integer, public  :: nlayers
    !> The number of unique degrees of freedom
    integer, public  :: undf
    !> The number of cells from the underlying function space
    integer, public  :: ncell
    !> Each field has a pointer to the function space on which it lives
    type( function_space_type ), pointer, public :: vspace
    !> Each field has a pointer to the gaussian quadrature rule which will be
    !! used to integrate over its values
    type( gaussian_quadrature_type ), pointer, public &
                                      :: gaussian_quadrature
    !> Allocatable array of type real which holds the values of the field
    real(kind=dp), public, pointer                      :: data( : )

  contains
  end type field_proxy_type 

contains

  !> Function to create a proxy with access to the data in the field_type.
  !>
  !> @return The proxy type with public pointers to the elements of
  !> field_type
  type(field_proxy_type ) function get_proxy(self)
    implicit none
    class(field_type), target, intent(in)  :: self

    get_proxy % nlayers                =  self % nlayers
    get_proxy % undf                   =  self % undf
    get_proxy % ncell                  =  self % ncell
    get_proxy % vspace                 => self % vspace
    get_proxy % gaussian_quadrature    => self % gaussian_quadrature
    get_proxy %  data                  => self % data

  end function get_proxy

  !> Construct a <code>field_type</code> object.
  !>
  !> @param [in] vector_space the function space that the field lives on
  !> @param [in] gq the gaussian quadrature rule
  !> @param [in] num_layers integer number of layers for the field
  !> @return self the field
  !>
  function field_constructor( vector_space, &
                                   gq,           &
                                   num_layers) result(self)

    type(function_space_type), target, intent(in) :: vector_space
    type(gaussian_quadrature_type), target, intent(in) :: gq
    integer, intent(in) :: num_layers

    type(field_type), target :: self

    self%vspace => vector_space
    self%gaussian_quadrature => gq
    self%nlayers = num_layers
    self%ncell = self%vspace%get_ncell()
    self%undf = self%vspace%get_undf()

    ! allocate the array in memory
    allocate(self%data(self%undf))

  end function field_constructor

  function field_data_copy_constructor( field ) result (self)
    class( field_type ), intent(in) :: field
    type(  field_type )             :: self
	
    self%vspace => field%vspace
    self%gaussian_quadrature => field%gaussian_quadrature
    self%nlayers = field%nlayers
    self%undf = field%vspace%get_undf()
    self%ncell = field%ncell
    allocate( self%data(self%undf) )
    
  end function field_data_copy_constructor

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------

  !> Sends the field contents to the log
  !! @param[in] title A title added to the log before the data is written out
  !>
  subroutine print_field( self, title )

    use log_mod, only : log_event, log_scratch_space, LOG_LEVEL_DEBUG

    implicit none

    class( field_type ), target, intent( in ) :: self

    character( * ),          intent( in ) :: title

    integer                   :: cell
    integer                   :: layer
    integer                   :: df
    integer,          pointer :: map( : )

    call log_event( title, LOG_LEVEL_DEBUG )

    do cell=1,self%ncell
     map => self%vspace%get_cell_dofmap( cell )
      do df=1,self%vspace%get_ndf()
        do layer=0,self%nlayers-1
          write( log_scratch_space, '( I4, I4, I4, F8.2 )' ) &
              cell, df, layer+1, self%data( map( df ) + layer )
          call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
        end do
      end do
    end do

  end subroutine print_field

  subroutine minus_field_data(self, a, b)
    implicit none
    class(field_type), intent(inout) :: self
    class(field_type), intent(in)    :: a
    class(field_type), intent(in)    :: b

    self%data(:) = a%data(:) - b%data(:)
  end subroutine minus_field_data

  subroutine copy_field_data(self, fdata)
    implicit none
    class(field_type), intent(inout) :: self
    class(field_type), intent(in)    :: fdata
    
    self%data(:) = fdata%data(:)
  end subroutine copy_field_data

  subroutine set_field_scalar(self, scalar)
    implicit none
    class(field_type), intent(inout) :: self
    real(kind=dp)                    :: scalar
    
    self%data(:) = scalar
  end subroutine set_field_scalar

  subroutine axpy(self, a,x,y)
    implicit none
    class(field_type), intent(inout):: self
    real(kind=dp),     intent(in)   :: a
    class(field_type), intent(in)   :: x
    class(field_type), intent(in)   :: y
    
    self%data(:) = a*x%data(:) + y%data(:)
  end subroutine axpy

end module field_mod
