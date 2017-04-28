!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!> @brief A module providing scalar related classes.
!>
!> @details A representation of a scalar which provides both easy access to the
!> scalar data and a method by which the PSy layer can access the distributed
!> memory aspects of the scalar


module scalar_mod

  use constants_mod,      only: r_def, i_def
  use ESMF

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------

  !> PSy layer representation of a scalar.
  !>
  type, public :: scalar_type
    private

    !> The value of the scalar
    real(kind=r_def), public :: value

  contains

    !> Perform a global sum operation on the scalar
    !> @return The global sum of the scalar values over all ranks
    procedure, public :: get_sum

    !> Calculate the global minimum of the scalar
    !> @return The minimum of the scalar values over all ranks
    procedure, public :: get_min

    !> Calculate the global maximum of the scalar
    !> @return The maximum of the scalar values over all ranks
    procedure, public :: get_max

    !> Wait (i.e. block) until all current non-blocking reductions
    !> (sum, max, min) are complete.
    !>
    !> ESMF have only implemented blocking reductions, so this
    !> subroutine currently returns without waiting.
    procedure reduction_finish

  end type scalar_type

  interface scalar_type
    module procedure scalar_constructor
  end interface

contains

  !> Construct a <code>scalar_type</code> object.
  !>
  !> @param [in] value The value with which to initialize the scalar
  !> @return self the field
  !>
  function scalar_constructor(value) result(self)

    implicit none
    
    real(kind=r_def), intent(in) :: value
    type(scalar_type), target :: self

    self%value = value

  end function scalar_constructor

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------

  !! Start performing a global sum operation on a scalar
  !!
  function get_sum(self) result (global_sum)
    class(scalar_type), intent(in) :: self

    real(r_def) :: global_sum

    type(ESMF_VM) :: vm
    integer(i_def) :: rc

    call ESMF_VMGetCurrent(vm=vm, rc=rc)
! Currently ESMF has only implemented blocking reductions. Using anything
! other than ESMF_SYNC_BLOCKING for syncflag results in an error
    call ESMF_VMAllFullReduce(vm, &
                              [self%value], &
                              global_sum, &
                              1, &
                              ESMF_REDUCE_SUM, &
                              syncflag = ESMF_SYNC_BLOCKING, &
                              rc=rc)
  end function get_sum

  !! Start the calculation of the global minimum of a scalar
  !!
  function get_min(self) result (global_min)
    class(scalar_type), intent(in) :: self

    real(r_def) :: global_min

    type(ESMF_VM) :: vm
    integer(i_def) :: rc

    call ESMF_VMGetCurrent(vm=vm, rc=rc)
! Currently ESMF has only implemented blocking reductions. Using anything
! other than ESMF_SYNC_BLOCKING for syncflag results in an error
    call ESMF_VMAllFullReduce(vm, &
                              [self%value], &
                              global_min, &
                              1, &
                              ESMF_REDUCE_MIN, &
                              syncflag = ESMF_SYNC_BLOCKING, &
                              rc=rc)
  end function get_min

  !! Start the calculation of the global maximum of a scalar
  !!
  function get_max(self) result (global_max)

    class(scalar_type), intent(in) :: self

    real(r_def) :: global_max

    type(ESMF_VM) :: vm
    integer(i_def) :: rc

    call ESMF_VMGetCurrent(vm=vm, rc=rc)
! Currently ESMF has only implemented blocking reductions. Using anything
! other than ESMF_SYNC_BLOCKING for syncflag results in an error
    call ESMF_VMAllFullReduce(vm, &
                              [self%value], &
                              global_max, &
                              1, &
                              ESMF_REDUCE_MAX, &
                              syncflag = ESMF_SYNC_BLOCKING, &
                              rc=rc)
  end function get_max

  !! Wait for any current (non-blocking) reductions (sum, max, min) to complete
  !!
  !! Currently, ESMF has only implemented blocking reductions, so there is
  !! no need to ever call this subroutine. It is left in here to complete the
  !! API so when non-blocking reductions are implemented, we can support them
  subroutine reduction_finish(self)

    class(scalar_type), intent(in) :: self

    type(ESMF_VM)  :: vm
    integer(i_def) :: rc
    real(r_def)    ::  value_tmp

    value_tmp=self%value            ! reduction_finish currently does nothing.
                                    ! The "self" that is passed in automatically
                                    ! to a type-bound subroutine is not used -
                                    ! so the compilers complain -  have to use
                                    ! it for something harmless.

    call ESMF_VMGetCurrent(vm=vm, rc=rc)
    call ESMF_VMCommWaitAll(vm=vm, rc=rc)

  end subroutine reduction_finish

end module scalar_mod
