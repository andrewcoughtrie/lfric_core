!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!> @brief Algorithm to process and dump fields to file
module output_alg_mod

  use constants_mod,                     only: r_def, str_max_filename, i_def
  use interpolated_output_mod,           only: interpolated_output
  use function_space_collection_mod,     only: function_space_collection
  use function_space_mod,                only: function_space_type
  use field_mod,                         only: field_type
  use finite_element_config_mod,         only: element_order
  use fs_continuity_mod,                 only: W0, W3, Wtheta
  use galerkin_projection_algorithm_mod, only: galerkin_projection_algorithm
  use nodal_output_alg_mod,              only: nodal_output_alg
  use operator_mod,                      only: operator_type
  use output_config_mod,                 only: write_nodal_output,        &
                                               write_interpolated_output, &
                                               diag_stem_name,            &
                                               output_projections_on_w3
  use quadrature_mod,                    only: quadrature_type, GAUSSIAN
  use mesh_mod,                          only: mesh_type
  use mesh_collection_mod,               only: mesh_collection
  use runtime_constants_mod,             only: get_coordinates

  implicit none

  private
  public :: output_alg

contains

!> @brief Algorithm to process and dump fields to file
!> @details Writes field to a .m formated file indexed by a 
!>          timestep stamp either by dumping the values on
!>          nodal points or by sampling/interpolating the field on a regular grid.
!>          For interpolating vector fields the components are first projected into a
!>          continuous function space before they are interpolated
!> @param[in] field_name Character giving the name to be applied to the field
!>            file
!> @param[in] n Time step index
!> @param[inout] field Field to output 
!> @param[in] mesh_id  Id of the mesh all fields are on
  subroutine output_alg(field_name,n, field, mesh_id)

    implicit none

    character(len=*),    intent(in)    :: field_name
    integer(i_def),      intent(in)    :: n
    type(field_type),    intent(inout) :: field
    integer(i_def),      intent(in)    :: mesh_id

    ! output variables
    type(field_type),    pointer       :: chi(:) => null()
    type(field_type), allocatable      :: projected_field(:)
    type(quadrature_type)              :: qr
    type(mesh_type ), pointer          :: mesh => null()
    character(len=str_max_filename)    :: fname
    character(len=str_max_filename)    :: rank_name
    integer(kind=i_def)                :: d, dir, fs_handle
    type(function_space_type), pointer :: fs
    character(len=1)                   :: uchar

    ! Determine the rank and set rank_name
    ! No rank name appended for a serial run
    mesh => mesh_collection%get_mesh( mesh_id )
    if ( mesh%get_total_ranks() == 1 ) then
      rank_name=".m"
    else
      write( rank_name, "("".Rank"", I6.6, A)") mesh%get_local_rank(), ".m"
    end if

    ! Compute projections
    if ( write_interpolated_output .or. output_projections_on_w3) then
      qr = quadrature_type(element_order+3, GAUSSIAN)

      ! Vector or Scalar space?
      fs_handle = field%which_function_space()    
      fs => field%get_function_space()
      d = fs%get_dim_space()
      allocate( projected_field(d) )
      ! If its a vector field project to W0 otherwise just copy
      if ( d > 1 ) then
        ! Create fields needed for output (these can be in CG or DG space)
        do dir = 1,d
          projected_field(dir) = field_type( vector_space = &
             function_space_collection%get_fs(mesh_id,element_order, W0) )
        end do
        call galerkin_projection_algorithm(projected_field, field, mesh_id, &
           d, qr)
      else
        projected_field(1) = field
      end if
    end if

    ! Write interpolated output
    if ( write_interpolated_output ) then
      chi  => get_coordinates()
      fname=trim(ts_fname("interp_",field_name,n, rank_name))
      call interpolated_output(d, projected_field(1:d), mesh_id, chi, &
                               fname)
    end if
      
    ! Compute output on nodal points of the field itself
    if ( write_nodal_output ) then  
      fname=trim(ts_fname("nodal_", field_name, n, rank_name))
      call nodal_output_alg(field, fname)
      if (output_projections_on_w3)then
        if (d > 1)then ! For vectors (i.e. winds) output individual components
          do dir = 1,d
            write(uchar,'(i1)') dir
            fname=trim(ts_fname("nodal_"//uchar, field_name, n, rank_name))
            call nodal_output_alg(projected_field(dir), fname)
          end do
        end if
        
        ! Let's also output fields from theta space as a w3 projection
        if (fs_handle == Wtheta)then
          projected_field(1) = field_type( vector_space = &
             function_space_collection%get_fs(mesh_id,element_order, W3) )
          call galerkin_projection_algorithm(projected_field(1), field, mesh_id, &
             d, qr)
          fname=trim(ts_fname("nodal_w3projection_", field_name, n, rank_name))
          call nodal_output_alg(projected_field(1), fname)
        end if
      end if
    end if

    if (allocated(projected_field))deallocate( projected_field )

  end subroutine output_alg

  ! Private function to determine diagnostic output filename at a given timestep
  function ts_fname(file_type, field_name, ts, rank_name)

    character(len=*),    intent(in) :: field_name, file_type
    integer,             intent(in) :: ts
    character(len=*),    intent(in) :: rank_name
    character(len=str_max_filename) :: ts_fname
    write(ts_fname,'(A,A,A,A,A,I6.6,A)') trim(diag_stem_name),"_", &
         trim(file_type),trim(field_name),"_T",ts,trim(rank_name)

  end function ts_fname

end module output_alg_mod

