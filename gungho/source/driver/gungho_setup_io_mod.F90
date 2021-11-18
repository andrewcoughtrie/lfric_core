!-------------------------------------------------------------------------------
! (C) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-------------------------------------------------------------------------------
!> @brief Sets up I/O configuration from within GungHo.
!>
!> @details Collects configuration information relevant for the I/O subsystem
!>          and formats it so that it can be passed to the infrastructure
!>
module gungho_setup_io_mod

  use clock_mod,                 only: clock_type
  use constants_mod,             only: i_def, str_def, str_max_filename
  use lfric_xios_file_mod,       only: xios_file_type
  use linked_list_mod,           only: linked_list_type, &
                                       linked_list_item_type

  ! Configuration modules
  use files_config_mod,          only: ancil_directory,           &
                                       checkpoint_stem_name,      &
                                       land_area_ancil_path,      &
                                       orography_ancil_path,      &
                                       aerosols_ancil_path,       &
                                       albedo_nir_ancil_path,     &
                                       albedo_vis_ancil_path,     &
                                       hydtop_ancil_path,         &
                                       ozone_ancil_path,          &
                                       plant_func_ancil_path,     &
                                       sea_ancil_path,            &
                                       sea_ice_ancil_path,        &
                                       soil_ancil_path,           &
                                       sst_ancil_path,            &
                                       surface_frac_ancil_path,   &
                                       start_dump_filename,       &
                                       start_dump_directory,      &
                                       lbc_filename,              &
                                       lbc_directory
  use initialization_config_mod, only: init_option,               &
                                       init_option_fd_start_dump, &
                                       ancil_option,              &
                                       ancil_option_start_dump,   &
                                       ancil_option_fixed,        &
                                       ancil_option_updating,     &
                                       lbc_option,                &
                                       lbc_option_file
  use io_config_mod,             only: diagnostic_frequency,      &
                                       checkpoint_write,          &
                                       checkpoint_read,           &
                                       write_dump
  use io_context_mod,            only: io_context_type
  use orography_config_mod,      only: orog_init_option,          &
                                       orog_init_option_ancil
  use time_config_mod,           only: timestep_start,            &
                                       timestep_end
#ifdef UM_PHYSICS
  use surface_config_mod,        only: sea_alb_var_chl, albedo_obs
#endif

  implicit none

  private
  public :: init_gungho_files

  contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Adds details of all files-of-interest to a list.
  !>
  !> @param[in,out] files_list  Collection of xios_file_type objects.
  !> @param[in]     clock       Model time.
  !>
  subroutine init_gungho_files( files_list, clock )

    implicit none

    type(linked_list_type), intent(inout) :: files_list
    class(clock_type),      intent(in)    :: clock

    type(xios_file_type)            :: tmp_file
    character(len=str_max_filename) :: checkpoint_write_fname, &
                                       checkpoint_read_fname,  &
                                       dump_fname,             &
                                       ancil_fname,            &
                                       lbc_fname

    ! Setup diagnostic output file
    call tmp_file%init_xios_file("lfric_diag", freq=diagnostic_frequency)
    call files_list%insert_item(tmp_file)

    ! Setup diagnostic averages output
    call tmp_file%init_xios_file("lfric_averages", freq=clock%get_last_step() )
    call files_list%insert_item(tmp_file)

    ! Setup dump-writing context information
    if ( write_dump ) then
      ! Create dump filename from base name and end timestep
      write(dump_fname,'(A,A,I6.6)') &
         trim(start_dump_directory)//'/'//trim(start_dump_filename),"_", &
         clock%get_last_step()

      ! Setup dump file for end timestep
      call tmp_file%init_xios_file( "lfric_fd_dump", dump_fname, &
                                    clock%get_last_step())
      call files_list%insert_item(tmp_file)
    end if

    ! Setup dump-reading context information
    if( init_option == init_option_fd_start_dump .or. &
        ancil_option == ancil_option_start_dump ) then
      ! Create dump filename from stem
      write(dump_fname,'(A)') trim(start_dump_directory)//'/'// &
                              trim(start_dump_filename)

      ! Setup dump file
      call tmp_file%init_xios_file( "read_lfric_fd_dump", path=dump_fname)
      call files_list%insert_item(tmp_file)
    end if

#ifdef UM_PHYSICS
    ! Setup ancillary files
    if( ancil_option == ancil_option_fixed .or. &
        ancil_option == ancil_option_updating ) then
      ! Set land area ancil filename from namelist
      write(ancil_fname,'(A)') trim(ancil_directory)//'/'// &
                               trim(land_area_ancil_path)
      call tmp_file%init_xios_file("land_area_ancil", path=ancil_fname)
      call files_list%insert_item(tmp_file)

      ! Set soil ancil filename from namelist
      write(ancil_fname,'(A)') trim(ancil_directory)//'/'// &
                               trim(soil_ancil_path)
      call tmp_file%init_xios_file("soil_ancil", path=ancil_fname)
      call files_list%insert_item(tmp_file)

      ! Set plant functional type ancil filename from namelist
      write(ancil_fname,'(A)') trim(ancil_directory)//'/'// &
                               trim(plant_func_ancil_path)
      call tmp_file%init_xios_file("plant_func_ancil", path=ancil_fname)
      call files_list%insert_item(tmp_file)

      ! Set sea chlorophyll ancil filename from namelist
      if ( sea_alb_var_chl ) then
        write(ancil_fname,'(A)') trim(ancil_directory)//'/'// &
                                 trim(sea_ancil_path)
        call tmp_file%init_xios_file("sea_ancil", path=ancil_fname)
        call files_list%insert_item(tmp_file)
      end if

      ! Set sea surface temperature ancil filename from namelist
      write(ancil_fname,'(A)') trim(ancil_directory)//'/'// &
                               trim(sst_ancil_path)
      call tmp_file%init_xios_file("sst_ancil", path=ancil_fname)
      call files_list%insert_item(tmp_file)

      ! Set sea ice ancil filename from namelist
      write(ancil_fname,'(A)') trim(ancil_directory)//'/'// &
                               trim(sea_ice_ancil_path)
      call tmp_file%init_xios_file("sea_ice_ancil", path=ancil_fname)
      call files_list%insert_item(tmp_file)

      if ( albedo_obs ) then
        ! Set albedo_vis ancil filename from namelist
        write(ancil_fname,'(A)') trim(ancil_directory)//'/'// &
                                 trim(albedo_vis_ancil_path)
        call tmp_file%init_xios_file("albedo_vis_ancil", path=ancil_fname)
        call files_list%insert_item(tmp_file)

        ! Set albedo_nir ancil filename from namelist
        write(ancil_fname,'(A)') trim(ancil_directory)//'/'// &
                                 trim(albedo_nir_ancil_path)
        call tmp_file%init_xios_file("albedo_nir_ancil", path=ancil_fname)
        call files_list%insert_item(tmp_file)
      end if

      ! Set topmodel hydrology filename from namelist
      write(ancil_fname,'(A)') trim(ancil_directory)//'/'// &
                               trim(hydtop_ancil_path)
      call tmp_file%init_xios_file("hydtop_ancil", path=ancil_fname)
      call files_list%insert_item(tmp_file)

      ! Set ozone filename from namelist
      write(ancil_fname,'(A)') trim(ancil_directory)//'/'// &
                               trim(ozone_ancil_path)
      call tmp_file%init_xios_file("ozone_ancil", path=ancil_fname)
      call files_list%insert_item(tmp_file)

      ! Set surface fraction ancil filename from namelist
      write(ancil_fname,'(A)') trim(ancil_directory)//'/'// &
                               trim(surface_frac_ancil_path)
      call tmp_file%init_xios_file("surface_frac_ancil", path=ancil_fname)
      call files_list%insert_item(tmp_file)

      ! Set aerosol ancil filename from namelist
      write(ancil_fname,'(A)') trim(ancil_directory)//'/'// &
                               trim(aerosols_ancil_path)
      call tmp_file%init_xios_file("aerosols_ancil", path=ancil_fname)
      call files_list%insert_item(tmp_file)

    end if
#endif

    ! Setup orography ancillary file
    if( ( orog_init_option == orog_init_option_ancil ) .or. &
      ( ancil_option == ancil_option_fixed .or. &
        ancil_option == ancil_option_updating ) ) then

      ! Set orography ancil filename from namelist
      write(ancil_fname,'(A)') trim(ancil_directory)//'/'// &
                               trim(orography_ancil_path)
      call tmp_file%init_xios_file("orography_ancil", path=ancil_fname)
      call files_list%insert_item(tmp_file)
    end if

    ! Setup the lbc file
    if ( lbc_option == lbc_option_file ) then
      write(lbc_fname,'(A)') trim(lbc_directory)//'/'// &
                             trim(lbc_filename)
      call tmp_file%init_xios_file("lbc", path=lbc_fname)
      call files_list%insert_item(tmp_file)
    endif

    ! Setup checkpoint writing context information
    if ( checkpoint_write ) then
      ! Create checkpoint filename from stem and end timestep
      write(checkpoint_write_fname,'(A,A,I6.6)') &
                           trim(checkpoint_stem_name),"_", clock%get_last_step()
      call tmp_file%init_xios_file( "lfric_checkpoint_write", &
                                    checkpoint_write_fname,   &
                                    clock%get_last_step(), &
                                    field_group_id="checkpoint_fields" )
      call files_list%insert_item(tmp_file)
    end if

    ! Setup checkpoint reading context information
    if ( checkpoint_read ) then
      ! Create checkpoint filename from stem and (start - 1) timestep
      write(checkpoint_read_fname,'(A,A,I6.6)') &
                   trim(checkpoint_stem_name),"_", (clock%get_first_step() - 1)
      call tmp_file%init_xios_file( "lfric_checkpoint_read",    &
                                    checkpoint_read_fname,      &
                                    clock%get_first_step() - 1, &
                                    field_group_id="checkpoint_fields" )
      call files_list%insert_item(tmp_file)
    end if

  end subroutine init_gungho_files

end module gungho_setup_io_mod
