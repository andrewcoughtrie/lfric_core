!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!>  @brief   Restart/checkpoint functionality for the model.
!>
!>  @details Reads a namelist with the data for controlling whether prognostic
!!           fields are read and/or written, how many timesteps the model is run 
!!           for and what is the diagnostic frequency (to be replaced by the 
!!           configuration object). 
!!           restart_type data attributes are all private so it is used
!!           by calling procedures contained within.
!-------------------------------------------------------------------------------
module restart_control_mod
  use constants_mod, only : str_max_filename, str_long
  use log_mod,       only : log_event, log_scratch_space, &
                            LOG_LEVEL_INFO, LOG_LEVEL_ERROR
  implicit none

  private

  !> @brief Holds information for restart/checkpoint functionality.
  !> @details Objects in this type provide accessor functions (getters) to 
  !!          information needed for restarting the model from a known state,
  !!          creating model checkpoints and diagnostic outputs. The relevant 
  !!          information (e.g. start and end timesteps, names of I/O files 
  !!          etc.) is also contained here.
  type, public :: restart_type

     private
     logical :: checkpoint_read 
     integer :: timestep_start
     integer :: timestep_end 
     logical :: checkpoint_write
     character(len=str_max_filename) :: restart_stem_name 
     integer :: diagnostic_frequency

   contains

     !> @brief Gets filename for field data from previous timestep.
     !> @param[in] field_name Name of the field.
     !> @return Filename for output of the field data at timestep before the  
     !>         starting one (timestep_start - 1)
     procedure :: startfname

     !> @brief Gets filename for field data at final timestep.
     !> @param[in] field_name Name of the field.
     !> @return Filename for output of the field data at final timestep 
     !>         (timestep_end).
     procedure :: endfname

     !> @brief Gets filename for field data at given timestep.
     !> @param[in] field_name Name of the field.
     !> @param[in] ts Timestep.
     !> @return Filename for diagnostic output of the field data at given 
     !>         timestep (ts).
     procedure :: ts_fname

     !> @brief Gets the starting timestep.
     !> @return Timestep from which the run starts, read from a namelist.
     procedure :: ts_start

     !> @brief Gets the final timestep.
     !> @return Timestep at which the run ends, read from a namelist.
     procedure :: ts_end

     !> @brief Gets the diagnostic frequency.
     !> @return The frequency of diagnostic measurement, read from a namelist.
     procedure :: diag_freq

     !> @brief Decides whether to read from a checkpoint.
     !> @return User-defined variable to decide whether the model state is read  
     !>         from a checkpoint file.
     procedure :: read_file

     !> @brief Decides whether to write to a checkpoint.
     !> @return User-defined variable to decide whether the model state will be 
     !>         written to a checkpoint file.
     procedure :: write_file

  end type restart_type

  interface restart_type
     module procedure restart_constructor
  end interface restart_type

contains

  !> @brief Constructor for the restart_type.
  !> @param[in] fname The filename of the namelist file
  !> @detail Opens, reads and closes namelist file (with error reporting),
  !!         sets the values of the data attributes from the namelist file 
  !!         and does some sanity checking to help prevent user error for 
  !!         timestep start and end values.
  function restart_constructor(fname) result(self)
    implicit none
    character(len=str_max_filename), intent(in) :: fname ! the name of the nml file
    type(restart_type)                          :: self
    logical            :: checkpoint_read, checkpoint_write
    integer, parameter :: funit = 555
    integer, parameter :: timestep_limit  = 1000000
    integer            :: timestep_start
    integer            :: timestep_end   
    integer            :: diagnostic_frequency
    integer            :: ierr
    character(len=str_long)         :: ioerrmsg=''
    character(len=str_max_filename) :: restart_stem_name

    namelist /restart_nml/ checkpoint_read, timestep_start, & 
                           timestep_end, checkpoint_write,  &
                           restart_stem_name, diagnostic_frequency

    ! Open restart file
    open(funit,file=trim(fname),iostat=ierr,status='old',iomsg=ioerrmsg)
    if(ierr/=0) then
       write( log_scratch_space,'(A)') "problems opening File..."
       call log_event(log_scratch_space,LOG_LEVEL_INFO)
       call log_event(ioerrmsg,LOG_LEVEL_ERROR)
    end if   
    ! Read restart file
    read(funit,nml=restart_nml,iostat=ierr,iomsg=ioerrmsg)
    if(ierr/=0) then
       write(*,*) checkpoint_read
       write(*,*) timestep_start
       write(*,*) timestep_end
       write(*,*) checkpoint_write
       write(*,*) trim(restart_stem_name)
       write(*,*) diagnostic_frequency
       write( log_scratch_space,'(A)') "problems reading File ..."
       call log_event(log_scratch_space,LOG_LEVEL_INFO)
       call log_event(ioerrmsg,LOG_LEVEL_ERROR)
    end if
    ! Close restart file
    close(funit,iostat=ierr,iomsg=ioerrmsg)
    if(ierr/=0) then
       write( log_scratch_space,'(A,A)') "Closing File:",fname
       call log_event(log_scratch_space,LOG_LEVEL_INFO)
       call log_event(ioerrmsg,LOG_LEVEL_ERROR)
    end if

    ! Update restart_type variables with ones from the namelist
    self%checkpoint_read = checkpoint_read
    self%timestep_start = timestep_start
    self%timestep_end = timestep_end
    self%checkpoint_write = checkpoint_write
    self%restart_stem_name = restart_stem_name
    self%diagnostic_frequency = diagnostic_frequency

    ! Do some sanity checks 
    if(self%timestep_start >= self%timestep_end) then
       write(log_scratch_space,'(A,I6,",",I6)') & 
            "restart_control: timestep_start must be less than timestep_end:", &
            self%timestep_start,self%timestep_end
       call log_event(log_scratch_space,LOG_LEVEL_ERROR)
    end if    
    if(self%timestep_start<=0) then
       write(log_scratch_space,'(A,I6)') & 
            "restart_control: timestep_start cannot be negative or zero:",  &
            self%timestep_start
       call log_event(log_scratch_space,LOG_LEVEL_ERROR)
    end if
    if(self%timestep_end<0) then
       write(log_scratch_space,'(A,I6)')    &
            "restart_control: timestep_end cannot be negative", self%timestep_end
       call log_event(log_scratch_space,LOG_LEVEL_ERROR)
    end if
    if(self%timestep_start>=timestep_limit) then
       write(log_scratch_space,'(A,I8)')  &
            "restart_type: timestep_start too big, reduce to less than ", &
                           timestep_limit
       call log_event(log_scratch_space,LOG_LEVEL_ERROR)
    end if
    if(self%timestep_end>=timestep_limit) then
       write(log_scratch_space,'(A,I8)')  &
            "restart_type: timestep_end too big, reduce to less than ", &
                           timestep_limit
       call log_event(log_scratch_space,LOG_LEVEL_ERROR)
    end if
    if(self%checkpoint_read) then
       write(log_scratch_space,'(A,A)') "Re-starting Dynamo:", &
            trim(self%startfname(""))
       call log_event(log_scratch_space,LOG_LEVEL_INFO)
    else 
       call log_event("Cold start, spinning up",LOG_LEVEL_INFO)
    end if
    
  end function restart_constructor

  ! Gets filename with field data from previous timestep to start the run.
  function startfname(self,field_name)

    class(restart_type), intent(in) :: self
    character(len=*),    intent(in) :: field_name
    character(len=str_max_filename) :: startfname
    integer :: ts_minus_1
    ts_minus_1 = self%timestep_start - 1
    write(startfname,'(A,A,A,A,I6.6)') trim(self%restart_stem_name),"_", &
         trim(field_name),".T",ts_minus_1

  end function startfname

  ! Gets filename for outputs of field data at final timestep.
  function endfname(self,field_name)

    class(restart_type), intent(in) :: self
    character(len=*),    intent(in) :: field_name
    character(len=str_max_filename) :: endfname
    write(endfname,'(A,A,A,A,I6.6)') trim(self%restart_stem_name),"_", &
         trim(field_name),".T",self%timestep_end

  end function endfname

  ! Gets filename for outputs of field data at a given timestep.
  function ts_fname(self,field_name,ts)

    class(restart_type), intent(in) :: self
    character(len=*),    intent(in) :: field_name
    integer,             intent(in) :: ts
    character(len=str_max_filename) :: ts_fname
    write(ts_fname,'(A,A,A,A,I6.6)') trim(self%restart_stem_name),"_", &
         trim(field_name),".T",ts

  end function ts_fname

  ! Gets the starting timestep of the run.
  function ts_start(self)

    class(restart_type), intent(in) :: self
    integer                         :: ts_start
    ts_start = self%timestep_start 

  end function ts_start

  ! Gets the final timestep of the run.
  function ts_end(self)

    class(restart_type), intent(in) :: self
    integer                         :: ts_end
    ts_end = self%timestep_end

  end function ts_end

  ! Gets the diagnostic frequency for outputs of field data.
  function diag_freq(self)

    class(restart_type), intent(in) :: self
    integer                         :: diag_freq
    diag_freq = self%diagnostic_frequency

  end function diag_freq

  ! Decides whether to read model state from a checkpoint.
  function read_file(self) 

    class(restart_type), intent(in) :: self
    logical read_file
    read_file = self%checkpoint_read

  end function read_file

  ! Decides whether to write model state to a checkpoint.
  function write_file(self) 

    class(restart_type), intent(in) :: self
    logical write_file
    write_file = self%checkpoint_write

  end function write_file
  
end module restart_control_mod
