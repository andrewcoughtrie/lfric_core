!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!>
!> @brief Holds and manages the multiple meshes used in a model run.
!>
!> @details A container which holds a collection of meshes
!>          It will handle the creation and storing of requested meshes.
!
module mesh_collection_mod

  use constants_mod,      only: r_def, i_def, l_def, str_def, imdi
  use mesh_mod,           only: mesh_type
  use log_mod,            only: log_event, log_scratch_space,     &
                                LOG_LEVEL_ERROR, LOG_LEVEL_TRACE
  use linked_list_mod,    only: linked_list_type, &
                                linked_list_item_type

  use development_config_mod, only: implement_consolidated_multigrid

  implicit none

  private

  type, public :: mesh_collection_type

    private

    type(linked_list_type) :: mesh_list

    character(str_def), allocatable :: name_tags(:)
    integer(i_def),     allocatable :: name_ids(:)

  contains
    private
    procedure, public :: add_new_mesh
    procedure, public :: add_unit_test_mesh

    procedure, public :: n_meshes
    procedure, public :: get_mesh_names
    procedure, public :: get_mesh_id

    procedure, public :: get_mesh_by_name
    procedure, public :: get_mesh_by_id
    generic,   public :: get_mesh => get_mesh_by_id, &
                                     get_mesh_by_name

    procedure, public :: check_for
    procedure, public :: clear

    final             :: mesh_collection_destructor

  end type mesh_collection_type

  interface mesh_collection_type
    module procedure mesh_collection_constructor
  end interface

  ! Module variable allows access to the single mesh collection
  type(mesh_collection_type), public, allocatable :: mesh_collection

contains

!===========================================================================
!> Constructs the mesh collection object
!> @return self The constructed mesh collection object
function mesh_collection_constructor() result(self)

  implicit none

  type(mesh_collection_type) :: self

  self%mesh_list = linked_list_type()

end function mesh_collection_constructor


!===========================================================================
!> @brief Creates a mesh object and adds it to the mesh collection
!> @param [in] global_mesh   Global mesh object on which the partition is
!>                           applied
!> @param [in] partition     Partition object to base 3D-Mesh on
!> @param [in] extrusion     Mechanism by which extrusion will be performed.
!> @param [in] mesh_name     Name to assign to partitioned mesh. Global
!>                           mesh name will be used if this argument is absent.
!> @return                   A unique identifier for the created mesh
function add_new_mesh( self,        &
                       global_mesh, &
                       partition,   &
                       extrusion,   &
                       mesh_name ) result( mesh_id )

  use extrusion_mod,   only : extrusion_type
  use global_mesh_mod, only : global_mesh_type
  use partition_mod,   only : partition_type

  implicit none

  class(mesh_collection_type),  intent(inout)       :: self
  type(global_mesh_type),       intent(in), pointer :: global_mesh
  type(partition_type),         intent(in)          :: partition
  class(extrusion_type),        intent(in)          :: extrusion
  character(str_def), optional, intent(in)          :: mesh_name

  ! Local variables
  type(mesh_type) :: mesh

  integer(i_def) :: mesh_id
  integer(i_def) :: i

  character(str_def) :: name

  mesh_id  = imdi

  if (present(mesh_name)) then
    name = trim(mesh_name)
  else
    name = global_mesh%get_mesh_name()
  end if

  if (implement_consolidated_multigrid) then

    ! Check list of tag names to see if mesh is already in collection
    if (self%check_for(name)) then
      do i=1, size(self%name_tags)
        if ( trim(self%name_tags(i)) == trim(name) ) then
          write(log_scratch_space,'(A)')                               &
              'Mesh '//trim(name)//' already present in collection. '//&
              'Mesh names within a collection should be unique.'
          call log_event(log_scratch_space, LOG_LEVEL_ERROR)
        end if
      end do
    end if
  end if

  mesh = mesh_type( global_mesh, partition, extrusion, mesh_name=name )

  mesh_id = mesh%get_id()

  call self%mesh_list%insert_item( mesh )

  if ( implement_consolidated_multigrid ) call update_tags(self, name, mesh_id)

end function add_new_mesh


!===========================================================================
!> @brief Creates a unit test version of the mesh object and adds it to the
!>        mesh collection
!> @param [in] mesh_cfg Sets the type of test mesh.
!> @return              A unique identifier for the created mesh
function add_unit_test_mesh( self, mesh_cfg ) result( mesh_id )
  implicit none

  class(mesh_collection_type), intent(inout) :: self
  integer(i_def), intent(in) :: mesh_cfg

  type(mesh_type)    :: mesh
  integer(i_def)     :: mesh_id
  character(str_def) :: name

  mesh    = mesh_type( mesh_cfg )
  mesh_id = mesh%get_id()
  name    = mesh%get_mesh_name()

  call self%mesh_list%insert_item( mesh )

  if ( implement_consolidated_multigrid ) &
      call update_tags( self, name, mesh_id )

  return
end function add_unit_test_mesh


!===========================================================================
!> @brief Requests a mesh object with specified mesh name from
!>        the collection.
!>
!> @param[in] mesh_name Name tag of mesh object to retrieve.
!>
!> @return Mesh object with requested mesh name if present in
!>         collection, a null pointer is returned if there is no
!>         mesh with the requested name.
!>
function get_mesh_by_name( self, mesh_name ) result( mesh )

  implicit none

  class(mesh_collection_type), intent(in) :: self
  character(str_def),          intent(in) :: mesh_name

  type(mesh_type), pointer :: mesh

  integer(i_def) :: n_meshes, i
  integer(i_def) :: mesh_id

  n_meshes = self%mesh_list%get_length()

  mesh => null()
  do i=1, n_meshes
    if (trim(mesh_name) == trim(self%name_tags(i))) then
      mesh_id = self%name_ids(i)
      mesh => mesh_collection%get_mesh( self%name_ids(i) )
      if ( .not. associated(mesh) ) then
        write(log_scratch_space,'(A)') &
            trim(mesh_name)//' not found in mesh collection.'
        call log_event(log_scratch_space, LOG_LEVEL_ERROR)
      end if
      exit
    end if
  end do

  return

end function get_mesh_by_name


!===========================================================================
!> @brief Requests a mesh object with specified mesh id from
!>        the collection.
!>
!> @param[in] mesh_id ID of mesh object to retrieve.
!>
!> @return Mesh object with requested mesh id if present in
!>         collection, a null pointer is returned if there is no
!>         mesh with the requested id.
!>
function get_mesh_by_id( self, mesh_id ) result( mesh )

  implicit none

  class(mesh_collection_type), intent(in) :: self
  integer(i_def),              intent(in) :: mesh_id

  type(mesh_type), pointer :: mesh

  ! Pointer to linked list - used for looping through the list
  type(linked_list_item_type),pointer :: loop => null()

  ! start at the head of the mesh collection linked list
  loop => self%mesh_list%get_head()

  do
    ! If list is empty or we're at the end of list and we didn't find the
    ! mesh_id, return a null pointer
    if ( .not. associated(loop) ) then
      nullify(mesh)
      exit
    end if

    ! Otherwise search list for the id we want
    if ( mesh_id == loop%payload%get_id() ) then
      ! 'cast' to the mesh_type
      select type(m => loop%payload)
        type is (mesh_type)
          mesh => m
      end select
      exit
    end if
    loop => loop%next
  end do

  nullify(loop)

end function get_mesh_by_id


!===========================================================================
!> @brief Queries the collection as to the presence of a
!>        mesh object with the specified mesh name.
!>
!> @param[in] mesh_name  Mesh name of object to check for.
!>
!> @return    logical    .true. if global mesh object present
!>                       in collection.
!>
function check_for(self, mesh_name) result(answer)

  implicit none

  class(mesh_collection_type), intent(in) :: self
  character(str_def),          intent(in) :: mesh_name

  logical :: answer

  answer = .false.
  if (allocated(self%name_tags)) then
    answer = any(self%name_tags == mesh_name)
  end if

  return
end function check_for


!===========================================================================
!> @brief Returns the number of meshes in the collection.
!>
!> @detail This function returns the number of unique mesh
!>         tag names in this collection.
!>
!> @return Number of mesh tag names availble to query.
!>
function n_meshes(self) result(number_of_meshes)

  implicit none

  class(mesh_collection_type), intent(in) :: self

  integer(i_def) :: number_of_meshes
  if (allocated(self%name_tags)) then
    number_of_meshes = size(self%name_tags)
  else
    number_of_meshes = 0
  end if

  return
end function n_meshes


!===========================================================================
!> @brief Returns mesh tag names of mesh objects in the collection.
!>
!> @return mesh_names  String array <<allocatable>> of mesh names in
!>                     collection.
!>
function get_mesh_names(self) result(mesh_names)

  implicit none

  class(mesh_collection_type), intent(in) :: self

  character(str_def), allocatable :: mesh_names(:)

  integer(i_def) :: n_meshes

  n_meshes = self%mesh_list%get_length()

  if (n_meshes > 0) then
    if (allocated(mesh_names)) deallocate(mesh_names)
    allocate(mesh_names, source=self%name_tags)
  end if

  return
end function get_mesh_names


!===========================================================================
!> @brief Returns mesh ID of specified mesh in the collection.
!>
!> @param[in] mesh_name  Mesh name of object to return ID.
!>
!> @return    mesh_id    Integer ID of global mesh object if present
!>                       in collection.
!>
function get_mesh_id( self, mesh_name ) result( mesh_id )

  implicit none

  class(mesh_collection_type) :: self
  character(str_def), intent(in) :: mesh_name
  integer(i_def) :: n_meshes, i
  integer(i_def) :: mesh_id

  mesh_id = imdi
  n_meshes = size(self%name_tags)

  do i=1 , n_meshes
    if (trim(mesh_name) == trim(self%name_tags(i))) then
      mesh_id = self%name_ids(i)
      exit
    end if
  end do

  return
end function get_mesh_id


!===========================================================================
!> Clear all items from the mesh collection linked list
subroutine clear(self)

  implicit none

  class(mesh_collection_type), intent(inout) :: self

  call self%mesh_list%clear()

  if (allocated(self%name_tags)) deallocate(self%name_tags)
  if (allocated(self%name_ids))  deallocate(self%name_ids)

end subroutine clear


!===========================================================================
! Mesh collection destructor
subroutine mesh_collection_destructor(self)

  implicit none

  type (mesh_collection_type), intent(inout) :: self

  call self%clear()

end subroutine mesh_collection_destructor


!===========================================================================
!> @brief PRIVATE SUBROUTINE: Updates the name_tags and name_ids
!>                            arrays when a mesh is added to the
!>                            collection.
!>
!> @param[inout] self       Mesh collection object to update.
!> @param[in]    mesh_name  The mesh tag name to be added. It must
!>                          not already exist in name_tags array.
!> @param[in]    mesh_id    ID to map to the provided mesh name.
!>
subroutine update_tags( self, mesh_name, mesh_id )

  implicit none

  class(mesh_collection_type), intent(inout) :: self
  character(str_def),          intent(in)    :: mesh_name
  integer(i_def),              intent(in)    :: mesh_id

  character(str_def), allocatable :: new_tag_list(:)
  integer(i_def),     allocatable :: new_id_list(:)
  integer(i_def)                  :: i, n_tags

  ! 1. Check that the id provided exists
  if (.not. self%mesh_list%item_exists(mesh_id)) then
    write(log_scratch_space,'(A,I0,A)')      &
        'Unable to update tags, mesh id: ',  &
         mesh_id,' not found in collection'
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end if

  ! 2. Check to see if the mesh name/mesh_id entry exists already
  if ( allocated(self%name_tags) ) then
    do i=1, size(self%name_tags)

      if ( (self%name_tags(i) == mesh_name) .and. &
           (self%name_ids(i)  == mesh_id) ) then
        ! Do nothing
        return

      else if ( (self%name_tags(i) == mesh_name) .and. &
                (self%name_ids(i)  /= mesh_id) ) then
        ! Tag name name used for different mesh, flag error
        write(log_scratch_space,'(A,I0,A)')      &
            'Mesh tag name "'//trim(mesh_name)// &
            '" already assigned in collection.'
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )

      end if
    end do

    ! Tag name not used and id exists, so append new entry
    n_tags = size(self%name_tags)
    allocate( new_tag_list(n_tags+1) )
    allocate( new_id_list(n_tags+1)  )

    new_tag_list(:n_tags)  = self%name_tags(:)
    new_id_list(:n_tags )  = self%name_ids(:)
    new_tag_list(n_tags+1) = mesh_name
    new_id_list(n_tags+1)  = mesh_id

    call move_alloc( new_tag_list, self%name_tags )
    call move_alloc( new_id_list, self%name_ids )

    if (allocated(new_tag_list)) deallocate( new_tag_list )
    if (allocated(new_id_list))  deallocate( new_id_list )

  else

    ! No tags assigned yet
    allocate(self%name_tags(1))
    allocate(self%name_ids(1))
    self%name_tags(1) = mesh_name
    self%name_ids(1)  = mesh_id

  end if

  return
end subroutine update_tags

end module mesh_collection_mod
