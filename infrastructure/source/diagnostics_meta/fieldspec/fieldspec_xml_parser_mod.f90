!-----------------------------------------------------------------------------
! (C) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Procedures to parse an iodef.xml file and add a fieldspec object to
!>        the collection
!>
!> @details Uses the FoX library to parse an iodef.xml file and pass the values
!>          to a fieldspec_factory object. This is then used to create a
!>          fieldspec object , which is added to the fieldspec_collection
!>
!>          The parser expects to find an iodef.xml file with a field_definition
!>          section with the following structure (shown here with details
!>          ignored by the parser removed for clarity):
!>
!>          <field_definition> \n
!>            <field_group id="example_field_group_1" enabled=".TRUE."> \n
!>              <field id="example_field_1"> \n
!>                <variable name="variable_1_name">variable_1_value</variable> \n
!>              </field> \n
!>            </field_group> \n
!>          </field_definition>

module fieldspec_xml_parser_mod

  use constants_mod,             only: i_def, str_def, l_def
  use field_type_enum_mod,       only: field_type_from_name
  use fieldspec_collection_mod,  only: fieldspec_collection_type
  use fieldspec_factory_mod,     only: fieldspec_factory_type
  use fs_continuity_mod,         only: functionspace_from_name
  use fox_sax,                   only: XML_T, dictionary_t, getValue, hasKey, &
                                       parse, open_xml_file, close_xml_t
  use io_driver_enum_mod,        only: io_driver_from_name
  use log_mod,                   only: log_event, log_scratch_space, &
                                       LOG_LEVEL_ERROR

  implicit none

  private
  public :: populate_fieldspec_collection

  type(fieldspec_collection_type), pointer :: fieldspec_collection

  !> @brief Factory used to build up fieldspec objects while reading XML
  !>        before adding them to the collection
  type(fieldspec_factory_type) :: fieldspec_factory
  logical :: fieldspec_factory_initialised = .false.


  ! Module-level variables used by XML handler methods called from populate()

  ! Tells the parser when to skip lines in the XML file
  logical :: ignore_element = .false.

  ! Flags used to check which element the XML parser is currently within
  logical :: in_field_def = .false.
  logical :: in_field_group = .false.
  logical :: in_field = .false.
  logical :: in_field_variable = .false.

  ! Stores the name of the current XML "field" element's
  ! "variable" while it is processed by the XML event handlers
  character(str_def) :: xml_field_variable = ""
  character(str_def) :: field_group_id = ""

contains

  !===========================================================================
  !> @brief Populates the fieldspec collection from an XIOS iodef.xml file
  !> @param[in] iodef_filepath The path to the iodef.xml file
  !>
  subroutine populate_fieldspec_collection( iodef_filepath )

    implicit none

    character(len = *),               intent(in)       :: iodef_filepath
    type(XML_T)                                        :: parser
    integer                                            :: iostatus

    fieldspec_collection => fieldspec_collection_type()

    call open_xml_file(parser, iodef_filepath, iostatus)
    if (iostatus /= 0) then
      write(log_scratch_space, '(A)') 'Error opening file'
      call log_event( log_scratch_space, LOG_LEVEL_ERROR)
      stop
    end if

    ! Call FoX's parse subroutine to begin reading through the XML file
    ! Tells the FoX API the mapping of interfaces to the subroutines containing
    ! instructions for event handling
    ! (e.g event when reaching an opening/closing tag of an XML element)
    call parse( parser, &
                startElement_handler = startElement_handler, &
                characters_handler = text_handler, &
                endElement_handler = endElement_handler )

    ! end parsing of XML file
    call close_xml_t(parser)

    return
  end subroutine populate_fieldspec_collection

  !===========================================================================
  !> @brief "Switches" on/off the logicals used to keep track of which
  !> XML elements the parser is currently within
  !>
  !> Intended for use by XML start/endElement_handler rather than being called
  !> manually
  !>
  !> @param[in] element_name The name of the flag whose value is to be set
  !> @param[in] new_value The value to set the flag to (.true. or .false.)
  subroutine switch_xml_element_flag( element_name, new_value )

    implicit none

    character(len=*), intent(in)    :: element_name
    logical, intent(in)             :: new_value

    select case (element_name)

      case ("field_definition")
        in_field_def = new_value

      case ("field_group")
        in_field_group = new_value

      case ("field")
        in_field = new_value

      case("variable")
        if (in_field) then
          in_field_variable = new_value
        end if

    end select

    return
  end subroutine switch_xml_element_flag

  !===========================================================================
  !> @brief Handles the event where the FoX SAX parser reaches an XML element's
  !>        opening tag
  !>
  !> This subroutine is only called in the populate subroutine using the FoX API
  !> and is not to be called manually
  !>
  !> @param[in] namespaceURI
  !> @param[in] localname
  !> @param[in] name
  !> @param[in] attributes
  subroutine startElement_handler( namespaceURI, localname, name, attributes )

    implicit none

    character(len = *), intent(in)    :: namespaceURI
    character(len = *), intent(in)    :: localname
    character(len = *), intent(in)    :: name
    type(dictionary_t), intent(in)    :: attributes
    integer                           :: i

    ! Ignore element if parser is still in a field group that is not enabled
    if (ignore_element) then
      return
    end if

    ! Set the flag corresponding to this element to true to signify parser is
    ! inside it
    call switch_xml_element_flag(name, .true.)


    if (in_field_def) then

      ! Set parser to ignore current group of fields if they are not enabled
      if (name == "field_group") then
        field_group_id = getValue(attributes, "id")
        if (getValue(attributes, "enabled") == ".FALSE.") then
          ignore_element = .true.
          return
        end if
      end if

      ! Store field's ID to be put in fieldspec object in endElement_handler
      if (in_field_group) then
        if (name == "field" .and.  hasKey(attributes, "id")) then

          ! Clear the fieldspec factory of any previous data
          call fieldspec_factory%initialise()
          fieldspec_factory_initialised = .true.
          call fieldspec_factory%set_unique_id( getValue(attributes, "id") )
          call fieldspec_factory%set_field_group_id( field_group_id )

        end if

        ! Store name of variable while it is processed by text_handler
        if (name == "variable" .and. in_field) then
          xml_field_variable = getValue(attributes, "name")
        end if

      end if

    end if

    return
  end subroutine startElement_handler

  !===========================================================================
  !> @brief Handles the event where the FoX SAX parser
  !>        starts reading an XML element's text
  !>
  !> The text corresponds to the value of variable whose name is stored in
  !> xml_field_variable. It is stored in the fieldspec_factory to be to be put
  !> in a fieldspec object in endElement_handler
  !>
  !> This subroutine is only called in the populate subroutine using the FoX API
  !> and is not to be called manually
  !>
  !> @param[in] text The text to be handled
  subroutine text_handler( text )
    use fox_common, only : rts
    implicit none

    character(len = *), intent(in) :: text
    integer(i_def)                 :: int_from_char
    character(str_def)             :: trimmed_char
    logical(l_def)                 :: checksum

    if (in_field .and. in_field_variable) then

      select case(xml_field_variable)
        case ("mesh_id")
          ! rts function to convert XML text to integer
          call rts(text, int_from_char)
          call fieldspec_factory%set_mesh_id(int_from_char)

        case ("function_space")
          ! rts function used to trim whitespace on BOTH sides
          call rts(text, trimmed_char)
          call fieldspec_factory%set_function_space( functionspace_from_name(trimmed_char) )

        case ("order", "element_order")
          call rts(text, int_from_char)
          call fieldspec_factory%set_order(int_from_char)

        case ("field_kind")
          call rts(text, int_from_char)
          call fieldspec_factory%set_kind(int_from_char)

        case ("field_type")
          call rts(text, trimmed_char)
          call fieldspec_factory%set_type( field_type_from_name(trimmed_char) )

        case ("io_driver")
          call rts(text, trimmed_char)
          call fieldspec_factory%set_io_driver( io_driver_from_name(trimmed_char))

        case ("checksum")
          call rts(text, checksum)
          call fieldspec_factory%set_checksum(checksum)
      end select

    end if

    return
  end subroutine text_handler

  !===========================================================================
  !> @brief Handles the event where the FoX SAX parser reaches an XML element's
  !>        closing tag
  !>
  !> This subroutine is only called in the populate subroutine using the FoX API
  !> and is not to be called manually
  !>
  !> @param[in] namespaceURI
  !> @param[in] localname
  !> @param[in] name
  subroutine endElement_handler( namespaceURI, localname, name )
    implicit none

    character(len = *),              intent(in)  :: namespaceURI
    character(len = *),              intent(in)  :: localname
    character(len = *),              intent(in)  :: name

    ! Stop parser ignoring lines now that it has finished reading the disabled
    ! field_group
    if (ignore_element .and. name == "field_group") then
      ignore_element = .false.
    end if

    ! Create fieldspec object when the parser is done collating its
    ! properties from the XML field
    if (in_field_def .and. in_field_group .and. name == "field" &
            .and. fieldspec_factory_initialised) then

      call fieldspec_collection%add_fieldspec( fieldspec_factory%finalise() )
      fieldspec_factory_initialised = .false.

    end if

    ! Switch off flag for current element now the parser has finished reading it
    call switch_xml_element_flag(name, .false.)

    return
  end subroutine endElement_handler

end module fieldspec_xml_parser_mod