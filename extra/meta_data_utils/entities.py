##############################################################################
# (c) Crown copyright 2020 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
"""This modules contains three classes that represent the three structures that
field meta data is kept in. Field encapsulates a field and all the meta data for
that field. Fields objects are held within Group objects. Group objects are then
 in turn held within Section objects."""

import logging
import re
from typing import Dict

LOGGER = logging.getLogger(__name__)


class Field:
    """Encapsulates all information for a field. Provides methods to create
    derived fields and check for valid meta data"""

    UNIQUE_ID_REGEX = re.compile(
        r"(?P<section_name>[a-z_]+)__(?P<item_name>[a-z_]+)")

    def __init__(self, file_name):
        self.file_name = file_name
        self._unique_id = None
        self.units = None
        self.function_space = None
        self.order = None
        self.io_driver = None
        self.trigger = None
        self.description = None
        self.data_type = None
        self.time_step = None
        self.recommended_interpolation = None
        self.packing = None
        self.vertical_dimension = None
        self.standard_name = None
        self.level_definition = None
        self.misc_meta_data = None
        self.item_name = None
        self.item_title = None
        self.long_name = None

    @property
    def unique_id(self) -> str:
        """getter method for unique_id"""
        return self._unique_id

    @unique_id.setter
    def unique_id(self, var: str):
        """setter method for unique_id"""
        if self._unique_id is None:

            found = Field.UNIQUE_ID_REGEX.search(var)
            if found:
                self.item_name = found.group("item_name")
                self.item_title = self.item_name.replace("_", " ").title()
            else:
                LOGGER.error("Unique ID %s does not conform to the standard",
                             var)

            self._unique_id = var
        else:
            LOGGER.error(
                "Unique ID %s has already been set. Tried to set to %s",
                self.unique_id, var)

    def is_valid(self) -> bool:
        """Checks for existence of mandatory values within the field.
        Logs any non-existent fields as errors
        :return: is_valid: A boolean value, True if meta data is valid,
        False otherwise"""
        is_valid = True

        if not self.unique_id:
            LOGGER.error("A unique id is missing from a field in %s",
                         self.file_name)
            is_valid = False
        if not self.units:
            LOGGER.error("A unit of measure is missing from a field in %s",
                         self.file_name)
            is_valid = False
        if not self.function_space:
            LOGGER.error("A function space is missing from a field in %s",
                         self.file_name)
            is_valid = False
        if not self.trigger:
            LOGGER.error("Triggering syntax is missing from a field in %s",
                         self.file_name)
            is_valid = False
        if not self.description:
            LOGGER.error("A description is missing from a field in %s",
                         self.file_name)
            is_valid = False
        if not self.data_type:
            LOGGER.error("A data type is missing from a field in %s",
                         self.file_name)
            is_valid = False
        if not self.time_step:
            LOGGER.error("A time step is missing from a field in %s",
                         self.file_name)
            is_valid = False
        if not self.recommended_interpolation:
            LOGGER.error(
                "A recommended_interpolation attribute is missing from a"
                " field in %s", self.file_name)
            is_valid = False
        return is_valid


class Group:
    """Represents a group within a science section. Encapsulates all aspects of
    a groups and is composed of the fields within the group"""

    def __init__(self, name: str, file_name: str):
        self.file_name = file_name
        self.title = name.replace("_", " ").title()
        self.name = name
        self.fields: Dict[str, Field] = {}

    def add_field(self, field: Field):
        """Adds a field to the group
        :param field: The field to be added"""
        if field.unique_id not in self.fields:
            self.fields.update({field.unique_id: field})
        else:
            LOGGER.error("Field with unique ID: %s is already in Group: %s",
                         field.unique_id, self.title)


class Section:
    """Represents a science section. Encapsulates all aspects of a section and
    is composed of the groups within the section"""
    def __init__(self, name: str):
        self.title = name.replace("_", " ").title()
        self.name = name
        self.groups: Dict[str, Group] = {}

    def add_group(self, group: Group):
        """Adds a group to the section
        :param group: The group to be added"""
        if group.name not in self.groups:
            self.groups.update({group.name: group})
        else:
            LOGGER.error("Group: %s is already in Section: %s",
                         group.name, self.title)
