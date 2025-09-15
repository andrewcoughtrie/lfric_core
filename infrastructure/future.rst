..
    (c) Crown copyright 2025 Met Office. All rights reserved.
    The file LICENCE, distributed with this code, contains details of the terms
    under which the code may be used.

Ideas For the Future of Configuration
=====================================

This document holds thoughts, sketches and notions for potential future work on
the configuration system.

.. attention::

  The existence of an idea in this document is no guarantee it will ever be
  implemented. Ideas are half-baked and highly speculative. They are captured
  in order that they don't get lost.

Runtime Validation
~~~~~~~~~~~~~~~~~~

We generate the name-list loading code from the configuration meta-data which is
also used to drive the Rose configuration preparation system. This way there is
no double-entry, once a configuration field has been added to the meta-data it
is both editable using Rose and loadable into the model.

There is additional information held in this meta-data which Rose uses to
validate user input. Potentially we could take advantage of this to also
validate the configuration as it is loaded. This is particularly useful as the
configuration we are loading need not have been prepared by the Rose tool. At
the moment we trust to luck that it is correct.

This would require understanding the validation rules in the meta-data and
converting them into code for the name-list loading routines.

Conditional Configuration
~~~~~~~~~~~~~~~~~~~~~~~~~

As noted above the configuration meta-data contains information we are not
currently taking advantage of. Another case of this is the idea of "trigger
ignore" which allows whole name-lists to be contingent on values (usually
boolean switches) in other name-lists. e.g. The orography name-list is only
useful if we have enabled orography.

There is the stub of a system to use this information in
``ensure_configuration`` procedure, generated as part of the ``loader.f90``
file. This is intended to indicate which of a list of name-lists has, and has
not, been loaded. Currently this is always used with a hard-coded list of
name-lists. By definition this list cannot contain optional name-lists.

The plan would be to move to a dynamic list of required name-lists driven by
the configuration meta-data. This would have to be part of generated code.

Inclusion Semantics
~~~~~~~~~~~~~~~~~~~

At the moment all the name-lists in a name-list file are user defined but we
could potentially have name-lists which control the configuration system. In
particular we could have an "inclusion" name-list which lists other name-list
files to read as part of reading this one.

This would potentially allow better management of name-lists by allowing them
to be split into fragments, then composed in different ways for different
purposes.

This is probably not useful for people using Rose since this approach does not
fit into its model of operation. It could be very useful for people who are
managing name-lists by other means.
