Quick Start Guide for LFRic
===========================

So you want to play with LFRic models? These instructions should get you up and
running quickly and easily.

.. contents:: Table of Contents

.. note::
   The canonical version of this document is held as reStructured text in
   the repository at
   `source:LFRic/trunk/gungho/documentation/quickstart.rst`:trac:.
   Any changes in a branch which render this document inaccurate should also
   include updates to the version of this document on that branch. The version
   displayed on the wiki is generated from the head of trunk.

Build Environment
-----------------

Within the Met Office (and on certain external machines) we make use of the
`Environment Modules system <http://modules.sourceforge.net/>`_ to manage our
development environment. If you do not have access to this skip to the
`Outwith the Met Office`_ section for more details.

Met Office LFRic Environment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In a terminal window you need to source the module setup script and load the
LFRic environment to set up libraries and compiler:

 1. Source the setup script
 
      +--------------------+---------------------------------------------------+
      | Met Office Desktop | ``source /data/users/lfric/modules/setup``        |
      +--------------------+---------------------------------------------------+
      | Met Office XC-40   | ``source /data/d03/lfric/modules/setup``          |
      +--------------------+---------------------------------------------------+
      | MONSooN            | ``source /common/lfric/modules/setup``            |
      +--------------------+---------------------------------------------------+
      | ARCHER             | ``source /fs2/n02/n02/mhambley/modules/setup``    |
      +--------------------+---------------------------------------------------+

    .. note::
      The script must be executed in-line using the ``source`` command, not in
      a sub-shell as would normally happen by simply running it.

 #. Load the modules

      +--------------------+------------------------------------------------+
      | Met Office Desktop | ``module load environment/dynamo/intel``       |
      |                    +------------------------------------------------+
      |                    | ``module load environment/dynamo/gnu``         |
      +--------------------+------------------------------------------------+
      | Met Office XC-40   | ``module load meto-environment/dynamo/cce``    |
      |                    +------------------------------------------------+
      |                    | ``module load meto-environment/dynamo/intel``  |
      +--------------------+------------------------------------------------+
      | MONSooN            | ``module load meto-environment/lfric/cray``    |
      |                    +------------------------------------------------+
      |                    | ``module load meto-environment/lfric/intel``   |
      +--------------------+------------------------------------------------+
      | ARCHER             | ``module load lfric-environment/dynamo/cce``   |
      |                    +------------------------------------------------+
      |                    | ``module load lfric-environment/dynamo/intel`` |
      +--------------------+------------------------------------------------+

Modules for rendering your own video using the "gource" documentation target
is only available on the Met Office desktop. You will need to do the
following::

  module load common-environment/gource

This will conflict with the compile environment.

Outwith the Met Office
~~~~~~~~~~~~~~~~~~~~~~

The build system makes use of environment variables to understand the system on
which it finds itself.

+---------------+----------------------+--------------------------------------------------------------------------------+
| Variable Name | Tool                 | Notes                                                                          |
+===============+======================+================================================================================+
| FC            | Fortran Compiler     | Supported compilers: Cray, GNU, Intel, Portland                                |
+---------------+----------------------+--------------------------------------------------------------------------------+
| FPP           | Fortran preprocessor | This will be ``cpp -traditional-cpp`` for GNU Fortran and ``fpp`` for Intel.   |
+---------------+----------------------+--------------------------------------------------------------------------------+
| LDMPI         | MPI linker           | The tool to use for linking MPI executables. This will normally be ``mpif90``. |
+---------------+----------------------+--------------------------------------------------------------------------------+

The build system expects paths to libraries to be presented in compiler
environment variables.

+---------------+-----------------------------------+------------------------------------+--------------------------------------------------------+
| Variable Name | Prepend with                      | Purpose                            | Example                                                |
+===============+===================================+====================================+========================================================+
| ``FFLAGS``    | ``-I<path to module directory>``  | Point to ``.mod`` files.           | ``-I/opt/esmf/mod/mod0/Linux.ifort.64.mpich2.default`` |
+---------------+-----------------------------------+------------------------------------+--------------------------------------------------------+
| ``LDFLAGS``   | ``-L<path to library directory>`` | Point to ``.so`` or ``.a`` files.  | ``-L/opt/esmf/lib/lib0/Linux.ifort.64.mpich2.default`` |
+---------------+-----------------------------------+------------------------------------+--------------------------------------------------------+

When LFRic models are dynamically linked the directory in which the ``.so``
files are found must be included in the ``LD_LIBRARY_PATH`` variable.

The ``extra`` directory contains a script which will construct a self-contained
development environment on a Debian Linux system. When run it will build the
environment and when in-lined using ``source`` it will set up the required
environment variables. It is not guaranteed to work although it did at one
point. If nothing else it should codify the requirements outlined below.

Currently the following compilers are used at the Met Office:

+------------------+-----------------+------------------------------+
| Compiler         | Version         | Notes                        |
+==================+=================+==============================+
| Intel Fortran    | 16.0.1 (15.0.1) |                              |
+------------------+-----------------+------------------------------+
| GNU Fortran      | 6.1.0           |                              |
+------------------+-----------------+------------------------------+
| Cray Fortran     | 8.4.3           | Compile only. No unit tests. |
+------------------+-----------------+------------------------------+
| Portland Fortran | 15.7            | Compile only. No unit tests. |
+------------------+-----------------+------------------------------+

The minimum requirements for building LFRic models are:

+----------+----------+-----------------+---------------------------------------+------------------------------------------+
| Package  | Version  | Dependency Type | Purpose                               | Pointed to by...                         |
+==========+==========+=================+=======================================+==========================================+
| ESMF     | 7.0.0    | Build, Run      | Framework & Infrastructure library    | FFLAGS, LDFLAGS, LD_LIBRARY_PATH         |
+----------+----------+-----------------+---------------------------------------+------------------------------------------+
| GMake    | 3.81     | Build           | GNU's version of "make"               | PATH                                     |
+----------+----------+-----------------+---------------------------------------+------------------------------------------+
| HDF5     | 1.8.16   | Build, Run      | Parallel filesystem used by NetCDF    | FFLAGS, LDFLAGS, LD_LIBRARY_PATH         |
+----------+----------+-----------------+---------------------------------------+------------------------------------------+
| Jinja2   | 2.6      | Build           | Required by configuration handler     | PYTHONPATH                               |
+----------+----------+-----------------+---------------------------------------+------------------------------------------+
| MPICH    | 3.2      | Build, Run      | MPI library used by NetCDF            | FFLAGS, LDFLAGS, PATH, LD_LIBRARY_PATH   |
+----------+----------+-----------------+---------------------------------------+------------------------------------------+
| NetCDF   | 4.3.1.1  | Build, Run      | File I/O                              | FFLAGS, LDFLAGS, LD_LIBRARY_PATH         |
+----------+----------+-----------------+---------------------------------------+------------------------------------------+
| pFUnit   | 3.2.8    | Build           | Unit testing framework                | PATH, FFLAGS, LDFLAGS, PFUNIT            |
+----------+----------+-----------------+---------------------------------------+------------------------------------------+
| PSyclone | 1.3.2    | Build           | PSy layer generator                   | PATH, PYTHONPATH                         |
+----------+----------+-----------------+---------------------------------------+------------------------------------------+
| Python   | 2.7      | Build           | Required by the dependency analyser   | PATH                                     |
+----------+----------+-----------------+---------------------------------------+------------------------------------------+
| Zlib     | 1.2.8    | Build, Run      | Compression library used by NetCDF    | CPPFLAGS, LDFLAGS, LD_LIBRARY_PATH       |
+----------+----------+-----------------+---------------------------------------+------------------------------------------+

To build the documentation you will need:

+----------+----------+-------------------------------------------------------------+
| Package  | Version  | Purpose                                                     |
+==========+==========+=============================================================+
| Doxygen  | 1.8.12   | API documentation                                           |
+----------+----------+-------------------------------------------------------------+
| Inkscape | 0.47     | Used to convert vector graphic formats                      |
+----------+----------+-------------------------------------------------------------+
| LaTeX    | 2e       | Technical and science documentation is prepared using LaTeX |
+----------+----------+-------------------------------------------------------------+

Gource is just a bit of fun so it gets a separate section:

+-----------+---------+---------------------------------+-----------------------------------------------------+
| Package   | Version | Note                            | Purpose                                             |
+===========+=========+=================================+=====================================================+
| Boost     | 1.60.0  |                                 | C++ template library used by Gource                 |
+-----------+---------+---------------------------------+-----------------------------------------------------+
| GLEW      | 1.13.0  |                                 | OpenGL Extension Wrangler used by SDL               |
+-----------+---------+---------------------------------+-----------------------------------------------------+
| SDL       | 2.0.4   |                                 | Simple Directmedia Layer used by Gource             |
+-----------+---------+---------------------------------+-----------------------------------------------------+
| SDL Image | 2.0.1   |                                 | Image loading component of SDl, used by Gource      |
+-----------+---------+---------------------------------+-----------------------------------------------------+
| FFMPEG    | 3.0     |                                 | Video transcoding tool, used to create video output |
+-----------+---------+---------------------------------+-----------------------------------------------------+
| Gource    | 0.43    | `Hosted on GitHub`__            | Repository visualisation tool                       |
+-----------+---------+---------------------------------+-----------------------------------------------------+

__ https://github.com/acaudwell/Gource

Normally the build system suppresses much output in order to reduce clutter.
While getting an initial build to work some of the suppressed output can be
invaluable in making sure the correct paths are being searched. To get this
output use::

  make VERBOSE=1


Checkout a Working Copy
-----------------------

To checkout a working copy of the code to a new directory, named 'trunk' in
this example, run this command::

  svn co https://code.metoffice.gov.uk/svn/lfric/LFRic/trunk trunk

Users of FCM may take advantage of its support for keywords to shorten this.

Met Office developers should find they can use a site-wide keyword "lfric.x"::

  fcm co fcm:lfric.x-tr

Those without these site-wide keywords can set up their own locally. In
``~/.metomi/fcm/keyword.cfg`` just add the following lines::

  # LFRic repository
  location{primary}[lfric] = https://code.metoffice.gov.uk/svn/lfric/LFRic

You may now use the shortened URL::

  fcm co fcm:lfric-tr

The ``-br`` suffix may be used to access the branches directory::

  fmc co fcm:lfric.x-br/dev/joebloggs/r1234_MyBranch

To work in a working copy just change into the directory::

  cd r1234_MyBranch


Running Make
------------

The current LFRic build system uses "Make". Everyting has been set up such
that running ``make`` in top level of a working copy will build the
executables and build and execute the unit tests::

  cd workingcopy
  make

It must be GNU make and a sufficiently modern version is needed. The build
system will check and complain if the version isn't up to scratch but will just
fail if a non-GNU version is used.

Three build profiles are offered:

+------------+---------------------------------------+---------+
| Profile    | Result                                | Default |
+============+=======================================+=========+
| full-debug | No optimisation and run-time checking |         |
+------------+---------------------------------------+---------+
| fast-debug | Safe optimisation only                | Yes     |
+------------+---------------------------------------+---------+
| production | Risky optimisation                    |         |
+------------+---------------------------------------+---------+

All profiles include debug symbols into the executable code.

Pass the ``PROFILE`` variable to make in order to select one of these profies::

  make PROFILE=production

In order for PSyclone to select the correct optimisation script it must know
the platform you are building on. This is achieved by setting the
``OPTIMISATION_PROFILE`` environment variable to a single platform identifier,
as defined above.

Use ``make clean`` to remove all compiled application and unit test output.

LFRic model binaries may be found in the ``bin`` directory in the top level of
your working copy.

.. NOTE::
   At present the unit tests are only know to compile with Intel and GNU
   Fortrans. If you are not using these, running ``make`` in the top level of
   the working copy will produce errors which can be ignored if you're not
   interested in the unit tests.

Running The Gung Ho Binary
--------------------------

The binary for Gung Ho can be found in the ``bin`` directory in the top level
of your working copy. It expects to find a grid file in the current working
directory. The quickest way to execute it is as follows::

  cd gungho/data
  ../../bin/gungho

Explicitly Running The Unit Tests
---------------------------------

The unit tests can be built and run from the top level directory with the
following::

  make test

Running the Test Suite
----------------------

The test suite requires `Cylc <https://github.com/cylc/cylc>`_ and `Rose
<https://github.com/metomi/rose>`_ to run. It makes use of the "Rose Stem"
test launch tool.

Once they are installed and working, set the environment variable
`TEST_SUITE_TARGETS` to a space separated list of target platforms taken
from `rose-stem/opt/rose-suite-<target>.conf`.

The `test-suite` make target may then be used thus::

  export TEST_SUITE_TARGETS=place-machine
  make test-suite

The Met Office environment module sets up this variable for local platforms.

Using the command ``rose stem`` will launch the suite against the Met Office
SPICE server farm. This is useful during development.

Building The Documentation
--------------------------

The Documentation can be built and run from the top level directory with the
following::

  make docs

To view Doxygen documentation for the !GungHo science code point a browser at::
``documentation/gungho/api/index.html``

For the software infrastructure, point a browser at:
``documentation/infrastructure/api/index.html``

A pdf of the scientific formulation is found at:
``documentation/gungho/formulation.pdf``

A pdf that provides an introduction to the data model is found at:
``documentation/gungho/datamodel.pdf``


Possible Issues
---------------

Slow builds
~~~~~~~~~~~

You may find that builds stall around dependency analysis. If this is the case
refer to `Dynamo/BuildSystem#RelocateBuildArtifacts`:trac:.
