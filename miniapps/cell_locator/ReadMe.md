Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
For further details please refer to the file LICENCE.original which you
should have received as part of this distribution.

# Cell Locator Miniapp

## Overview

This mini-app finds the cell of an unstructured grid that contains a target point. It also returns 
the parametric coordinates of the point within the unit cell, from which interpolation weights 
can be computed.  

## Prerequisites

This mini-app links to MINT (https://github.com/pletzer/mint), which links to VTK (https://www.vtk.org/) and 
NetCDF (serial C interface only). To build the cell locator miniapp you will need to set 
```
export MINT_DIR=<top-directory-where-mint-is-installed>
export VTK_DIR=<top-directory-where-vtk-is-installed>
export FFLAGS="$FFLAGS -I$MINT_DIR/mod"
```
The mint library (`libmint.a`) and the VTK libraries are expected to be under `$MINT_DIR/lib` and `$VTK_DIR/lib`
respectively. 

### How to build VTK

MINT relies on the VTK API for fast cell search in an unstructured grid. 

```
wget https://www.vtk.org/files/release/8.1/VTK-8.1.1.tar.gz
tar xf VTK-8.1.1.tar.gz
cd VTK-8.1.1
CXX=mpicxx CC=mpicc FC=mpif90 VTK_DIR=$INSTALL_DIR/lib/cmake/vtk-8.1 cmake -D CMAKE_BUILD_TYPE=Release -D CMAKE_INSTALL_PREFIX=$INSTALL_DIR .
make 
make install
```
Note 1: On Mac OS X with the GNU 6.4 MPI compilers, I had to use CC=cc
Note 2: INSTALL_DIR might be `$HOME/usr`
Note 3: The following will build a minimalistic version of VTK on Cray XC-50 systems without rendering:
```
CXX=CC CC=cc FC=ftn \
cmake -DBUILD_SHARED_LIBS=OFF \
-DCMAKE_BUILD_TYPE=Release \
-DCMAKE_INSTALL_PREFIX=$INSTALL_DIR \
-DCMAKE_BUILD_TYPE=Release \
-DVTK_Group_Rendering=OFF \
-DVTK_Group_Standalone=OFF \
-DModule_vtkChartsCore:BOOL=ON \
-DModule_vtkCommonColor:BOOL=ON \
-DModule_vtkCommonCore:BOOL=ON -DModule_vtkCommonDataModel:BOOL=ON -DModule_vtkCommonTransforms:BOOL=ON \
-DModule_vtkFiltersCore:BOOL=ON -DModule_vtkCommonMath:BOOL=ON -DModule_vtkCommonExecutionModel:BOOL=ON \
-DModule_vtkIOCore:BOOL=ON -DModule_vtkIOLegacy:BOOL=ON \
-DModule_vtkImagingCore:BOOL=ON \
-DModule_vtkCommonSystem:BOOL=ON -DModule_vtksys:BOOL=ON \
-DModule_vtkCommonMisc:BOOL=ON .
```

### How to build MINT

```
wget https://github.com/pletzer/mint/releases/download/v1.0.0/mint-v1.0.0.tar.gz
cd mint
CXX=mpicxx CC=mpicc FC=mpif90 cmake -D CMAKE_BUILD_TYPE=Release -D CMAKE_INSTALL_PREFIX=$INSTALL_DIR .
make
make install
```
Note 1: MINT will use the command `nc-config` to determine the location of the NetCDF library. This means
that you currently need to build NetCDF using autoconf. Alternatively yout can also specify the location
of NetCDF manually, eg.
```
CXX=mpicxx CC=mpicc FC=mpif90 cmake -D CMAKE_BUILD_TYPE=Release -D CMAKE_INSTALL_PREFIX=$INSTALL_DIR \
-DNETCDF_INCLUDE_DIR=<path/to/netcdf/include> -DNETCDF_LIBRARY_DIR=<path/to/netcdf/lib> -DNETCDF_LIBRARIES="netcdf hdf5 curl" .
```

On Cray systems replace mpicxx, mpicc and mpif90 by CC, cc and ftn, respectively. Also it is recommended to use a recent 
CMake version (e.g. 3.12 or later).

## How to build this mini-app

Type
```
make build
```
in this directory.


## How to run this mini-app

```
cd example
```

### Serial run

```
../bin/cell_locator cell_locator_configuration.nml
```
will store interpolation results in file cell_ids.nc:
```
netcdf cell_ids {
dimensions:
	number_points_found = 11 ;
	three = 3 ;
variables:
	int point_ids_0(number_points_found) ;
	int64 cell_ids_0(number_points_found) ;
	double pcoords(number_points_found, three) ;
	double dist_error_square(number_points_found) ;
data:

 point_ids_0 = 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ;

 cell_ids_0 = 12370, 6697, 22046, 11117, 12376, 21813, 22729, 14031, 9947, 
    8836, 19499 ;

 pcoords =
  0.487745585779831, 0.703888486947411, 0.724455324860642,
  0.251669747297137, 0.934964437001837, 0.611023510677586,
  0.883522894756573, 0.553806903093141, 0.722443382570227,
  0.291470999250657, 0.86341911477986, 0.322958913853173,
  0.840629369688711, 0.815943734895835, 0.36178865562231,
  0.16022016954327, 0.870611657486359, 0.228263230878948,
  0.675208287669281, 0.986995571101542, 0.293714046388823,
  0.400013406584138, 0.683586922275025, 0.630976123854491,
  0.103272930573458, 0.881433220003601, 0.0921049399450633,
  0.31583190322116, 0.617915342297447, 0.433701172679526,
  0.423835897977182, 0.223951798014559, 0.430862763329643 ;

 dist_error_square = 0, 4.93038065763132e-32, 6.16297582203915e-32, 
    7.88860905221012e-31, 8.0426834477611e-31, 4.93038065763132e-32, 
    7.88860905221012e-31, 7.88860905221012e-31, 1.9760041229413e-31, 
    1.23259516440783e-32, 3.08148791101958e-33 ;
}
``` 
The fact that all dist_error_square values are nearly zero indicates success. Variable `pcoords` gives the parametric 
coordinates of the target point inside the unit cell. Variable `cell_ids_0` are the cell indices (>=0). Note: cell_ids_0 
uses a zero based counting. 

### Parallel run

```
mpiexec -n 6 ../bin/cell_locator cell_locator_configuration.nml
```

When running in MPI using a number of tasks that is a multiple of 6, each MPI process will be the owner of a subset of the grid. 
Each process will then search the target points inside its own domain and silently return if the point 
is not found. Results for each MPI process are stored in separate files `cell_ids_*.nc`, one per MPI task. The files can be 
merged using the command:
```
python merge_results.py -i cell_ids.nc -o cell_ids_merged.nc
```
where `cell_ids_merged.nc should yield the same result as `cell_ids.nc` produced by the serial run. 

### Generating your own set of target points

Run `generate_target_points.py`. Type 
`python generate_target_points.py -h` to see the list of options. 

## Having troubles?

Please contact alexander.pletzer@nesi.org.nz


