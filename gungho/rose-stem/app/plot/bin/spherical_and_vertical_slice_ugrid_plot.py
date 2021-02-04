#!/usr/bin/env python
# -*- coding: utf-8 -*-
#-----------------------------------------------------------------------------
# (c) Crown copyright 2021 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
#-----------------------------------------------------------------------------
'''
Plots horizontal slices and vertical cross sections from ugrid NetCDF data.
Can loop over fields, slices and time steps.

Plotting is designed for vertical slice transport tests but should be compatible
with planar and spherical domains.
'''
import matplotlib
# Need to set a non-interactive backend for suites
matplotlib.use('Agg')  # noqa: E402
from six.moves import range

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from matplotlib.colors import ListedColormap

import math
import iris
from scipy.interpolate import griddata
from read_data import read_ugrid_data

import sys

#------------------------------------------------------------------------------#
# Rounding functions for neatly determining contour levels
#------------------------------------------------------------------------------#

def roundup(a, digits=0):
    n = 10**-digits
    return round(math.ceil(a / n) * n, digits)

def rounddown(a, digits=0):
    n = 10**-digits
    return round(math.floor(a / n) * n, digits)

#------------------------------------------------------------------------------#
# Main plotting script
#------------------------------------------------------------------------------#

def make_figures(filein, plotpath, field_list, slice_list,
                 time_list, extrusion, testname):


    # If specific model levels / slices are desired, they can be put here
    plot_lat = None
    plot_lon = None
    plot_level = None

#------------------------------------------------------------------------------#
# Set some plotting style details
#------------------------------------------------------------------------------#

    plt.rc('text', usetex=True)
    plt.rc('font', family='serif', size=36)

    # Preset conditions for some specific tests
    # Hard wire in top lid values
    if testname in ['curl_free', 'cylinder', 'div_free',
                    'eternal_fountain', 'rotational', 'translational']:
        spherical = False

        zmin = 0.0
        zmax = 2000.

    elif testname in ['baroclinic', 'aquaplanet', 'spherical']:
        spherical = True

        if testname == 'spherical':
            # This is a special 2D spherical shell
            zmin = 0.0
            zmax = 1.0
        elif spherical:
            zmin = 0.0
            zmax = 30000.  # Assume 30 km lid

    else:
        # Assume a spherical set-up if known test not specified
        spherical = True

    plot_zmin = zmin
    plot_zmax = zmax

    # Some specific field labels
    title_dict = {'theta': r'$\theta \ / $ K',
                  'rho': r'$\rho \ / $ kg m$^{-3}$',
                  'density':r'$\rho \ / $ kg m$^{-3}$',
                  'm_v': r'$m_v \ / $ kg kg$^{-1}$'}

    # Find number of full levels by asking for theta
    cube = read_ugrid_data(filein, 'theta')
    levels_name = cube.dim_coords[-1].name()
    nz_full = len(cube.coord(levels_name).points)

    # Find max horizontal extent for non-spherical domains
    try:
        cube = read_ugrid_data(filein, 'u1')
    except iris.exceptions.ConstraintMismatchError:
        # if u1 is not in cube, then try wind1 (e.g. for transport miniapp)
        cube = read_ugrid_data(filein, 'wind1')

    lon_data = np.around(cube.coord('longitude').points, decimals=5)
    lat_data = np.around(cube.coord('latitude').points, decimals=5)

    plot_lonmax = np.amax(np.abs(lon_data))
    plot_latmax = np.amax(np.abs(lat_data))

    epsilon = 1e-12
    # Assume a sensible domain here
    if np.amin(lon_data) < - epsilon:
        # Assume this means lon=0 in the middle of the domain
        plot_lonmin = -plot_lonmax
    else:
        # Assume this means lon=0 is the min of the domain
        plot_lonmin = 0.0
    if np.amin(lat_data) < - epsilon:
        # Assume this means lat=0 in the middle of the domain
        plot_latmin = -plot_latmax
    else:
        # Assume this means lat=0 is the min of the domain
        plot_latmin = 0.0

#------------------------------------------------------------------------------#
# Extract data
#------------------------------------------------------------------------------#

    for field in field_list:

        cube = read_ugrid_data(filein, field)
        # Vertical levels will be last entry in dimension coords
        levels_name = cube.dim_coords[-1].name()

        try:
            time = np.around(cube.coord('time').points, decimals=1)
        except iris.exceptions.CoordinateNotFoundError:
            # there is probably only one time level
            time = [0.0]

#------------------------------------------------------------------------------#
# Create mesh-grid for plotting on
#------------------------------------------------------------------------------#

        # Compute the horizontal grid
        if spherical:
            lon_data = np.around(cube.coord('longitude').points, decimals=5)
            lat_data = np.around(cube.coord('latitude').points, decimals=5)
            # Make a latitude-longitude grid with spacing of 0.5 degrees
            nlat, nlon = 360, 720
            if plot_lon is None:
                plot_lon = 360
            if plot_lat is None:
                plot_lat = 180
            if plot_level is None:
                plot_level = 0

            length_scaling = 1.0
            height_scaling = 1.0 / 1000.0 # Height in km

            plot_latmin = -90.
            plot_latmax = 90.
            plot_lonmin = -180.
            plot_lonmax = 180.

        else:
            lon_data = np.around(cube.coord('longitude').points, decimals=5)
            lat_data = np.around(cube.coord('latitude').points, decimals=5)
            nlat, nlon =  len(np.unique(lat_data)), len(np.unique(lon_data))
            if plot_lon is None:
                plot_lon = 0
            if plot_lat is None:
                plot_lat = 0
            if plot_level is None:
                plot_level = 0

            length_scaling = 1.0 / 1000.0 # Lengths in km
            height_scaling = 1.0 / 1000.0 # Height in km

        nz = len(cube.coord(levels_name).points)

        lonmin = np.amin(lon_data)
        lonmax = np.amax(lon_data)
        latmin = np.amin(lat_data)
        latmax = np.amax(lat_data)

        # Generate a regular grid to interpolate the data.
        lon1d = np.linspace(lonmin, lonmax, nlon)
        lat1d = np.linspace(latmin, latmax, nlat)

        lat2d, lon2d = np.meshgrid(lat1d, lon1d)

        z1d_full = make_extrusion(extrusion, nz_full, zmin, zmax)
        z1d_half = 0.5*(z1d_full[1:] + z1d_full[0:nz_full-1])

        # Choose whether to use heights from full level set or half levels
        if nz == nz_full:
            z1d = z1d_full
        else:
            z1d = z1d_half

#------------------------------------------------------------------------------#
# Interpolate or read in data
#------------------------------------------------------------------------------#

        # Allow shortcut for plotting all dumped timesteps if 'all' is in time list
        if 'all' in time_list:
            time_list = range(len(time))

        for time_point in time_list:

            t = int(time_point)

            plot_data = np.zeros((nlon, nlat, nz))

            # Interpolate using delaunay triangularization
            for level in range(nz):
                if len(time) > 1:
                    data = cube.data[t, level]
                else:
                    data = cube.data[level]  # There is no time coordinate
                fi = griddata((lon_data, lat_data), data, (lon2d, lat2d), method='linear')
                fi_n = griddata((lon_data, lat_data), data, (lon2d, lat2d), method='nearest')
                fi[np.isnan(fi)] = fi_n[np.isnan(fi)]

                plot_data[:, :, level] = fi

#------------------------------------------------------------------------------#
# Loop through data and pull out meshgrid and data for each slice
#------------------------------------------------------------------------------#

            for slice in slice_list:
                slice_fig = plt.figure(figsize=(15, 10))

                if slice == 'xy':
                    Y_meshgrid, X_meshgrid = np.meshgrid(lat1d * length_scaling, lon1d * length_scaling)
                    slice_data = plot_data[:, :, plot_level]
                    slice_label = r'$z$: %1.1e km,' % (z1d[plot_level] / 1000.)
                    if spherical:
                        plot_xlabel = r'$\phi \ / $ deg' if spherical else r'$x \ / $ km'
                        plot_ylabel = r'$\lambda \ / $ deg' if spherical else r'$y \ / $ km'
                    else:
                        plot_xlabel = r'$x \ / $ km'
                        plot_ylabel = r'$y \ / $ km'
                    plot_xmin = plot_lonmin * length_scaling
                    plot_xmax = plot_lonmax * length_scaling
                    plot_ymin = plot_latmin * length_scaling
                    plot_ymax = plot_latmax * length_scaling

                elif slice == 'xz':
                    if nz < 2:
                        raise ValueError('Cannot plot xz slice for field %s which is only 1 layer thick' % field)
                    Y_meshgrid, X_meshgrid = np.meshgrid(z1d * height_scaling, lon1d * length_scaling)
                    slice_data = plot_data[:, plot_lat, :]
                    if spherical:
                        slice_label = r'$\lambda$: %1.1e deg,' % (lat1d[plot_lat] * length_scaling)
                        plot_xlabel = r'$\phi \ / $ deg'
                    else:
                        slice_label = r'$y$: %1.1e km,' % (lat1d[plot_lat] * length_scaling)
                        plot_xlabel = r'$x \ / $ km'
                    plot_ylabel = r'$z \ / $ km'
                    plot_xmin = plot_lonmin * length_scaling
                    plot_xmax = plot_lonmax * length_scaling
                    plot_ymin = plot_zmin * height_scaling
                    plot_ymax = plot_zmax * height_scaling

                elif slice == 'yz':
                    if nz < 2:
                        raise ValueError('Cannot plot yz slice for field %s which is only 1 layer thick' % field)
                    Y_meshgrid, X_meshgrid = np.meshgrid(z1d * height_scaling, lat1d * length_scaling)
                    slice_data = plot_data[plot_lon, :, :]
                    if spherical:
                        slice_label = r'$\phi$: %1.1e deg,' % (lon1d[plot_lon] * length_scaling)
                        plot_xlabel = r'$\lambda \ / $ deg'
                    else:
                        slice_label = r'$x$: %1.1e km,' % (lon1d[plot_lon] * length_scaling)
                        plot_xlabel = r'$y \ / $ km'
                    plot_ylabel = r'$z \ / $ km'
                    plot_xmin = plot_latmin * length_scaling
                    plot_xmax = plot_latmax * length_scaling
                    plot_ymin = plot_zmin * height_scaling
                    plot_ymax = plot_zmax * height_scaling

                else:
                    raise ValueError('Cannot plot slice %s' % slice)

#------------------------------------------------------------------------------#
# Set up contour details
#------------------------------------------------------------------------------#

                # Special contours for our known tests
                if ((testname in ['cylinder', 'div_free',
                                 'eternal_fountain', 'rotational',
                                 'translational']
                    and field in ['theta', 'density', 'rho', 'm_v'])
                    or (testname == 'curl_free' and field in ['theta', 'm_v'])):

                    # Hardwire contour details
                    # 2.0 is the background value (usually the minimum)
                    # 5.0 is the expected maximum value
                    # Take steps of 0.4 from 1.0 to 6.0 but omit 2.0 for contour lines
                    step = 0.5
                    tracer_background = 2.0
                    tracer_max = 5.0
                    max_field = tracer_max + 2*step
                    min_field = tracer_background - 2*step
                    contour_colours = np.arange(min_field, max_field+step, step=step)
                    contour_lines = []
                    epsilon = 1e-14
                    for contour in contour_colours:
                        if abs(contour - tracer_background) > epsilon:
                            contour_lines.append(contour)
                elif (testname == 'curl_free' and field in ['density', 'rho']):
                    step = 0.5
                    min_field = 0.0
                    max_field = 6.0
                    contour_colours = np.arange(min_field, max_field+step, step=step)
                    contour_lines = np.copy(contour_colours)

                else:
                    # Set some nice contours based on the min and max fields
                    # Values are specific for this slice at this time
                    digits = math.floor(-np.log10(np.amax(plot_data)- np.amin(plot_data)))
                    max_field = roundup(np.amax(plot_data), digits=digits)
                    min_field = rounddown(np.amin(plot_data), digits=digits)
                    step = (max_field - min_field) / 20
                    contour_colours = np.arange(min_field, max_field+step, step=step)
                    contour_lines = np.copy(contour_colours)

#------------------------------------------------------------------------------#
# Set up colour bar details
#------------------------------------------------------------------------------#

                colour_code = 'Blues'  # This looks good also when converted to grey scale
                colour_levels_scaling = 1.2  # We scale colour map to avoid extreme colours

                # Make a colour map for more contours than we'll use
                actual_num_colour_levels = len(contour_colours)
                pure_num_colour_levels = np.ceil(actual_num_colour_levels*colour_levels_scaling)

                pure_cmap = cm.get_cmap(colour_code, pure_num_colour_levels)
                new_colours = pure_cmap(np.linspace(0, 1/colour_levels_scaling),
                                        actual_num_colour_levels)
                cmap = ListedColormap(new_colours)

#------------------------------------------------------------------------------#
# Actually plot fields and contours
#------------------------------------------------------------------------------#

                # Plot field with colours
                cf = plt.contourf(X_meshgrid, Y_meshgrid, slice_data,
                                  contour_colours, cmap=cmap, extend='both')

                # Set colours for over and under shoots
                cf.cmap.set_under('magenta')
                cf.cmap.set_over('yellow')

                # Plot contour lines
                cl = plt.contour(X_meshgrid, Y_meshgrid, slice_data,
                                 contour_lines, linewidths=1.0, colors='k',
                                 linestyle="", extend='min')

#------------------------------------------------------------------------------#
# Plot labels
#------------------------------------------------------------------------------#

                plt.xlim([plot_xmin, plot_xmax])
                plt.ylim([plot_ymin, plot_ymax])
                plt.xticks([plot_xmin, plot_xmax])
                plt.yticks([plot_ymin, plot_ymax])
                plt.xlabel(plot_xlabel, labelpad=-20)
                plt.ylabel(plot_ylabel, labelpad=-20)
                plt.title(slice_label+' max: %2.2e, min: %2.2e, time: %d s'
                          % (np.max(slice_data), np.min(slice_data), time[t]),
                          pad=40, size=30, loc='left')

                try:
                    colour_label = title_dict[field]
                except KeyError:
                    colour_label = field
                    colour_label = colour_label.replace('_', '\_') # make strings latex friendly
                plt.colorbar(cf, cmap=cmap, label=colour_label)

                # Save file
                pngfile = '%s/%s-%s-time%s-%s.png' % \
                    (plotpath, testname, field, time[t], slice)
                plt.savefig(pngfile, bbox_inches='tight')
                plt.close()

#------------------------------------------------------------------------------#
# Set up heights with options for different extrusion types
#------------------------------------------------------------------------------#

def make_extrusion(extrusion, nz, zmin, zmax):

    # Get level heights based on what the stretching is
    if extrusion == 'linear':
        z1d = np.linspace(zmin, zmax, nz)
    elif extrusion == 'quadratic':
        z1d = zmin + (zmax - zmin) * np.array([(float(k) / float(nz)) ** 2
                                               for k in range(nz)])
    elif extrusion == 'geometric':
        stretching = 1.03
        z1d = np.zeros([nz])
        for k in range(nz):
            z1d[k] = zmin + (zmax - zmin) * ((stretching**k - 1)
                                             / (stretching**nz - 1))

    elif extrusion == 'dcmip':
        mu = 15.
        z1d = np.zeros([nz])
        for k in range(nz):
            z1d[k] = zmin + ((zmax - zmin) / (np.sqrt(mu+1) - 1)
                             * (np.sqrt(mu * (float(k)/float(nz))**2 + 1) - 1))
    elif extrusion == 'um':
        if nz != 39:
            raise ValueError('For UM extrusion need 39 levels not %d' % nz)

        z1d = zmin + (zmax - zmin) * np.array([.0000000, .0005095, .0020380,
                 .0045854, .0081519, .0127373, .0183417, .0249651, .0326074,
                 .0412688, .0509491, .0616485, .0733668, .0861040, .0998603,
                 .1146356, .1304298, .1472430, .1650752, .1839264, .2037966,
                 .2246857, .2465938, .2695209, .2934670, .3184321, .3444162,
                 .3714396, .3998142, .4298913, .4620737, .4968308, .5347160,
                 .5763897, .6230643, .6772068, .7443435, .8383348, 1.000000])

    else:
        raise NotImplementedError('Extrusion %s is not implemented' % extrusion)

    return z1d


#------------------------------------------------------------------------------#
# Main function for passing fields to plot
#------------------------------------------------------------------------------#

if __name__ == "__main__":

    try:
        args = sys.argv[:]
        filein, plotpath, fields, slices, times, extrusion, testname = args[1:8]

        # Split out lists of fields, slices and times
        field_list = fields.split(':')
        slice_list = slices.split(':')
        time_list = times.split(':') # Should be indices of dumped times or 'all'

    except ValueError:
        print("Usage: {0} <filein> <plotpath> <field_list> \
              <slice_list> <time_list> <extrusion> <testname>"
              .format(sys.argv[0]))
        exit(1)

    make_figures(filein, plotpath, field_list, slice_list,
                 time_list, extrusion, testname)
