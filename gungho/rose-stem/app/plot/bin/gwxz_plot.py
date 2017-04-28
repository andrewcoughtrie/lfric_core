#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################
'''
Python script to plot xz slices along the equator and
a hovmoller plot along the equator at height = 5 km of the dcmip test case

This version takes nodal format output files and
interpolates onto a regular grid.

Filename hardcoded.

Levels are determined from the data

This version stitches together a directory of files
and extracts all levels so it can work in the serial
case where there is one file or the parallel case where
there is a file for each processor.

'''


import numpy as np
# Need to set a non-interactive backend for suites
import matplotlib
matplotlib.use('Agg')

import matplotlib.pyplot as plt
import matplotlib.cm as cm

from scipy.interpolate import griddata
import math
import glob
import sys

x = []
y = []
z = []
z0 = []
levels = []
model='diagDynamo'

def process_file_list(filestem0,ts):

  ts0 = 'T000000'
  filestem =  filestem0 + ts0 + "*"
  # get the list of files to stitch together
  dirlist = glob.glob(filestem)
 
  for f in dirlist:
    print "processing file ", f
    fo = open(f, "r")

    # Step through all lines in the file, split the lines
    # and where the level matches the specifed one, append 
    # data to appropriate list 
    for strline in fo:
       strsplit = strline.split()
       # check we got a valid data line
       if (len(strsplit) == 5):
          # get the level
          level = float(strsplit[3])
          # Is the level already in the levels list?
          if (level in levels):
             # If it is then append the data into the correct list
             x[levels.index(level)].append(float(strsplit[0]))
             y[levels.index(level)].append(float(strsplit[1]))
             z0[levels.index(level)].append(float(strsplit[4]))
          else:
             # add the level to the levels list and append
             # corresponding empty lists to x, y and z lists
             levels.append(level)
             x.append([])
             y.append([])
             z0.append([])
             # ...and then append the data
             x[levels.index(level)].append(float(strsplit[0]))
             y[levels.index(level)].append(float(strsplit[1]))
             z0[levels.index(level)].append(float(strsplit[4]))

    fo.close()

  del levels[:]
  del x[:]
  del y[:]


  filestem =  filestem0 + ts + "*"
  # get the list of files to stitch together
  dirlist = glob.glob(filestem)
 
  for f in dirlist:
    print "processing file ", f
    fo = open(f, "r")

    # Step through all lines in the file, split the lines
    # and where the level matches the specifed one, append 
    # data to appropriate list 
    for strline in fo:
       strsplit = strline.split()
       # check we got a valid data line
       if (len(strsplit) == 5):
          # get the level
          level = float(strsplit[3])
          # Is the level already in the levels list?
          if (level in levels):
             # If it is then append the data into the correct list
             x[levels.index(level)].append(float(strsplit[0]))
             y[levels.index(level)].append(float(strsplit[1]))
             z[levels.index(level)].append(float(strsplit[4]))
          else:
             # add the level to the levels list and append
             # corresponding empty lists to x, y and z lists
             levels.append(level)
             x.append([])
             y.append([])
             z.append([])
             # ...and then append the data
             x[levels.index(level)].append(float(strsplit[0]))
             y[levels.index(level)].append(float(strsplit[1]))
             z[levels.index(level)].append(float(strsplit[4]))

    fo.close()

def make_figure(plotpath, field, timestep):       
  plt.figure(figsize=(15,10))

  # get min and max of x,y data for plot axes
  xmin = min(x[0])
  xmax = max(x[0])
  ymin = min(y[0])
  ymax = max(y[0])
  zmin = 0.0
  zmax = 10000.0
  
  r2d = 1.0/1000.0
  nx,ny,nz = 300 ,2,11

  #create 2D plot
  x2d = np.linspace(xmin, xmax, nx)
  z2d = np.linspace(zmin, zmax, nz)
  y2d = 0.0
  xi, yi = np.meshgrid(x2d, y2d)  
  zi = np.zeros([1,nx,len(levels)])
  zi0 = np.zeros([1,nx,len(levels)])
  for p in xrange(len(levels)):
      zi[:,:,p] = griddata((np.asarray(x[p]), np.asarray(y[p])), np.asarray(z[p]), (xi, yi), method='linear')
      zi0[:,:,p] = griddata((np.asarray(x[p]), np.asarray(y[p])), np.asarray(z0[p]), (xi, yi), method='linear')


  yi, xi = np.meshgrid(z2d, x2d) 
  dz = np.zeros([nx,len(levels)])
  for i in range(nx):
    dz[i,:] = zi[0,i,:] - zi0[0,0,:]

  cc = np.linspace(np.amin(dz),np.amax(dz),13)
  cf = plt.contourf(xi *r2d, yi , dz, cc)
  plt.colorbar(cf,  cmap=cm.spectral)
  cl = plt.contour(xi * r2d, yi, dz, cc, linewidths=0.5,colors='k')
  plt.title('max: %e, min: %e'%(np.max(dz),np.min(dz)))

  out_file_name = plotpath + "/" + 'gw_xz' + "_" + timestep +  ".png"
  plt.savefig(out_file_name , bbox_inches='tight')

if __name__ == "__main__":
  
  try:
    datapath, fields, timesteps, plotpath = sys.argv[1:5]
  except ValueError:
    print("Usage: {0} <datapath> <field_names> <timestep_list> <plotpath>".format(sys.argv[0]))
    exit(1)

  # Split out the list of fields
  field_list = fields.split(':')

  # Split out the list of timesteps
  ts_list = timesteps.split(':')

  for field in field_list:

    for ts in ts_list:

      # clear the lists in between plots
      del levels[:]
      del x[:]
      del y[:]
      del z[:]
      del z0[:]

      filestem =  datapath + "/diagDynamo_nodal_" + field + "_"
      process_file_list(filestem, ts)
      # Only try to plot if we found some files for this timestep
      if len(levels) > 0:
        make_figure(plotpath, field, ts)



