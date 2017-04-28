#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################
'''
Python script to plot xz slices along the y=0 and xy slices on a specified level 

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

# Make an empty list to hold the levels we find in the data


x = []
y = []
z = []
levels = []

def process_file_list(filestem):

  # get the list of files to stitch together
  dirlist = glob.glob(filestem)

  # If no files are found then don't try to process them
  if len(dirlist) < 1:
    print("No files found to plot")
  else:

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
       
def make_figure(plotpath, field, timestep, plotlevel):
  # Sort levels in asscending order, this is needed for high order spaces
  sorted_levels = sorted(levels)
  l2h = np.zeros(len(levels))
  for i in xrange(len(levels)):
    for j in xrange(len(levels)):
      if ( sorted_levels[i] == levels[j] ):
        l2h[i] = j


   # get min and max of x,y data for plot axes
  xmin =  min(x[0])
  xmax = max(x[0])
  ymin =  min(y[0])
  ymax = max(y[0])
  zmin = min(levels)*1000.0
  zmax = max(levels)*1000.0

  r2d = 180.0/np.pi;
  nx,ny,nz = 300,180,len(levels)

  #create 2D plot
  x2d = np.linspace(xmin, xmax, nx)
  z2d = np.linspace(zmin, zmax, nz)
  y2d = np.linspace(ymin, ymax, ny)
  zi = np.zeros([ny,nx,len(levels)])

  cc = np.linspace(np.amin(z),np.amax(z),13)

  # xz plot
  xi, yi = np.meshgrid(x2d, y2d)  
  for p in xrange(len(levels)):
    pp = int(l2h[p])
    zi[:,:,p] = griddata((np.asarray(x[p]), np.asarray(y[p])), np.asarray(z[p]), (xi, yi), method='linear')
 
  yi, xi = np.meshgrid(z2d, x2d) 
  dz = np.zeros([nx,len(levels)])
  for i in range(nx):
    dz[i,:] = zi[90,i,:]

  fig = plt.figure(figsize=(10,5))
  cf = plt.contourf(xi *r2d, yi / 1000.0, dz, cc)
  plt.colorbar(cf,  cmap=cm.spectral)
  cl = plt.contour(xi * r2d, yi / 1000.0, dz, cc, linewidths=0.5,colors='k')
  plt.ylim([0, 10])
  plt.title('max: %e, min: %e'%(np.max(dz),np.min(dz)))
  plt.xlabel('Longitude')
  plt.ylabel('z')
  out_file_name = plotpath + "/" "slice_xz_" + field + "_" + timestep +  ".png"
  plt.savefig(out_file_name , bbox_inches='tight')

  # yz plot
  xi, yi = np.meshgrid(x2d, y2d)  
  for p in xrange(len(levels)):
    pp = int(l2h[p])
    zi[:,:,p] = griddata((np.asarray(x[p]), np.asarray(y[p])), np.asarray(z[p]), (xi, yi), method='linear')

  yi, xi = np.meshgrid(z2d, y2d) 
  dz = np.zeros([ny,len(levels)])
  for i in range(ny):
    dz[i,:] = zi[i,150,:]

  fig = plt.figure(figsize=(10,5))
  cf = plt.contourf(xi *r2d, yi / 1000.0, dz, cc)
  plt.colorbar(cf,  cmap=cm.spectral)
  cl = plt.contour(xi * r2d, yi / 1000.0, dz, cc, linewidths=0.5,colors='k')
  plt.ylim([0, 10])
  plt.title('max: %e, min: %e'%(np.max(dz),np.min(dz)))
  plt.xlabel('Latitude')
  plt.ylabel('z')
  out_file_name = plotpath + "/" "slice_yz_" + field + "_" + timestep +  ".png"
  plt.savefig(out_file_name , bbox_inches='tight')

  # xy plot
  fig = plt.figure(figsize=(10,5))
  xi, yi = np.meshgrid(x2d, y2d) 
  dz = zi[:,:,plotlevel]
  cf = plt.contourf(xi *r2d, yi * r2d, dz, cc)
  plt.colorbar(cf,  cmap=cm.spectral)
  cl = plt.contour(xi * r2d, yi * r2d, dz, cc, linewidths=0.5,colors='k')
  plt.xlabel('Longitude')
  plt.ylabel('Latitude')
  out_file_name = plotpath + "/" "slice_xy_" + field + "_" + timestep +  ".png"
  plt.savefig(out_file_name , bbox_inches='tight')
 
if __name__ == "__main__":

  try:
    datapath, fields, timesteps, plotlevel, plotpath = sys.argv[1:6]
  except ValueError:
    print("Usage: {0} <datapath> <field_names> <timestep_list> <plotlevel> <plotpath>".format(sys.argv[0]))
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

      filestem =  datapath + "/gravity_wave_nodal_" + field + "_" + ts + "*"
      
      process_file_list(filestem)
      # Only try to plot if we found some files for this timestep
      if len(levels) > 0:
        make_figure(plotpath,field, ts, plotlevel)

