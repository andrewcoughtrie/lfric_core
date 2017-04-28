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
levels = []

def process_file_list(filestem):

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
       
def make_xz_figure(plotpath, field, timestep):

  plt.figure()
   # get min and max of x,y data for plot axes
  xmin = min(x[0])
  xmax = max(x[0])
  ymin = min(y[0])
  ymax = max(y[0])
  zmin = 0.0
  zmax = 10000.0
  
  r2d = 180/np.pi;
  nx,ny,nz = 80*4,2,11

  #create 2D plot
  x2d = np.linspace(xmin, xmax, nx)
  z2d = np.linspace(zmin, zmax, nz)
  y2d = 0.0
  xi, yi = np.meshgrid(x2d, y2d)  
  zi = np.zeros([1,nx,len(levels)])
  for p in xrange(len(levels)):
    zi[:,:,p] = griddata((np.asarray(x[p]), np.asarray(y[p])), np.asarray(z[p]), (xi, yi), method='linear')

  yi, xi = np.meshgrid(z2d, x2d) 
  dz = np.zeros([nx,len(levels)])
  for i in range(nx):
    dz[i,:] = zi[0,i,:] - zi[0,0,:]

  cc = np.linspace(-0.12,0.12,13)
  cf = plt.contourf(xi *r2d, yi , dz, cc)
  plt.colorbar(cf,  cmap=cm.spectral)
  cl = plt.contour(xi * r2d, yi, dz, cc, linewidths=0.5,colors='k')
  plt.title('max: %e, min: %e'%(np.max(dz),np.min(dz)))
  out_file_name = plotpath + "/" "dcmip301_xz_" + field + "_" + timestep +  ".png"
  plt.savefig(out_file_name , bbox_inches='tight')

def make_hovmoller_figure(datapath, plotpath, field):

  fig = plt.figure(figsize=(15,10))

  r2d = 180/np.pi;
  nx,ny,nz,nt = 90*4,2,1,11

  t2d = np.zeros([nx,nt])


  for t in range(0,nt):

    timestep = "T"+str(36*t).zfill(6)
    filestem =  datapath + "/diagDynamo_nodal_" + field + "_" + timestep + "*"

    x = []
    y = []
    z = []
    levels = []

    # get the list of files to stitch together
    dirlist = glob.glob(filestem)
 
    for f in dirlist:
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
       

     # get min and max of x,y data for plot axes
    xmin = min(x[0])
    xmax = max(x[0])
    ymin = min(y[0])
    ymax = max(y[0])
    zmin = 0.0
    zmax = 10000.0
  
    #create 2D plot
    x2d = np.linspace(xmin, xmax, nx)
    z2d = 5
    y2d = 0.0
    xi, yi = np.meshgrid(x2d, y2d)   
    zi = np.zeros([1,nx])
    p = 5
    zi[:,:] = griddata((np.asarray(x[p]), np.asarray(y[p])), np.asarray(z[p]), (xi, yi), method='linear')
    for i in range(nx):
      t2d[i,t] = zi[0,i] - zi[0,0]

  time =  np.linspace(0, 3600, nt)
  ti, xi = np.meshgrid(time, x2d)   
  cc = np.linspace(-0.12,0.12,21)
  cf = plt.contourf(xi *r2d, ti , t2d, cc)
  plt.colorbar(cf,  cmap=cm.spectral)
  cl = plt.contour(xi * r2d, ti, t2d, cc, linewidths=0.5,colors='k')
  plt.title('max: %e, min: %e'%(np.max(t2d),np.min(t2d)))

  # Add in constant speeds
  Lz = 20000.0
  N =  0.01
  u1 = (20.0 - N*Lz/(2.0*np.pi))
  u2 = (20.0 + N*Lz/(2.0*np.pi))

  #Convert to degrees
  a=6371229.0/125.0
  u1 = u1/a * r2d
  u2 = u2/a * r2d

  x0 = 2.0/3.0*np.pi * r2d

  plt.plot([x0,x0+u1*3600.0],[0,3600.0],'k',linewidth=4)
  plt.plot([x0,x0+u2*3600.0],[0,3600.0],'k',linewidth=4)

  out_file_name = plotpath + "/" "dcmip301_hovmoller_" + field + "_" + timestep +  ".png"
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

      filestem =  datapath + "/diagDynamo_nodal_" + field + "_" + ts + "*"

      process_file_list(filestem)
      # Only try to plot if we found some files for this timestep
      if len(levels) > 0:
        make_xz_figure(plotpath,field, ts)

    del levels[:]
    del x[:]
    del y[:]
    del z[:]
    make_hovmoller_figure(datapath, plotpath, field)


