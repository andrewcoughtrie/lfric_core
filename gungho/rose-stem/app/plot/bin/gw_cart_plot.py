#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################

import numpy as np
# Need to set a non-interactive backend for suites
import matplotlib
matplotlib.use('Agg')

import matplotlib.mlab as mlab
import matplotlib.pyplot as plt
import matplotlib.cm as cm

from scipy.interpolate import griddata

import math

import glob
import sys


# Make an empty list to hold the levels we find in the data


# Set up some empty lists for
# x,y coordinates and value

# Hardcode the filename


x = []
y = []
z = []
w = []
levels = []

x0 = []
y0 = []
z0 = []
w0 = []
levels0 = []

def process_file_list0(filestem):

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
          if (level in levels0):
             # If it is then append the data into the correct list
             x0[levels0.index(level)].append(float(strsplit[0]))
             y0[levels0.index(level)].append(float(strsplit[1]))
             z0[levels0.index(level)].append(float(strsplit[2]))
             w0[levels0.index(level)].append(float(strsplit[4]))
          else:
             # add the level to the levels list and append
             # corresponding empty lists to x, y and z lists
             levels0.append(level)
             x0.append([])
             y0.append([])
             z0.append([])
             w0.append([])
             # ...and then append the data
             x0[levels0.index(level)].append(float(strsplit[0]))
             y0[levels0.index(level)].append(float(strsplit[1]))
             z0[levels0.index(level)].append(float(strsplit[2]))
             w0[levels0.index(level)].append(float(strsplit[4]))

    fo.close()

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
             z[levels.index(level)].append(float(strsplit[2]))
             w[levels.index(level)].append(float(strsplit[4]))
          else:
             # add the level to the levels list and append
             # corresponding empty lists to x, y and z lists
             levels.append(level)
             x.append([])
             y.append([])
             z.append([])
             w.append([])
             # ...and then append the data
             x[levels.index(level)].append(float(strsplit[0]))
             y[levels.index(level)].append(float(strsplit[1]))
             z[levels.index(level)].append(float(strsplit[2]))
             w[levels.index(level)].append(float(strsplit[4]))

    fo.close()

def make_figure(plotpath, field, timestep):
       
  fig = plt.figure(figsize=(7,7))

  # Sort levels in ascending order, this is needed for high order spaces
  sorted_levels = sorted(levels)
  l2h = np.zeros(len(levels))
  for i in xrange(len(levels)):
    for j in xrange(len(levels)):
      if ( sorted_levels[i] == levels[j] ):
        l2h[i] = j

   # get min and max of x,y data for plot axes
  deltaz = min(z[1])-min(z[0])
  xmin =  min(x[0])
  xmax = max(x[0])
  ymin =  min(y[0])
  ymax = max(y[0])
  zmin = 0.0
  zmax = 10000.0

  r2d = 1.0/1000.0;
  nx,ny,nz = 300,2,len(levels)

  #create 2D plot
  x2d = np.linspace(xmin, xmax, nx)
  z2d = np.linspace(zmin, zmax, nz)
  y2d = 0.0
  xi, yi = np.meshgrid(x2d, y2d)  
  
  mid_z = math.floor(len(levels)/2)
  wi = np.zeros([1,nx,len(levels)])
  wi0= np.zeros([1,nx,len(levels)])
  wi_bg = np.zeros(len(levels))

  for p in xrange(len(levels)):
    pp = int(l2h[p])
    wi[:,:,p] =griddata((np.asarray(x[pp]), np.asarray(y[pp])), np.asarray(w[pp]), (xi, yi), method='linear')
    wi0[:,:,p] =griddata((np.asarray(x[pp]), np.asarray(y[pp])), np.asarray(w0[pp]), (xi, yi), method='linear')
    

# subtracting background profile  
  for l in xrange(len(levels)):
    wi_bg[l] = wi0[0,0,l]  
    wi[:,:,l] -= wi_bg[l]
    
  zi, xi = np.meshgrid(z2d, x2d) 
  dw = np.zeros([nx,len(levels)])
  for i in range(nx):
    dw[i,:] = wi[0,i,:]


  ax = fig.add_subplot(2,1,1) 

  matplotlib.rcParams['contour.negative_linestyle'] = 'solid'
  cc = np.linspace(-0.003,0.003,13)
  cf = ax.contourf(xi *r2d, zi * r2d, dw, cc)
  plt.colorbar(cf,  cmap=cm.spectral)
  cl = ax.contour(xi * r2d, zi*r2d, dw, cc, linewidths=0.5,colors='k')
  ax.set_title('max: %2.4e, min: %2.4e'%(np.max(dw),np.min(dw)))
  ax.set_xlabel('x (km)')
  ax.set_ylabel('z (km)')

  bx =fig.add_subplot(2,1,2)
  bx.plot(xi*r2d, dw[:,mid_z], color='k')
  bx.set_xlabel('x (km)')  
  bx.set_ylabel('theta (K)')
  bx.set_ylim([-0.002, 0.003])  
 
  out_file_name = plotpath + "/" + 'gravity_wave' + "_" + timestep + ".png"
  plt.savefig(out_file_name, bbox_inches='tight')

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
    # clear the lists in between plots
    del levels0[:]
    del x0[:]
    del y0[:]
    del z0[:]
    del w0[:]
    # Create initial data
    filestem =  datapath + "/diagDynamo_nodal_" + field + "_" + "T000000" + "*"
    process_file_list0(filestem)

    for ts in ts_list:

      # clear the lists in between plots
      del levels[:]
      del x[:]
      del y[:]
      del z[:]
      del w[:]

      filestem =  datapath + "/diagDynamo_nodal_" + field + "_" + ts + "*"

      process_file_list(filestem)
      # Only try to plot if we found some files for this timestep
      if len(levels) > 0:
        make_figure(plotpath,field, ts)

