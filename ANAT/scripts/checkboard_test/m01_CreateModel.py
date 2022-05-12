#!/usr/bin/env python3

import os, sys
import numpy as np
from math import sqrt
from geoTools import hanning
sys.path.append("../..")
from __Parameters__ import WorkDir

###########################################
# Set model parameters for the checkerboard
is_get_coordinates_from_MeshParFile = False
Mesh_Par_file = WorkDir + '/parameter_files/DATA/meshfem3D_files/Mesh_Par_file'
###########################################
global lon1, lon2, lat1, lat2
if is_get_coordinates_from_MeshParFile:
	for line in open(Mesh_Par_file, 'r'):
		if 'LONGITUDE_MIN' in line and '=' in line:
			lon1 = float(line.split()[2].split('d')[0])
		if 'LONGITUDE_MAX' in line and '=' in line:
			lon2 = float(line.split()[2].split('d')[0])
		if 'LATITUDE_MIN' in line and '=' in line:
			lat1 = float(line.split()[2].split('d')[0])
		if 'LATITUDE_MAX' in line and '=' in line:
			lat2 = float(line.split()[2].split('d')[0])
else:
	lon1 = 98
	lon2 = 106
	lat1 = 23
	lat2 = 31
				# study region

dlon = 0.02		# grid spacing in longitude
dlat = 0.02		# grid spacing in latitude
dv = -0.10		# velocity anomaly in percent
rmax = 0.5		# radius of the anomaly
dstep = 0.9		# spacing between alternating low- and high-velocity anomalies
lon0 = 99.7		# longitude of the starting point
lat0 = 24.7		# latitude of the starting point
nx = 6			# number of anomalies in longitude direction
ny = 5			# number of anomalies in latitude direction
pow = 1			# pow for the hanning tappering function

###########################################
nx0 = int((lon2 - lon1) / dlon) + 1
ny0 = int((lat2 - lat1) / dlat) + 1
data = []

###########################################
for j in range(ny0):
	lat = lat1 + dlat*j
	for i in range(nx0):
		lon = lon1 + dlon*i
		v = 0.0
		data.append((lon, lat, v))

###########################################
isign = 1
for j in range(ny):
	lat_c = lat0 + dstep*j
	isign = -1 * isign
	jsign = isign
	for i in range(nx):
		lon_c = lon0 + dstep * i
		for k, item in enumerate(data):
			lon, lat, v = item
			r = sqrt((lon-lon_c)*(lon-lon_c)+(lat-lat_c)*(lat-lat_c))
			if r <= rmax:
				data[k] = (lon, lat, dv*hanning(r, rmax, pow)*jsign)
		jsign = -1*jsign

###########################################
with open('perturb_10percent.dat', 'w') as myfile:
	for item in data:
		lon, lat, v = item
		myfile.write("%12.4f %12.4f %10.4f\n"%(lon, lat, v))
