#!/usr/bin/env python3

#the maxmum distance

import sys, os, subprocess, glob

srcfolder = "/home/ose-zhaoy/ELIP/data_processing/02_region_station"
os.chdir(srcfolder)

dist_max = 0
for evtfile in glob.glob('X1.*'):
	os.chdir(srcfolder+'/'+evtfile)
	for sacfile in glob.glob('*.sac'):
		(status,dist) = subprocess.getstatusoutput("saclst DIST f %s |awk '{print $2}'" % (sacfile))
		dist_num = float(dist)
		if dist_num > dist_max:
			dist_max = dist_num
			evt_max = sacfile
print(sacfile,dist_max)
			



