#!/usr/bin/env python3

#select station distance>30km
#output the distance between each station to a file
#there are 164 stations in station.lst, however there are 163 events, check the missed one

import sys, os, subprocess, glob

stalist = "/home/ose-zhaoy/ELIP/data_processing/station.lst"
srcfolder = "/home/ose-zhaoy/ELIP/data_processing/02_region_station"
tarfolder = "/home/ose-zhaoy/ELIP/data_processing/03_station_distance_30km"

os.chdir(srcfolder)
for line in open(stalist, 'r'):
	sta = line.split()[2]
	if os.path.exists(srcfolder+'/X1.'+sta):
		continue
	else:
		print(sta+'dosent exist')

for evtfile in glob.glob('X1.*'):
	os.makedirs(tarfolder+'/'+evtfile, exist_ok=True)
	os.chdir(srcfolder+'/'+evtfile)
	for sacfile in glob.glob('*.sac'):
		(status,dist) = subprocess.getstatusoutput("saclst DIST f %s |awk '{print $2}'" % (sacfile))
		dist_num = float(dist)
		if dist_num >= 30:
			os.system('cp '+srcfolder+'/'+evtfile+'/'+sacfile+' '+tarfolder+'/'+evtfile+'/')
			os.system('cp '+srcfolder+'/'+evtfile+'/'+sacfile+'_snr '+tarfolder+'/'+evtfile+'/')



