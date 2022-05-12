#!/usr/bin/env python3

#select study region stations

import sys, os, subprocess, glob

stalist = "/home/ose-zhaoy/ELIP/data_processing/station.lst"
srcfolder = "/home/ose-zhaoy/ELIP/data_processing/01_CC/Results/CC_AFTAN"
tarfolder = "/home/ose-zhaoy/ELIP/data_processing/02_region_station"

os.chdir(srcfolder)
for evtfile in glob.glob('X1.*'):
	evt = evtfile.split('.')[1]
	for line in open(stalist, 'r'):
		sta = line.split()[2]
		if sta == evt:
			os.makedirs(tarfolder+'/'+evtfile, exist_ok=True)
			print(evtfile+'in the region, continue!')

			os.chdir(srcfolder+'/'+evtfile)
			for stafile in glob.glob('X1.*'):
				evt2 = stafile.split('_')[1].split('.')[1]
				for line2 in open(stalist, 'r'):
					sta2 = line2.split()[2]
					if sta2 == evt2:
						os.system('cp '+srcfolder+'/'+evtfile+'/'+stafile+'/'+stafile+'_ls.SAC '+tarfolder+'/'+evtfile+'/'+'X1.'+evt2+'.sac')
						os.system('cp '+srcfolder+'/'+evtfile+'/'+stafile+'/'+stafile+'_ls.SAC_snr '+tarfolder+'/'+evtfile+'/'+'X1.'+evt2+'.sac_snr')
			break




