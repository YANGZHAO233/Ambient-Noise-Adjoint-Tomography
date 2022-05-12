#!/usr/bin/env python3

#all CCs for X1 stations

import sys, os, subprocess, glob

#stalist = "/home/ose-zhaoy/ELIP/data_processing/station.lst"
srcfolder = "/home/ose-zhaoy/ELIP/data_processing/01_CC/Results/CC_AFTAN"
tarfolder = "/home/ose-zhaoy/ELIP/data_processing/02_all_station_CC"

os.chdir(srcfolder)
for evtfile in glob.glob('X1.*'):
	os.makedirs(tarfolder+'/'+evtfile, exist_ok=True)
	os.chdir(srcfolder+'/'+evtfile)
	for stafile in glob.glob('X1.*'):
		evt2 = stafile.split('_')[1].split('.')[1]
		os.system('cp '+srcfolder+'/'+evtfile+'/'+stafile+'/'+stafile+'_ls.SAC '+tarfolder+'/'+evtfile+'/'+'X1.'+evt2+'.sac')