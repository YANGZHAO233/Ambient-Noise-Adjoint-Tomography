#!/usr/bin/env python3

#change BHZ to BXZ

import sys, os, subprocess, glob
srcfolder = "/home/ose-zhaoy/ELIP/data_processing/06_BXZ"
#tarfolder = "/home/ose-zhaoy/ELIP/data_processing/05_stack"

os.chdir(srcfolder)

for evtfile in glob.glob('X1.*'):
#	os.makedirs(tarfolder+'/'+evtfile, exist_ok=True)
	os.chdir(srcfolder+'/'+evtfile)

	for sacfile in glob.glob('X1.*'):
		net,sta,comp,sac0 = sacfile.split('.')
		print(net,sta,comp,sac0)
		os.system('mv '+ srcfolder+'/'+evtfile+'/'+sacfile+' '+srcfolder+'/'+evtfile+'/'+net+'.'+sta+'.BXZ.sac')
