#!/usr/bin/env python3

#add station and event to sacfile

import sys, subprocess, os, glob

tarfolder = "/work/ose-zhaoy/ELIP/checkboard/data"
stafile = "/work/ose-zhaoy/ELIP/checkboard/src_rec/sources.dat"

for line in open(stafile, 'r'):
	esta,net,elat,elon,temp1,temp2 = line.split()
	evtfile = tarfolder+'/'+net+'.'+esta
	if os.path.exists(evtfile):
		os.chdir(evtfile)

		for sacfile in glob.glob('./*.sac'):
			stnm = sacfile.split('.')[2]
			print(esta,stnm)
			for line2 in open(stafile, 'r'):
				sta,net,slat,slon,temp1,temp2 = line2.split()
				if sta == stnm:
					break

			p = subprocess.Popen(['sac'], stdin=subprocess.PIPE)
			s = "r %s\n" % (sacfile)
			s += "ch evlo %s\n" % (elon)
			s += "ch evla %s\n" % (elat)
			#s += "ch evdp %s\n" % (edep)
			s += "ch stla %s\n" % (slat)
			s += "ch stlo %s\n" % (slon)
			s += "ch lovrok true\n"
			s += "ch lcalda true\n"
			s += "ch khole ''\n"
			s += "w over\n" 
			s += "q\n"
			p.communicate(s.encode())


