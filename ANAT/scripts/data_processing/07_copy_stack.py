#!/usr/bin/env python3

#copy and stack CCs
#add BHZ

import sys, os, subprocess, glob
srcfolder = "/home/ose-zhaoy/ELIP/data_processing/04_SNR_5"
tarfolder = "/home/ose-zhaoy/ELIP/data_processing/05_stack"

os.chdir(srcfolder)

for evtfile in glob.glob('X1.*'):
	os.makedirs(tarfolder+'/'+evtfile, exist_ok=True)
	os.chdir(srcfolder+'/'+evtfile)

	for sacfile in glob.glob('X1.*'):
		tar,sta = sacfile.split('.')[0:2]
		tar_sta = tar +'.'+ sta
		os.makedirs(tarfolder+'/'+tar_sta, exist_ok=True)

		sacfile_temp1 = sacfile+'_temp1'
		sacfile_temp2 = sacfile+'_temp2'
		p = subprocess.Popen(['sac'], stdin=subprocess.PIPE)
		s = "r %s\n" % (sacfile)
		s += "reverse\n"
		s += "addf %s\n" % (sacfile)
		s += "w %s\n" % (sacfile_temp1)
		s += "cut 0 1000\n"
		s += "r %s\n" % (sacfile_temp1)
		s += "w %s\n" % (sacfile_temp2)
		s += "cut off\n"
		s += "q\n"
		p.communicate(s.encode())

		os.system('cp '+ srcfolder+'/'+evtfile+'/'+sacfile_temp2+' '+tarfolder+'/'+evtfile+'/'+tar_sta+'.BHZ.sac')
		os.system('cp '+ srcfolder+'/'+evtfile+'/'+sacfile_temp2+' '+tarfolder+'/'+tar_sta+'/'+evtfile+'.BHZ.sac')

		tar_sac = tarfolder+'/'+tar_sta+'/'+evtfile+'.BHZ.sac'
		(status,elon) = subprocess.getstatusoutput("saclst EVLO f %s |awk '{print $2}'" % (tar_sac))
		(status,elat) = subprocess.getstatusoutput("saclst EVLA f %s |awk '{print $2}'" % (tar_sac))
		(status,slon) = subprocess.getstatusoutput("saclst STLO f %s |awk '{print $2}'" % (tar_sac))
		(status,slat) = subprocess.getstatusoutput("saclst STLA f %s |awk '{print $2}'" % (tar_sac))

		p = subprocess.Popen(['sac'], stdin=subprocess.PIPE)
		s = "r %s\n" % (tar_sac)
		s += "ch evlo %s\n" % (slon)
		s += "ch evla %s\n" % (slat)		
		s += "ch stla %s\n" % (elat)
		s += "ch stlo %s\n" % (elon)
		s += "wh over\n" 
		s += "q\n"
		p.communicate(s.encode())