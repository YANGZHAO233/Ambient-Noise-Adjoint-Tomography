#!/usr/bin/env python3

#rm the snr <5 when the period below 50s

import sys, os, subprocess, glob
srcfolder = "/home/ose-zhaoy/ELIP/data_processing/04_SNR_5"

os.chdir(srcfolder)

num_all = 0
num_5 = 0

for evtfile in glob.glob('X1.*'):
	os.chdir(srcfolder+'/'+evtfile)
	for snrfile in glob.glob('*_snr'):
		num_all += 1
		sacfile = snrfile.split('_')[0]

		for line in open(snrfile, 'r'):
			period,left,right,ave = line.split()
			period_num = float(period)
			left_num = float(left)
			right_num = float(right)
			ave_num = float(ave)

			if period_num <= 50:
				if ave_num < 5 and left_num < 5 and right_num < 5:
					num_5 += 1
					os.system('rm -rf '+srcfolder+'/'+evtfile+'/'+sacfile)
					break
	os.system('rm -rf '+srcfolder+'/'+evtfile+'/*_snr')

num_percent = num_5/num_all
print(num_all, num_5, num_percent)	
os.system("find %s -depth -type 'd' -empty -exec rmdir {} \;" %(srcfolder))		