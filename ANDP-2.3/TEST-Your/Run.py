#!/usr/bin/env python3

#=======================================================================================
#=======================================================================================
# This script is the only one that you need to execute to process the seismic ambient
# noise data with parallel computing. It will invoke the <AND_Driver>, <TF_PWS> and
# <AFTAN_PROG> three executables to deal with the input seismic waveforms based on input
# parameters as defined in <input.dat>. Detailed description of each individual parameter
# can be found in <input.dat>. The output of this program is cross-correlation functions
# and corresponding group and phase velocity dispersion data.
#=======================================================================================
# The path of input data folder is given in the <SACfolder> variable in the following
# script. The data structure should be */year/month/event/net.sta.cha.SAC
# e.g. DATA/2018/01/20180101_120000/DB.EW01.LHZ.SAC
#=======================================================================================
# Individual or combined full RESP or PZ file(s) should be put in <RESPfolder>.
#=======================================================================================
# 1-D phase velocity reference model used in AFTAN is given by <ref1Dmod.dat>. It has
# the format of (period, reference phase velocity)
#=======================================================================================
# The path of output data folder is given in the <tarfolder> variable in the following
# script. The ouptput 'CC_AFTAN' folder contains all the cross-correlation waveforms and
# corresponding dispersion measurements. The output 'FINAL' folder contains all the final
# dispersion data. The output 'BOOTSTRAP' folder contains all the bootstrap data (optional).
#=======================================================================================
# Required auxiliary files and programs:
#   sac
#   myTools.py
#   input.dat
#   ref1Dmod.dat
#=======================================================================================
# Notes: The algorithm of this program is modified from Barmin's code (http://ciei.color
#        ado.edu/Products/), including the cross-correlation part and the frequency-time
#        analysis part. The phase-weighted stacking program is modified from Guoliang Li'
#        code (guoleonlee@gmail.com)
#        Any bug reports, suggestions or comments related to this program can be directed
#        to Xingli Fan via <fanxldengcm@gmail.com>.
#=======================================================================================
#=======================================================================================

import os
from myTools import GetPZ

###########################################################
# Please modify the following three path varialbes with
# your own case and set the right core numbers for parallel
# processing according to your own computing resource.
###########################################################
SACfolder = "/home/fanxl/Documents/My-Code/ANDP/ANDP-2.3/DATA_1Hz"
RESPfolder = "/home/fanxl/Documents/My-Code/ANDP/ANDP-2.3/RESPfiles"
tarfolder = "/home/fanxl/Documents/My-Code/ANDP/ANDP-2.3/Results"
ncores = 28
###########################################################

# Reomve empty folder(s) and file(s)
###########################################################
os.system('find %s -depth -type "d" -empty -exec rmdir {} \;' %(SACfolder))
os.system('find %s -name "*" -type f -size 0c | xargs -n 1 rm -f' % (SACfolder))

# Retrive the station and event infomation.
###########################################################
os.system('rm -rf stations.junk events.lst')

for year in sorted(os.listdir(SACfolder)):
    for month in sorted(os.listdir(SACfolder+'/'+year)):
        for day in sorted(os.listdir(SACfolder+'/'+year+'/'+month)):
            sacfiles = SACfolder+'/'+year+'/'+month+'/'+day+'/*.SAC'
            os.system("saclst knetwk kstnm stlo stla delta kcmpnm f "+sacfiles+" | gawk \
            '{printf \"%s %6s %10.4f %10.4f %8.4f %6s\\n\",$2,$3,$4,$5,$6,$7}' >> stations.junk")
        dayfolders = SACfolder+'/'+year+'/'+month+'/*'
        os.system("ls %s -d >> events.lst"%(dayfolders))
os.system("sort stations.junk | uniq > stations.lst")
os.system("rm -rf stations.junk")

# Return if <stations.lst> contains duplicate stations
###########################################################
stations = []
duplicates = []
for line in open('stations.lst', 'r'):
    net, sta = line.split()[0:2]
    name = net+'.'+sta
    if name in stations:
        duplicates.append(name)
    else:
        stations.append(name)

if len(duplicates) > 0:
    print('Error: The following stations have multiple longitude or latitude or sampling rate.\n\
       Please check <stations.lst> for details and correct the corresponding sac \n\
       headers before running this program.')
    print(duplicates)
    exit()

# Convert all the instrument response files to each individual
# PZ file for one station which will be put in current
# [PZfiles] folder.
###########################################################
GetPZ(RESPfolder, 'PZfiles')

# Execute the main program to process the ambient noise data.
###########################################################
os.system("mpirun -np %d AND_Driver %s"%(ncores, tarfolder))
