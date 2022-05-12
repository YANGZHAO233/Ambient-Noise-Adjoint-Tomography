# modify dt, nstep in parameter_files/DATA/Par_file

import os, sys, linecache
sys.path.append("..")
from __Parameters__ import WorkDir
from __Parameters__ import nstep, dt

############################################
src_rec = WorkDir + '/src_rec'
sources = src_rec + '/sources.dat'
fwd_test = WorkDir + '/fwd_test'
para_dir = WorkDir + '/parameter_files'
############################################

print('configure the parameter_files/Data/Par_file ...')
os.system("sed -i '/NSTEP                           =/c\\NSTEP     "
          "                      = %s' %s" % (nstep, para_dir + '/DATA/Par_file'))

os.system("sed -i '/DT                              =/c\\DT        "
          "                      = %s' %s" % (dt, para_dir + '/DATA/Par_file'))
############################################
the_line = linecache.getline(sources, 1)               # get the line of first station
stnm, ntwk, evla, evlo = the_line.split()[0:4]         # station name, network name, longitude, latitude
evtid = ntwk + '.' + stnm

print('configure the fwd_test/%s/Data/Par_file ...' % evtid)

os.system("sed -i '/NSTEP                           =/c\\NSTEP     "
          "                      = %s' %s" % (nstep, fwd_test + '/' + evtid + '/DATA/Par_file'))

os.system("sed -i '/DT                              =/c\\DT        "
          "                      = %s' %s" % (dt, fwd_test + '/' + evtid + '/DATA/Par_file'))