# choose channel from OUTPUT_FILES/

import os, sys, linecache
sys.path.append("..")
from __Parameters__ import WorkDir

############################################
src_rec = WorkDir + '/src_rec'
sources = src_rec + '/sources.dat'
fwd_test = WorkDir + '/fwd_test'
############################################
# master station
the_line = linecache.getline(sources, 1)               # get the line of first station
stnm, ntwk, evla, evlo = the_line.split()[0:4]         # station name, network name, longitude, latitude
evtid = ntwk + '.' + stnm

# receiver station
the_line = linecache.getline(sources, 2)               # get the line of second station
stnm, ntwk, stla, stlo = the_line.split()[0:4]         # station name, network name, longitude, latitude
stid = ntwk + '.' + stnm

OUTPUT_FILES = fwd_test + '/' + evtid + '/OUTPUT_FILES'
list_file = os.listdir(OUTPUT_FILES)
channel = "None"
for i in list_file:
    temp = i.split('.')[0:2]
    temp = '.'.join(temp)
    if temp == stid:
        channel = i.split('.')[2][0:2]
        break

if channel == "None":
    print("Error!!!please check!")
else:
    print("channel = %s" % channel)
    print("Please modify parameter channel in __Parameters__.py\n"
          "Complete this step before proceeding to the next step.")
