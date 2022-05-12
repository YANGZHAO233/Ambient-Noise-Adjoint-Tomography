# choose dt and step from OUTPUT_FILES/output_meshfem3D.txt

import os, sys, linecache, re
sys.path.append("..")
from __Parameters__ import WorkDir

############################################
src_rec = WorkDir + '/src_rec'
sources = src_rec + '/sources.dat'
fwd_test = WorkDir + '/fwd_test'
############################################
the_line = linecache.getline(sources, 1)               # get the line of first station
stnm, ntwk, evla, evlo = the_line.split()[0:4]         # station name, network name, longitude, latitude
evtid = ntwk + '.' + stnm

output_meshfem3D = fwd_test + '/' + evtid + '/OUTPUT_FILES/output_meshfem3D.txt'
for line in open(output_meshfem3D, 'r'):
    if 'Maximum suggested time step for simulation' in line and '=' in line:
        print(line, end="")
        max_dt = float(line.split()[-1])

        # method 1: regular expression
        my_dt = re.findall(r"\d{1,}?\.\d{2}", str(max_dt))[0]
        # method 2: split
        # my_dt2 = str(max_dt).split('.')[0] + '.' + str(max_dt).split('.')[1][:2]

        print("dt(time step) suggested in this study   = %s" % my_dt)
        if float(my_dt) >= float(max_dt):
            print("Error!!!dt must be less than Maximum suggested time step\n"
                  "Please reselect Dt.")
        else:
            print("Please modify parameter dt and nstep in __Parameters__.py\n"
                  "(1)dt = %s\n"
                  "(2)nstep is determined by plotting the syntheics of the station pair with maximum spacing.\n"
                  "Complete this step before proceeding to the next step." % my_dt)
        break