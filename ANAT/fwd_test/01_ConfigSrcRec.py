# configure Data/Para_file and src_rec files
# run this scrip before the all station simulations

import os
import sys
import linecache, random
sys.path.append("..")
from __Parameters__ import WorkDir, ToolsDir
from __Parameters__ import mod
from __Parameters__ import nproc, sac_path

############################################
src_rec = WorkDir + '/src_rec'
sources = src_rec + '/sources.dat'
para_dir = WorkDir + '/parameter_files'
fwd_test = WorkDir + '/fwd_test'
seis_process = ToolsDir + '/seis_process'
process_syn = seis_process + '/process_syn.pl'
process_data = seis_process + '/process_data.pl'
############################################
print('configure the Data/Par_file ...')
os.system("sed -i '/NPROC                           =/c\\NPROC     "
          "                      = %s' %s" % (nproc, para_dir + '/DATA/Par_file'))
############################################
print('configure tools/seis_process ...')
content = open(process_syn, 'r').readlines()
for i, line in enumerate(content):
    if "$ENV{'SACAUX'}" in line and '=' in line:
        content[i] = "$ENV{'SACAUX'}='%s';\n" % (sac_path)
        break
with open(process_syn, 'w') as myfile:
    myfile.writelines(content)

content = open(process_data, 'r').readlines()
for i, line in enumerate(content):
    if "$ENV{'SACAUX'}" in line and '=' in line:
        content[i] = "$ENV{'SACAUX'}='%s';\n" % (sac_path)
        break
with open(process_data, 'w') as myfile:
    myfile.writelines(content)

############################################
print('####################################')
print('Generate cmt and force solutions ...')
print('Create station file for particular source ...')

the_line = linecache.getline(sources, 1)               # get the line of first station

stnm, ntwk, evla, evlo = the_line.split()[0:4]         # station name, network name, longitude, latitude
evid = ntwk + '.' + stnm

forcefile = fwd_test + '/FORCESOLUTION'                # FORCE_SOLUTION file
with open(forcefile, 'w') as myfile:
    myfile.write('FORCE  001\n')
    myfile.write('time shift:     0.0\n')
    myfile.write('f0:             1.0\n')
    myfile.write('latorUTM:       %s\n' % (evla))
    myfile.write('longorUTM:      %s\n' % (evlo))
    myfile.write('depth:          1.0\n')
    myfile.write('factor force source:             1.d15\n')
    myfile.write('component dir vect source E:     0.d0\n')
    myfile.write('component dir vect source N:     0.d0\n')
    myfile.write('component dir vect source Z_UP:  1.d0\n')

cmtfile = fwd_test + '/CMTSOLUTION'                    # CMT_SOLUTION file
with open(cmtfile, 'w') as myfile:
    myfile.write('PDE 2000 1 1 0 0 0.0 %s %s 6.4 4.2 4.2 ELIP\n' % (evla, evlo))
    myfile.write('event name:     ambient noise\n')
    myfile.write('time shift:     0.0\n')
    myfile.write('half duration:  0.0\n')
    myfile.write('latorUTM:       %s\n' % (evla))
    myfile.write('longorUTM:      %s\n' % (evlo))
    myfile.write('depth:          0.0\n')
    myfile.write('Mrr:      -0.100000e+23\n')
    myfile.write('Mtt:      -0.100000e+23\n')
    myfile.write('Mpp:       0.000000e+23\n')
    myfile.write('Mrt:      -0.000000e+23\n')
    myfile.write('Mrp:      -0.000000e+23\n')
    myfile.write('Mtp:       0.000000e+23\n')

# Create station file for each source
content = open(sources, 'r').readlines()

staidfile = fwd_test + '/STATIONS'
with open(staidfile, 'w') as myfile:
    for line2 in content:
        stnm0, ntwk0 = line2.split()[0:2]
        evid0 = ntwk0 + '.' + stnm0
        if evid != evid0:
            myfile.write(line2)
