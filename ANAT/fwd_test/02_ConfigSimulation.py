# configure simulation files

import os, sys, linecache
sys.path.append("..")
from __Parameters__ import WorkDir, ProcDir, BinDir, ToolsDir
from __Parameters__ import mod


############################################
src_rec = WorkDir + '/src_rec'
sources = src_rec + '/sources.dat'
para_dir = WorkDir + '/parameter_files'
fwd_test = WorkDir + '/fwd_test'
############################################
print('Copy input files for the forward simulation ...')
if mod == 'M00':
    os.system('cp %s/* %s/bin/' % (BinDir, para_dir))

print('########################################')
the_line = linecache.getline(sources, 1)               # get the line of first station
stnm, ntwk, evla, evlo = the_line.split()[0:4]         # station name, network name, longitude, latitude
evtid = ntwk + '.' + stnm

fwd_dir = fwd_test + '/' + evtid
os.system('mkdir -p ' + fwd_dir + '/DATA')
os.system('mkdir -p ' + fwd_dir + '/OUTPUT_FILES/DATABASES_MPI')

# Link and copy input files required by the forward simulation
os.system('ln -sf %s/bin %s/' % (para_dir, fwd_dir))
os.system('ln -sf %s/DATA/meshfem3D_files %s/' % (para_dir, fwd_dir + '/DATA'))
os.system('cp -r %s/DATA/Par_file %s/' % (para_dir, fwd_dir + '/DATA'))
os.system('cp -r %s/change_simulation_type.pl %s/' % (ToolsDir + '/ADJOINT_TOMOGRAPHY_TOOLS', fwd_dir))

# Copy source solution file and station file
os.system('cp %s/FORCESOLUTION %s' % (fwd_test, fwd_dir + '/DATA/FORCESOLUTION'))
os.system('cp %s/STATIONS %s' % (fwd_test, fwd_dir + '/DATA/STATIONS'))


# the mode of 'tomo' means using the initial model M00 provided by user.
os.system("sed -i '/MODEL                           =/c\MODEL     "
          "                      = tomo' %s" % (fwd_dir + '/DATA/Par_file'))
# link initial model M00 to event file
os.system('ln -sf %s/DATA/tomo_files %s/' % (para_dir, fwd_dir + '/DATA'))