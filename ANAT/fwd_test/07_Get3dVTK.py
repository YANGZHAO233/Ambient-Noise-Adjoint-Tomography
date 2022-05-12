# get 3d VTK model for Paraview plotting

import os, sys, linecache
sys.path.append("..")
from __Parameters__ import WorkDir
from __Parameters__ import mod, nproc


# ============================================================
src_rec = WorkDir + '/src_rec'
sources = src_rec + '/sources.dat'
fwd_test = WorkDir + '/fwd_test'
# ============================================================
the_line = linecache.getline(sources, 1)               # get the line of first station
stnm, ntwk, evla, evlo = the_line.split()[0:4]         # station name, network name, longitude, latitude
evtid = ntwk + '.' + stnm

evtid_dir = fwd_test + '/' + evtid                     # station directory

resolution = 0                                         # high/low-resolution 1/0
                                                       # resolution is recommanded to be as 0
                                                       # high resolution model shown in Paraview needs large memory.
# ============================================================
run_file = fwd_test + '/xcombine.sh'
if os.path.exists(run_file):
    os.system('rm -f %s' % run_file)

target_dir = fwd_test + '/vtk'
if os.path.exists(target_dir):
    os.system('rm -rf %s' % target_dir)
os.system('mkdir -p %s' % target_dir)

targets = ['vs', 'vp', 'rho']
with open(run_file, 'w') as myfile:
    myfile.write('cd %s\n' % evtid_dir)
    for target in targets:
        myfile.write('./bin/xcombine_vol_data_vtk  0 %d %s %s/OUTPUT_FILES/DATABASES_MPI/ %s/ %d\n\n' \
                    % (nproc-1, target, evtid_dir, target_dir, resolution))

os.system('chmod 755 ' + run_file)
os.system('bash %s' % run_file)
os.system('rm -f %s' % run_file)

print('%s.vtk are generated!' % ".vtk ".join(targets))

