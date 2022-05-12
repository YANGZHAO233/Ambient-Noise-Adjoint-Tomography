# get 3d VTK model for Paraview plotting

import os
import linecache, random
from __Parameters__ import WorkDir, ProcDir, ResultsDir
from __Parameters__ import mod, nproc


# ============================================================
src_rec = WorkDir + '/src_rec'
sources = src_rec + '/sources.dat'
solver_mod = ProcDir + '/solver/' + mod
# ============================================================
num_lines = len(open(sources).readlines())             # number of all stations
particular_line = random.randint(1, num_lines)         # particular station
the_line = linecache.getline(sources, particular_line) # get the line of particular station

stnm, ntwk = the_line.split()[0:2]
evtid = ntwk + '.' + stnm
evtid_dir = solver_mod + '/' + evtid                   # station directory

resolution = 0                                         # high/low-resolution 1/0
                                                       # resolution is recommanded to be as 0
                                                       # high resolution model shown in Paraview needs large memory.
# ============================================================
run_file = WorkDir + '/xcombine.sh'
if os.path.exists(run_file):
    os.system('rm -f %s' % run_file)

target_dir = ResultsDir + '/' + mod + '/vtk'
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
print('vtk files saved in %s' % target_dir)

