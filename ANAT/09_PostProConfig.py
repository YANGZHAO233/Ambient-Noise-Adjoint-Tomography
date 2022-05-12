# configure Post-Processing for kernels computing

import os, random, linecache
from __Parameters__ import WorkDir, ProcDir
from __Parameters__ import mod, step_range
from glob import glob


############################################
solver_mod = ProcDir + '/solver/' + mod
optimize_dir = ProcDir + '/optimize/' + mod
sum_kern_dir = optimize_dir + '/sum_kernels'
update_dir = optimize_dir + '/update_models'
src_rec = WorkDir + '/src_rec'
sources = src_rec + '/sources.dat'
para_dir = WorkDir + '/parameter_files'
############################################
is_sum_kern = True                          # if sum kernels
is_update = True                            # if update model

if os.path.exists(optimize_dir):
    os.system('rm -rf %s' % optimize_dir)
###################################################################
# particular station
num_lines = len(open(sources).readlines())              # number of all stations
particular_line = random.randint(1, num_lines)          # particular station
the_line = linecache.getline(sources, particular_line)  # get the line of particular station

stnm, ntwk = the_line.split()[0:2]
evtid = ntwk + '.' + stnm
evtid_dir = solver_mod + '/' + evtid                    # station directory
###################################################################

if is_sum_kern:
    os.system('mkdir -p %s' % sum_kern_dir)
    os.system('ln -sf %s/bin %s/' % (para_dir, sum_kern_dir))
    os.system('ln -sf %s/DATA %s/' % (para_dir, sum_kern_dir))
    os.system('ln -sf %s/OUTPUT_FILES %s/' % (evtid_dir, sum_kern_dir))

    os.system('mkdir -p %s/OUTPUT_SUM' % sum_kern_dir)
    os.system('mkdir -p %s/OUTPUT_PREC_KERNELS' % sum_kern_dir)
    os.system('mkdir -p %s/INPUT_KERNELS' % sum_kern_dir)
    os.system('mkdir -p %s/OUTPUT_SMOOTH_KERNELS' % sum_kern_dir)

    with open(sum_kern_dir + '/kernels_list.txt', 'w') as myfile:
        for isrc in sorted(os.listdir(solver_mod)):
            print(isrc + ' is processing ...')
            srcdir = solver_mod + '/' + isrc
            evtdir = sum_kern_dir + '/INPUT_KERNELS/' + isrc + '_DATABASES_MPI'

            os.makedirs(evtdir)
            os.system('ln -sf %s %s/' % (srcdir + '/OUTPUT_FILES/DATABASES_MPI/*kernel.bin', evtdir))

            numker = len(glob(evtdir + '/*.bin'))
            print('    number of kernel files: ', numker)

            if numker > 1:
                myfile.write('%s\n' % (os.path.basename(evtdir)))

if is_update:
    os.system('mkdir -p %s' % update_dir)

    os.system('seq %s > %s/step_len.dat' % (step_range, update_dir))

    os.system('ln -sf %s/bin %s/' % (para_dir, update_dir))
    os.system('ln -sf %s/DATA %s/' % (para_dir, update_dir))
    os.system('ln -sf %s/OUTPUT_FILES %s/' % (evtid_dir, update_dir))

    os.system('ln -sf %s/OUTPUT_SMOOTH_KERNELS %s/INPUT_GRADIENT' % (sum_kern_dir, update_dir))
    os.system('ln -sf %s/OUTPUT_FILES/DATABASES_MPI %s/INPUT_MODEL' % (evtid_dir, update_dir))
    os.system('ln -sf %s/OUTPUT_FILES/DATABASES_MPI %s/topo' % (evtid_dir, update_dir))