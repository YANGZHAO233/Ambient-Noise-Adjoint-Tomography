# configure simulation files

import os
from __Parameters__ import WorkDir, ProcDir, BinDir, ToolsDir
from __Parameters__ import mod, step, num_set


############################################
old_mod = 'M' + '%02d' % (int(mod[1:]) - 1)      # the model name of last iteration
src_rec = WorkDir + '/src_rec'
solver = ProcDir + '/solver'
solver_mod = solver + '/' + mod
optimize_mod = ProcDir + '/optimize/' + old_mod  # kernels and update directory
para_dir = WorkDir + '/parameter_files'
is_delete_solver_mod = True                      # Be careful, solver/$mod stores simulation results for current model.
############################################
print('Copy input files for the forward simulation ...')
if mod == 'M00':
    os.system('cp %s/* %s/bin/' % (BinDir, para_dir))

if os.path.exists(solver_mod) and is_delete_solver_mod:
    os.system('rm -rf %s' % (solver_mod))

start_set, end_set = 1, num_set + 1               # can be changed for specific set
for set in range(start_set, end_set):
    print('########################################')
    print('Model: %s, Set: %s' % (mod, set))
    # mod_set = mod + '.set' + str(set)
    sr_file = src_rec + '/sources_set' + str(set) + '.dat'
    num_evt = len(open(sr_file).readlines())      # number of all stations for each source
    print('%s events have been found!!!' % (num_evt))

    for line in open(sr_file, 'r'):
        stnm, ntwk = line.split()[0:2]
        evtid = ntwk + '.' + stnm
        fwd_dir = solver_mod + '/' + evtid
        os.system('mkdir -p ' + fwd_dir + '/DATA')
        os.system('mkdir -p ' + fwd_dir + '/OUTPUT_FILES/DATABASES_MPI')

        # Link and copy input files required by the forward simulation
        os.system('ln -sf %s/bin %s/' % (para_dir, fwd_dir))
        os.system('ln -sf %s/DATA/meshfem3D_files %s/' % (para_dir, fwd_dir + '/DATA'))
        os.system('cp -r %s/DATA/Par_file %s/' % (para_dir, fwd_dir + '/DATA'))
        os.system('cp -r %s/change_simulation_type.pl %s/' % (ToolsDir + '/ADJOINT_TOMOGRAPHY_TOOLS', fwd_dir))

        # Copy source solution file and station file
        os.system('cp %s/FORCESOLUTION_%s %s' % (src_rec, evtid, fwd_dir + '/DATA/FORCESOLUTION'))
        os.system('cp %s/STATIONS_%s %s' % (src_rec, evtid, fwd_dir + '/DATA/STATIONS'))

        if mod == 'M00':
            # the mode of 'tomo' means using the initial model M00 provided by user.
            os.system("sed -i '/MODEL                           =/c\MODEL     "
                      "                      = tomo' %s" % (fwd_dir + '/DATA/Par_file'))
            # link initial model M00 to event file
            os.system('ln -sf %s/DATA/tomo_files %s/' % (para_dir, fwd_dir + '/DATA'))

        else:
            # the mode of 'gll' means using the model calculated from the last iteration as reference model.
            os.system("sed -i '/MODEL                           =/c\MODEL     "
                      "                      = gll' %s" % (fwd_dir + '/DATA/Par_file'))

            # Change the name of new vp, vs and rho files for next iteration
            step_dir = optimize_mod + '/update_models/OUTPUT_MODEL_slen' + step
            if os.path.exists(step_dir + '/proc000000_vs_new.bin'):
                os.system("rename vp_new.bin vp.bin %s/*vp_new.bin" % (step_dir))
                os.system("rename vs_new.bin vs.bin %s/*vs_new.bin" % (step_dir))
                os.system("rename rho_new.bin rho.bin %s/*rho_new.bin" % (step_dir))

            os.system("ln -sf %s/*bin %s/OUTPUT_FILES/DATABASES_MPI/" % (step_dir, fwd_dir))