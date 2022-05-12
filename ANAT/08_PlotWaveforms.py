# plot data and synthetic waveforms

import os
from __Parameters__ import WorkDir, ProcDir, ResultsDir, ScriptsDir
from __Parameters__ import mod, bands, nproc, mpi_version


# ============================================================
seismo_plot = ScriptsDir + '/seismo_plot'
data_syn_images = ResultsDir + '/' + mod + '/data_syn_images'
src_rec = WorkDir + '/src_rec'
sources = src_rec + '/sources.dat'
solver_mod = ProcDir + '/solver/' + mod
solver_mod0 = ProcDir + '/solver/M00'
meas_dir = ProcDir + '/measure_adj/'
misfit_dir = ProcDir + '/misfits/' + mod
# ============================================================
is_compile_run_shell_mpi = True             # if compile the fortran code run_shell_mpi scripts/seismo_plot
is_link_syn_M00 = True                      # link synthetics of M00 to current model in measurement directory
# nproc_job = 40
nproc_job = nproc
wall_time = '00:20'                         # limit time for one job
job_mode = 'debug'                          # computing mode for submitting jobs
                                            # 'debug','short','medium','large' in SUSTECH Taiyi supercomputer
bands = ' '.join(bands)
# ============================================================
run_file = WorkDir + '/run_file.sh'
if is_compile_run_shell_mpi:
    with open(run_file, 'w') as myfile:
        myfile.write("#!/bin/bash\n\n")
        myfile.write("cd %s\n\n" % seismo_plot)
        myfile.write("module load %s\n\n" % mpi_version)
        myfile.write(" mpifort run_shell_mpi.f90 -o run_shell_mpi")
os.system('bash %s' % run_file)
os.system('rm -f %s' % run_file)
# ============================================================
if is_link_syn_M00:
    for i, line in enumerate(open(sources, 'r')):
        stnm, ntwk = line.split()[0:2]
        evtid = ntwk + '.' + stnm
        evtid_dir = solver_mod0 + '/' + evtid
        target_dir = meas_dir + '/' + mod + '/' + evtid
        if os.path.exists('%s/SYN_M00' % target_dir):
            os.system('rm -rf %s/SYN_M00' % target_dir)
        os.system('ln -sf %s/OUTPUT_FILES %s/SYN_M00' % (evtid_dir, target_dir))

if os.path.exists(data_syn_images):
    os.system('rm -rf %s' % data_syn_images)

with open('input.lst', 'w') as myfile:
    for isrc in sorted(os.listdir(solver_mod)):
        myfile.write('./plot_data_syn.sh ' + meas_dir + ' ' + mod + ' ' \
                     + isrc + ' ' + misfit_dir + ' ' + data_syn_images + ' "'+bands+'"\n')
os.system('mv input.lst %s/' % seismo_plot)

##########################################################
# Invoke the MPI fortran program
##########################################################
job_file = '%s/job02_PlotWaveforms_%s.sh' % (seismo_plot, mod)
if os.path.exists(job_file):
    os.system('rm -f %s' % (job_file))

err_file = "job02_PlotWaveforms_%s_err" % (mod)
out_file = "job02_PlotWaveforms_%s_out" % (mod)
err_file_dir = WorkDir + '/' + err_file
out_file_dir = WorkDir + '/' + out_file
if os.path.exists(err_file_dir):
    os.system('rm -f %s' % (err_file_dir))
if os.path.exists(out_file_dir):
    os.system('rm -f %s' % (out_file_dir))

with open(job_file, 'w') as myfile:
    myfile.write("#!/bin/bash\n\n")

    myfile.write("#BSUB -J PlotWaveforms.%s\n" % (mod))
    myfile.write("#BSUB -q %s\n" % (job_mode))
    myfile.write("#BSUB -n %d\n" % (nproc_job))
    myfile.write("#BSUB -R \"span[ptile=40]\"\n")
    myfile.write("#BSUB -W %s\n" % (wall_time))
    myfile.write("#BSUB -e %s\n" % (err_file))
    myfile.write("#BSUB -o %s\n\n" % (out_file))

    myfile.write("module load %s\n\n" % (mpi_version))

    myfile.write('cd %s\n' % (seismo_plot))
    myfile.write('mpirun -np %d run_shell_mpi\n' % (nproc_job))
    myfile.write('rm -rf input.lst\n')

os.system('bsub < %s' % (job_file))
os.system('mv %s %s/' % (job_file, WorkDir))
print('Figure saved in %s' % data_syn_images)

