# line-search recomputing

import os
from __Parameters__ import WorkDir, ProcDir, ToolsDir, DataDir
from __Parameters__ import mod, step_range, nproc, mpi_version


############################################
src_rec = WorkDir + '/src_rec'
solver = ProcDir + '/solver'
optimize_dir = ProcDir + '/optimize/' + mod
update_dir = optimize_dir + '/update_models'
sources = src_rec + '/sources.dat'
preproc_dir = ProcDir + '/pre_proc'
meas_dir = ProcDir + '/measure_adj'
############################################
nproc_job = nproc
# nproc_job = 40
wall_time = '80:00'                         # limit time for one job
job_mode = 'short'                          # computing mode for submitting jobs
                                            # 'debug','short','medium','large' in SUSTECH Taiyi supercomputer
############################################
os.system('seq %s > %s/tmp.dat' % (step_range, WorkDir))
for step in open('%s/tmp.dat' % WorkDir, 'r'):
    step = step.rstrip('\n')
    print('Model: %s  Step length: %s' % (mod, step))
    newmod = mod + '_slen' + step
    flag = True

    job_file = '%s/job06_LS_Recompute_%s.sh' % (WorkDir, newmod)
    if os.path.exists(job_file):
        os.system('rm -f %s/%s' % (WorkDir, job_file))
    err_file = "job06_LS_Recompute.%s_err" % (newmod)
    out_file = "job06_LS_Recompute.%s_out" % (newmod)
    err_file_dir = WorkDir + '/' + err_file
    out_file_dir = WorkDir + '/' + out_file
    if os.path.exists(err_file_dir):
        os.system('rm -f %s' % (err_file_dir))
    if os.path.exists(out_file_dir):
        os.system('rm -f %s' % (out_file_dir))

    solver_mod = solver + '/' + newmod
    preproc_dir_mod = preproc_dir + '/' + newmod
    meas_dir_mod = meas_dir + '/' + newmod
    step_dir = update_dir + '/OUTPUT_MODEL_slen' + step

    with open(job_file, 'w') as myfile:
        myfile.write("#!/bin/bash\n\n")

        myfile.write("#BSUB -J LS_Recompute.%s\n" % (newmod))
        myfile.write("#BSUB -q %s\n" % (job_mode))
        myfile.write("#BSUB -n %d\n" % (nproc_job))
        myfile.write("#BSUB -R \"span[ptile=40]\"\n")
        myfile.write("#BSUB -W %s\n" % (wall_time))
        myfile.write("#BSUB -e %s\n" % (err_file))
        myfile.write("#BSUB -o %s\n\n" % (out_file))

        myfile.write("module load %s\n\n" % (mpi_version))

        for evid in open('%s/job05_LineSearch.%s.lst' % (WorkDir, newmod), 'r'):
            evid = evid.rstrip('\n')
            print('Event %s is recomputing ...' % evid)
            station = evid.split('.')[1]
            index = 0  # index of events in sources.dat
            for i, line in enumerate(open(sources, 'r')):
                if station in line:
                    index = i + 1
                    break
            if index == 0:
                flag = False
                print('STOP, event is not found! Please check')
                break

            # configue sovler, measurement
            os.system('rm -rf %s/OUTPUT_FILES' % (solver_mod + '/' + evid))
            os.system('mkdir -p %s/OUTPUT_FILES/DATABASES_MPI' % (solver_mod + '/' + evid))

            os.system("ln -sf %s/*bin %s/OUTPUT_FILES/DATABASES_MPI/" % (step_dir, solver_mod + '/' + evid))

            os.system('rm -rf %s/ADJOINT_SOURCES*' % (meas_dir_mod + '/' + evid))

            # job_file
            myfile.write("cd %s\n\n" % (solver_mod + '/' + evid))

            myfile.write("echo '%s is processing ...'\n\n" % (evid))
            myfile.write("echo\n")

            # mesh
            myfile.write("date\necho '************************************'\n")
            myfile.write("echo 'running mesher ...'\n")
            myfile.write("mpirun -np %d ./bin/xmeshfem3D\n" % (nproc))
            myfile.write("echo 'done!'\n")
            myfile.write("echo '************************************'\n\n")
            myfile.write("echo\n")

            # xgenerate_databases
            myfile.write("echo '************************************'\n")
            myfile.write("echo 'running database generation ...'\n")
            myfile.write("mpirun -np %d ./bin/xgenerate_databases\n" % (nproc))
            myfile.write("echo 'Mesh done!'\n")
            myfile.write("echo '************************************'\n\n")
            myfile.write("echo\n")

            # xmeshfem3D
            myfile.write("perl change_simulation_type.pl -F\n")
            myfile.write("echo '************************************'\n")
            myfile.write("echo 'running solver (forward) ...'\n")
            myfile.write("mpirun -np %d ./bin/xspecfem3D\n" % (nproc))
            myfile.write("echo 'Solver done!'\n")
            myfile.write("echo '************************************'\n")
            myfile.write("date\necho\necho\n\n")

            # rename
            myfile.write("echo 'rename *.semd timestamp*'\n")
            myfile.write("rename timestamp timestamp_fwd OUTPUT_FILES/timestamp*\n")
            myfile.write("rename .semd .fwd.semd OUTPUT_FILES/*.semd\n\n")
            myfile.write("echo\necho\n\n")

            # **************************************************************
            # Run pre-processing
            # **************************************************************
            myfile.write("cd %s\n\n" % (preproc_dir_mod))
            myfile.write("./run %d\n\n" % (index))

            # **************************************************************
            # Run measure_adj
            # **************************************************************
            myfile.write("cd %s\n\n" % (meas_dir_mod))
            myfile.write("./run %d\n\n" % (index))

            # **************************************************************
            # delete files
            # **************************************************************
            myfile.write("cd %s\n\n" % (solver_mod + '/' + evid))
            myfile.write("rm -rf OUTPUT_FILES/DATABASES_MPI/*absorb_field.bin\n")
            myfile.write("rm -rf OUTPUT_FILES/DATABASES_MPI/*.vtk\n\n\n")

            myfile.write("echo '=================================================================='\n")
            myfile.write("echo '=================================================================='\n\n\n")

    if flag:
        os.system('bsub < %s' % (job_file))

os.system("rm -f %s/tmp.dat" % WorkDir)