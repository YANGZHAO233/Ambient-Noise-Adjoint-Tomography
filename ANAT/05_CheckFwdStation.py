# check if forward or adjont computing is completed
# if not completed, recompute the rest events

import os
from __Parameters__ import WorkDir, ProcDir
from __Parameters__ import mod, nstep, nproc, step, mpi_version


# ============================================================
src_rec = WorkDir + '/src_rec'
sources = src_rec + '/sources.dat'
solver_mod = ProcDir + '/solver/' + mod
old_mod = 'M' + '%02d' % (int(mod[1:]) - 1)
optimize_mod = ProcDir + '/optimize/' + old_mod
step_dir = optimize_mod + '/update_models/OUTPUT_MODEL_slen' + step
preproc_dir = ProcDir + '/pre_proc/' + mod
meas_dir = ProcDir + '/measure_adj/' + mod
# ============================================================
is_check = True
is_recompute = True
is_adj = True                               # if compute adjoint in 04_FwdPreMeaAdj.py
nproc_job = nproc                           # number of processors for one job
# nproc_job = 40
wall_time = '50:00'                         # limit time for one job
job_mode = 'short'                          # computing mode for submitting jobs
                                            # 'debug','short','medium','large' in SUSTECH Taiyi supercomputer
# ============================================================
if is_check:
    num = 0  # number of not completed stations
    os.system('cat /dev/null > %s/job01_%s_fwd_station.lst' % (WorkDir, mod))
    for i, line in enumerate(open(sources, 'r')):
        stnm, ntwk = line.split()[0:2]
        evtid = ntwk + '.' + stnm
        evtid_dir = solver_mod + '/' + evtid
        fwd_step = "%06d"%(nstep)
        if is_adj:
            fwd_file = evtid_dir + '/OUTPUT_FILES/timestamp' + str(fwd_step)
        else:
            fwd_file = evtid_dir + '/OUTPUT_FILES/timestamp_fwd' + str(fwd_step)
        if os.path.exists(fwd_file):
            continue
        else:
            print('%s is not completed, please check!!' % evtid)
            os.system('echo %s >> %s/job01_%s_fwd_station.lst' % (evtid, WorkDir, mod))
            num += 1
    if num == 0:
        print('Congratulations! All stations are completed!')
        os.system('rm -f %s/job01_%s_fwd_station.lst' % (WorkDir, mod))
    else:
        print('Unfortunately %s stations are not completed!' % num)

# ============================================================
if is_recompute and os.path.exists('%s/job01_%s_fwd_station.lst' % (WorkDir, mod)):
    flag = True
    job_file = 'job01_fwd_pre_mea_adj_%s_recompute.sh' % (mod)
    if os.path.exists(job_file):
        os.system('rm -f %s/%s' % (WorkDir, job_file))

    err_file = "job01_FWD_%s_recompute_err" % (mod)
    out_file = "job01_FWD_%s_recompute_out" % (mod)
    err_file_dir = WorkDir + '/' + err_file
    out_file_dir = WorkDir + '/' + out_file
    if os.path.exists(err_file_dir):
        os.system('rm -f %s' % (err_file_dir))
    if os.path.exists(out_file_dir):
        os.system('rm -f %s' % (out_file_dir))

    with open(job_file, 'w') as myfile:
        myfile.write("#!/bin/bash\n\n")

        myfile.write("#BSUB -J FWD.%s\n" % (mod))
        myfile.write("#BSUB -q %s\n" % (job_mode))
        myfile.write("#BSUB -n %d\n" % (nproc_job))
        myfile.write("#BSUB -R \"span[ptile=40]\"\n")
        myfile.write("#BSUB -W %s\n" % (wall_time))
        myfile.write("#BSUB -e %s\n" % (err_file))
        myfile.write("#BSUB -o %s\n\n" % (out_file))

        myfile.write("module load %s\n\n" % (mpi_version))

        for evid in open('%s/job01_%s_fwd_station.lst' % (WorkDir, mod), 'r'):
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

            if mod != 'M00':
                os.system("ln -sf %s/*bin %s/OUTPUT_FILES/DATABASES_MPI/" % (step_dir, solver_mod + '/' + evid))

            os.system('rm -rf %s/ADJOINT_SOURCES*' % (meas_dir + '/' + evid))

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
            myfile.write("cd %s\n\n" % (preproc_dir))
            myfile.write("./run %d\n\n" % (index))

            # **************************************************************
            # Run measure_adj
            # **************************************************************
            myfile.write("cd %s\n\n" % (meas_dir))
            myfile.write("./run %d\n\n" % (index))

            # **************************************************************
            # Run adjoint
            # **************************************************************
            myfile.write("cd %s\n\n" % (solver_mod + '/' + evid))
            myfile.write("echo '%s is processing ...'\n\n" % (evid))
            myfile.write("echo\n")

            # xmeshfem3D
            myfile.write("perl change_simulation_type.pl -b\n")
            myfile.write("date\necho '************************************'\n")
            myfile.write("echo 'running solver (adjoint) ...'\n")
            myfile.write("mpirun -np %d ./bin/xspecfem3D\n" % (nproc))
            myfile.write("echo 'Solver done!'\n")
            myfile.write("echo '************************************'\n")
            myfile.write("date\necho\necho\n\n")

            # **************************************************************
            # delete files
            # **************************************************************
            myfile.write("rm -rf OUTPUT_FILES/DATABASES_MPI/*absorb_field.bin\n")
            myfile.write("rm -rf OUTPUT_FILES/DATABASES_MPI/*.vtk\n\n\n")

            myfile.write("echo '=================================================================='\n")
            myfile.write("echo '=================================================================='\n\n\n")

    if flag:
        os.system('bsub < %s' % (job_file))
