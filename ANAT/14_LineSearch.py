# configure line-search computing

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
wall_time = '100:00'                        # limit time for one job
job_mode = 'short'                          # computing mode for submitting jobs
                                            # 'debug','short','medium','large' in SUSTECH Taiyi supercomputer
is_delete_database = True
############################################

os.system('seq %s > %s/tmp.dat' % (step_range, WorkDir))
for step in open('%s/tmp.dat' % WorkDir, 'r'):
    step = step.rstrip('\n')
    print('Model: %s  Step length: %s' % (mod, step))
    newmod = mod + '_slen' + step
    newmod_dir = solver + '/' + newmod
    if os.path.exists(newmod_dir):
        os.system("rm -rf %s" % newmod_dir)
    step_dir = update_dir + '/OUTPUT_MODEL_slen' + step

    for i, line in enumerate(open(sources, 'r')):
        stnm, ntwk = line.split()[0:2]
        evtid = ntwk + '.' + stnm
        fwd_dir = newmod_dir + '/' + evtid

        # ============================== Configure Solver =============================================
        os.system('mkdir -p ' + fwd_dir + '/DATA')
        os.system('mkdir -p ' + fwd_dir + '/OUTPUT_FILES/DATABASES_MPI')
        # Link and copy input files required by the forward simulation
        os.system('ln -sf %s/parameter_files/bin %s/' % (WorkDir, fwd_dir))
        os.system('ln -sf %s/parameter_files/DATA/meshfem3D_files %s/' % (WorkDir, fwd_dir + '/DATA'))
        os.system('cp -r %s/parameter_files/DATA/Par_file %s/' % (WorkDir, fwd_dir + '/DATA'))
        os.system('cp -r %s/parameter_files/DATA/FORCESOLUTION %s/' % (WorkDir, fwd_dir + '/DATA'))
        os.system('cp -r %s/ADJOINT_TOMOGRAPHY_TOOLS/change_simulation_type.pl %s/' % (ToolsDir, fwd_dir))

        # Copy source solution file and station file
        os.system('cp %s/FORCESOLUTION_%s %s' % (src_rec, evtid, fwd_dir + '/DATA/FORCESOLUTION'))
        os.system('cp %s/STATIONS_%s %s' % (src_rec, evtid, fwd_dir + '/DATA/STATIONS'))

        # the mode of 'gll' means using the model calculated from the last iteration as reference model.
        os.system("sed -i '/MODEL                           =/c\MODEL     "
                  "                      = gll' %s" % (fwd_dir + '/DATA/Par_file'))

        os.system("ln -sf %s/*bin %s/OUTPUT_FILES/DATABASES_MPI/" % (step_dir, fwd_dir))

    # ============================= Configure Pre-processing and Measurement =========================
    temp_script = "%s/ConfigPreMeaAdj_%s.py" % (WorkDir, newmod)
    os.system("cp %s/03_ConfigPreMeaAdj.py %s " % (WorkDir, temp_script))
    os.system(" sed -i '/mod_temp = /c\mod_temp = \"%s\" ' %s" % (newmod, temp_script))
    os.system(" sed -i '/is_adj = /c\is_adj = False ' %s" % (temp_script))
    os.system("python3 %s" % temp_script)
    os.system("rm -f %s" % temp_script)

    # ============================= Submit jobs for each step ========================================
    job_file = '%s/job05_LineSearch_%s.sh' % (WorkDir, newmod)
    if os.path.exists(job_file):
        os.system('rm -f %s/%s' % (WorkDir, job_file))
    err_file = "job05_LineSearch.%s_err" % (newmod)
    out_file = "job05_LineSearch.%s_out" % (newmod)
    err_file_dir = WorkDir + '/' + err_file
    out_file_dir = WorkDir + '/' + out_file
    if os.path.exists(err_file_dir):
        os.system('rm -f %s' % (err_file_dir))
    if os.path.exists(out_file_dir):
        os.system('rm -f %s' % (out_file_dir))

    ik = 0
    preproc_dir_mod = preproc_dir + '/' + newmod
    meas_dir_mod = meas_dir + '/' + newmod

    with open(job_file, 'w') as myfile:
        myfile.write("#!/bin/bash\n\n")

        myfile.write("#BSUB -J LineSearch.%s\n" % (newmod))
        myfile.write("#BSUB -q %s\n" % (job_mode))
        myfile.write("#BSUB -n %d\n" % (nproc_job))
        myfile.write("#BSUB -R \"span[ptile=40]\"\n")
        myfile.write("#BSUB -W %s\n" % (wall_time))
        myfile.write("#BSUB -e %s\n" % (err_file))
        myfile.write("#BSUB -o %s\n\n" % (out_file))

        myfile.write("module load %s\n\n" % (mpi_version))

        for i, line in enumerate(open(sources, 'r')):
            stnm, ntwk = line.split()[0:2]
            evtid = ntwk + '.' + stnm
            fwd_dir = newmod_dir + '/' + evtid

            ik += 1
            # **************************************************************
            # Run forward
            # **************************************************************
            myfile.write("cd %s\n\n" % (fwd_dir))

            myfile.write("echo '%s is processing ...'\n\n" % (evtid))
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
            myfile.write("./run %d\n\n" % (ik))

            # **************************************************************
            # Run measure_adj
            # **************************************************************
            myfile.write("cd %s\n\n" % (meas_dir_mod))
            myfile.write("./run %d\n\n" % (ik))

            # **************************************************************
            # delete files
            # **************************************************************
            if is_delete_database:
                myfile.write("cd %s\n" % (fwd_dir))
                myfile.write("rm -rf OUTPUT_FILES/DATABASES_MPI\n\n")

            myfile.write("echo '=================================================================='\n")
            myfile.write("echo '=================================================================='\n\n\n")

    os.system('bsub < %s' % (job_file))

os.system("rm -f %s/tmp.dat" % WorkDir)
