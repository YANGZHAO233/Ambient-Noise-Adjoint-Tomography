# submit jobs for forward, preprocessing, measurement and adjoint

import os
from __Parameters__ import WorkDir, ProcDir
from __Parameters__ import num_set, mod, nproc, mpi_version


############################################
src_rec = WorkDir + '/src_rec'
solver_dir = ProcDir + '/solver/' + mod
preproc_dir = ProcDir + '/pre_proc/' + mod
meas_dir = ProcDir + '/measure_adj/' + mod
############################################
nproc_job = nproc                           # number of processors for one job
# nproc_job = 40
wall_time = '80:00'                         # limit time for one job
job_mode = 'short'                          # computing mode for submitting jobs
                                            # 'debug','short','medium','large' in SUSTECH Taiyi supercomputer
is_generate_mesh = True
is_generate_database = True
is_forward_computing = True
is_preproc = True
is_meas = True
is_adj = True                               # computing adjoint is not necessary in last iteration
is_delete_job = False                       # delete job file after submission
is_delete_database = True                   # *absorb_field.bin and *vtk in database directory using large disk space,
                                            # it would be better to set it as True in forward computing to delete them
############################################
ik = 0      # index of shell script for preprocessing and measurement
for i in range(num_set):
    iset = i + 1
    job_file = 'job01_fwd_pre_mea_adj_%s_set%s.sh' % (mod, iset)
    src_rec_file = src_rec + '/' + 'sources_set' + str(iset) + '.dat'
    mod_iset = mod + '.set' + str(iset)

    if os.path.exists(job_file):
        os.system('rm -f %s/%s' % (WorkDir, job_file))

    err_file = "job01_FWD_%s_err" % (mod_iset)
    out_file = "job01_FWD_%s_out" % (mod_iset)
    err_file_dir = WorkDir + '/' + err_file
    out_file_dir = WorkDir + '/' + out_file
    if os.path.exists(err_file_dir):
        os.system('rm -f %s' % (err_file_dir))
    if os.path.exists(out_file_dir):
        os.system('rm -f %s' % (out_file_dir))

    with open(job_file, 'w') as myfile:
        myfile.write("#!/bin/bash\n\n")

        myfile.write("#BSUB -J FWD.%s\n" % (mod_iset))
        myfile.write("#BSUB -q %s\n" % (job_mode))
        myfile.write("#BSUB -n %d\n" % (nproc_job))
        myfile.write("#BSUB -R \"span[ptile=40]\"\n")
        myfile.write("#BSUB -W %s\n" % (wall_time))
        myfile.write("#BSUB -e %s\n" % (err_file))
        myfile.write("#BSUB -o %s\n\n" % (out_file))

        myfile.write("module load %s\n\n" % (mpi_version))


        for line in (open(src_rec_file, 'r')):
            stnm, ntwk = line.split()[0:2]
            evid = ntwk + '.' + stnm
            ik += 1
            # **************************************************************
            # Run forward
            # **************************************************************
            myfile.write("cd %s\n\n" % (solver_dir + '/' + evid))

            myfile.write("echo '%s is processing ...'\n\n" % (evid))
            myfile.write("echo\n")

            # mesh
            if is_generate_mesh:
                myfile.write("date\necho '************************************'\n")
                myfile.write("echo 'running mesher ...'\n")
                myfile.write("mpirun -np %d ./bin/xmeshfem3D\n" % (nproc))
                myfile.write("echo 'done!'\n")
                myfile.write("echo '************************************'\n\n")
                myfile.write("echo\n")

            # xgenerate_databases
            if is_generate_database:
                myfile.write("echo '************************************'\n")
                myfile.write("echo 'running database generation ...'\n")
                myfile.write("mpirun -np %d ./bin/xgenerate_databases\n" % (nproc))
                myfile.write("echo 'Mesh done!'\n")
                myfile.write("echo '************************************'\n\n")
                myfile.write("echo\n")

            # xmeshfem3D
            if is_forward_computing:
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
            if is_preproc:
                myfile.write("cd %s\n\n" % (preproc_dir))
                myfile.write("./run %d\n\n" % (ik))

            # **************************************************************
            # Run measure_adj
            # **************************************************************
            if is_meas:
                myfile.write("cd %s\n\n" % (meas_dir))
                myfile.write("./run %d\n\n" % (ik))

            # **************************************************************
            # Run adjoint
            # **************************************************************
            if is_adj:
                myfile.write("cd %s\n\n" % (solver_dir + '/' + evid))
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
            if is_delete_database:
                myfile.write("cd %s\n\n" % (solver_dir + '/' + evid))
                myfile.write("rm -rf OUTPUT_FILES/DATABASES_MPI/*absorb_field.bin\n")
                myfile.write("rm -rf OUTPUT_FILES/DATABASES_MPI/*.vtk\n\n\n")

            myfile.write("echo '=================================================================='\n")
            myfile.write("echo '=================================================================='\n\n\n")

    os.system('bsub < %s' % (job_file))
    if is_delete_job:
        os.system('rm -rf %s' % (job_file))
