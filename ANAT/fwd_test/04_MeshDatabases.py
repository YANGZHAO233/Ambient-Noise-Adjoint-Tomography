# submit jobs for mesh and databases

import os, sys, linecache
sys.path.append("..")
from __Parameters__ import WorkDir, ProcDir
from __Parameters__ import mod, nproc, mpi_version


############################################
src_rec = WorkDir + '/src_rec'
sources = src_rec + '/sources.dat'
fwd_test = WorkDir + '/fwd_test'
preproc_dir = fwd_test + '/pre_proc/' + mod
meas_dir = fwd_test + '/measure_adj/' + mod
############################################
nproc_job = nproc                           # number of processors for one job
# nproc_job = 40
wall_time = '00:20'                         # limit time for one job
job_mode = 'debug'                          # computing mode for submitting jobs
                                            # 'debug','short','medium','large' in SUSTECH Taiyi supercomputer
is_generate_mesh = True
is_generate_database = True
############################################
ik = 0      # index of shell script for preprocessing and measurement
job_file = 'job01_mesh_databases.sh'
if os.path.exists(job_file):
    os.system('rm -f %s/%s' % (fwd_test, job_file))

err_file = "job01_mesh_databases_err"
out_file = "job01_mesh_databases_out"
err_file_dir = fwd_test + '/' + err_file
out_file_dir = fwd_test + '/' + out_file
if os.path.exists(err_file_dir):
    os.system('rm -f %s' % (err_file_dir))
if os.path.exists(out_file_dir):
    os.system('rm -f %s' % (out_file_dir))

with open(job_file, 'w') as myfile:
    myfile.write("#!/bin/bash\n\n")

    myfile.write("#BSUB -J mesh_databases\n")
    myfile.write("#BSUB -q %s\n" % (job_mode))
    myfile.write("#BSUB -n %d\n" % (nproc_job))
    myfile.write("#BSUB -R \"span[ptile=40]\"\n")
    myfile.write("#BSUB -W %s\n" % (wall_time))
    myfile.write("#BSUB -e %s\n" % (err_file))
    myfile.write("#BSUB -o %s\n\n" % (out_file))

    myfile.write("module load %s\n\n" % (mpi_version))

    the_line = linecache.getline(sources, 1)        # get the line of first station
    stnm, ntwk, evla, evlo = the_line.split()[0:4]  # station name, network name, longitude, latitude
    evid = ntwk + '.' + stnm

    ik += 1
    # **************************************************************
    # Run forward
    # **************************************************************
    myfile.write("cd %s\n\n" % (fwd_test + '/' + evid))

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

    myfile.write("echo '=================================================================='\n")
    myfile.write("echo '=================================================================='\n\n\n")

os.system('bsub < %s' % (job_file))