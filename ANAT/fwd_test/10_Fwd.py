# submit jobs for forward

import os, sys, linecache
sys.path.append("..")
from __Parameters__ import WorkDir
from __Parameters__ import mod, nproc, mpi_version


############################################
src_rec = WorkDir + '/src_rec'
sources = src_rec + '/sources.dat'
fwd_test = WorkDir + '/fwd_test'
############################################
nproc_job = nproc                           # number of processors for one job
# nproc_job = 40
wall_time = '02:00'                         # limit time for one job
job_mode = 'short'                          # computing mode for submitting jobs
                                            # 'debug','short','medium','large' in SUSTECH Taiyi supercomputer
is_forward_computing = True
is_delete_database = True                   # *absorb_field.bin and *vtk in database directory using large disk space,
                                            # it would be better to set it as True in forward computing to delete them
############################################
ik = 0      # index of shell script for preprocessing and measurement
job_file = 'job03_Fwd_Pre_Mea.sh'
if os.path.exists(job_file):
    os.system('rm -f %s/%s' % (fwd_test, job_file))

err_file = "job03_Fwd_Pre_Mea_err"
out_file = "job03_Fwd_Pre_Mea_out"
err_file_dir = fwd_test + '/' + err_file
out_file_dir = fwd_test + '/' + out_file
if os.path.exists(err_file_dir):
    os.system('rm -f %s' % (err_file_dir))
if os.path.exists(out_file_dir):
    os.system('rm -f %s' % (out_file_dir))

with open(job_file, 'w') as myfile:
    myfile.write("#!/bin/bash\n\n")

    myfile.write("#BSUB -J Fwd_Pre_Mea\n")
    myfile.write("#BSUB -q %s\n" % (job_mode))
    myfile.write("#BSUB -n %d\n" % (nproc_job))
    myfile.write("#BSUB -R \"span[ptile=40]\"\n")
    myfile.write("#BSUB -W %s\n" % (wall_time))
    myfile.write("#BSUB -e %s\n" % (err_file))
    myfile.write("#BSUB -o %s\n\n" % (out_file))

    myfile.write("module load %s\n\n" % (mpi_version))

    the_line = linecache.getline(sources, 1)  # get the line of first station
    stnm, ntwk, evla, evlo = the_line.split()[0:4]  # station name, network name, longitude, latitude
    evid = ntwk + '.' + stnm

    ik += 1
    # **************************************************************
    # Run forward
    # **************************************************************
    myfile.write("cd %s\n\n" % (fwd_test + '/' + evid))

    myfile.write("echo '%s is processing ...'\n\n" % (evid))
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
    # delete files
    # **************************************************************
    if is_delete_database:
        myfile.write("rm -rf OUTPUT_FILES/DATABASES_MPI/*absorb_field.bin\n")
        myfile.write("rm -rf OUTPUT_FILES/DATABASES_MPI/*.vtk\n\n\n")

    myfile.write("echo '=================================================================='\n")
    myfile.write("echo '=================================================================='\n\n\n")

os.system('bsub < %s' % (job_file))
