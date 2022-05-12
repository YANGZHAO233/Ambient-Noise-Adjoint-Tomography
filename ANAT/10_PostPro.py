# kernel computing in post-processing

import os
from __Parameters__ import WorkDir, ProcDir
from __Parameters__ import mod, mpi_version, nproc, hsmooth, vsmooth


############################################
optimize_dir = ProcDir + '/optimize/' + mod
sum_kern_dir = optimize_dir + '/sum_kernels'
update_dir = optimize_dir + '/update_models'
############################################
# nproc_job = 40
nproc_job = nproc
wall_time = '20:00'                         # limit time for one job
job_mode = 'short'                          # computing mode for submitting jobs
                                            # 'debug','short','medium','large' in SUSTECH Taiyi supercomputer
is_sum_kern = True                          # if sum kernels
is_smooth = True                            # if smooth kernels
is_update = True                            # if update model
############################################
job_file = '%s/job03_PostProcessing_%s.sh' % (WorkDir, mod)
if os.path.exists(job_file):
    os.system('rm -f %s' % (job_file))

err_file = "job03_PostPro_%s_err" % (mod)
out_file = "job03_PostPro_%s_out" % (mod)
err_file_dir = WorkDir + '/' + err_file
out_file_dir = WorkDir + '/' + out_file
if os.path.exists(err_file_dir):
    os.system('rm -f %s' % (err_file_dir))
if os.path.exists(out_file_dir):
    os.system('rm -f %s' % (out_file_dir))


with open(job_file, 'w') as myfile:
    myfile.write("#!/bin/bash\n\n")

    myfile.write("#BSUB -J PostPro.%s\n" % (mod))
    myfile.write("#BSUB -q %s\n" % (job_mode))
    myfile.write("#BSUB -n %d\n" % (nproc_job))
    myfile.write("#BSUB -R \"span[ptile=40]\"\n")
    myfile.write("#BSUB -W %s\n" % (wall_time))
    myfile.write("#BSUB -e %s\n" % (err_file))
    myfile.write("#BSUB -o %s\n\n" % (out_file))

    myfile.write("module load %s\n\n" % (mpi_version))

    # **************************************************************
    # sum kernels
    # **************************************************************
    if is_sum_kern:

        myfile.write("cd %s\n\n" % (sum_kern_dir))

        myfile.write("rm -rf OUTPUT_*_KERNELS/*\n\n")

        myfile.write("date\necho '************************************'\n")
        myfile.write("echo 'sum kernels ...'\n\n")

        myfile.write("mpirun -np %d ./bin/xsum_kernels\n" % (nproc))
        myfile.write("mv OUTPUT_SUM OUTPUT_SUM_KERNELS\n")
        myfile.write("mkdir OUTPUT_SUM\n")
        myfile.write("mpirun -np %d ./bin/xsum_preconditioned_kernels\n" % (nproc))
        myfile.write("mv OUTPUT_SUM/* OUTPUT_PREC_KERNELS\n\n")

        myfile.write("echo 'sum kernels done!'\n")
        myfile.write("echo '************************************'\ndate\n\n")
        myfile.write("echo\n")
        myfile.write("echo\n\n\n\n")

    # **************************************************************
    # smooth
    # **************************************************************
    if is_smooth:
        myfile.write("cd %s\n\n" % (sum_kern_dir))
        myfile.write("date\necho '************************************'\n")
        myfile.write("echo 'smooth ...'\n\n")

        myfile.write("mpirun -np %d ./bin/xsmooth_sem %d %d alpha_kernel "
        "OUTPUT_PREC_KERNELS OUTPUT_SMOOTH_KERNELS FALSE\n" % (nproc, hsmooth, vsmooth))
        myfile.write("mpirun -np %d ./bin/xsmooth_sem %d %d beta_kernel "
        "OUTPUT_PREC_KERNELS OUTPUT_SMOOTH_KERNELS FALSE\n\n" % (nproc, hsmooth, vsmooth))

        myfile.write("echo 'gradient done!'\n")
        myfile.write("echo '************************************'\ndate\n\n")
        myfile.write("echo\n")
        myfile.write("echo\n\n\n\n")


    # **************************************************************
    # update
    # **************************************************************
    if is_update:
        myfile.write("cd %s\n\n" % (update_dir))
        myfile.write("date\necho '************************************'\n")
        myfile.write("echo 'update ...'\n\n")

        myfile.write("for step in `cat step_len.dat`;do\n")
        myfile.write("    echo $step\n")
        myfile.write("    rm -rf OUTPUT_MODEL_slen$step\n")
        myfile.write("    mkdir OUTPUT_MODEL_slen$step\n")
        myfile.write("    mpirun -np %d ./bin/xadd_model_iso $step INPUT_GRADIENT OUTPUT_MODEL_slen$step\n" % (nproc))
        myfile.write("done\n\n")

        myfile.write("echo 'update done!'\n")
        myfile.write("echo '************************************'\ndate\n\n")
        myfile.write("echo\n")
        myfile.write("echo\n\n\n\n")

os.system('bsub < %s' % (job_file))