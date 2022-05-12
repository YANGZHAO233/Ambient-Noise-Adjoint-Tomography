# mv job file to $mod, and delete line-search files to save disk spaces.

import os
from __Parameters__ import WorkDir, ProcDir
from __Parameters__ import mod, step_range, mpi_version


############################################
solver = ProcDir + '/solver'
preproc_dir = ProcDir + '/pre_proc'
meas_dir = ProcDir + '/measure_adj'
############################################
is_mv_job_file = True
is_delete_line_search_solver = True
is_delete_line_search_preproc = True
is_delete_line_search_meas = True
is_submit_jobs_delete = False               # submit jobs for deleting

##### if is_submit_jobs_delete = True ######
nproc_job = nproc
# nproc_job = 40
wall_time = '10:00'                         # limit time for one job
job_mode = 'short'                          # computing mode for submitting jobs
                                            # 'debug','short','medium','large' in SUSTECH Taiyi supercomputer
############################################
os.system('seq %s > %s/tmp.dat' % (step_range, WorkDir))
if not is_submit_jobs_delete:
    for step in open('%s/tmp.dat' % WorkDir, 'r'):
        step = step.rstrip('\n')
        newmod = mod + '_slen' + step
        print('%s is processing ...' % newmod)

        if is_delete_line_search_solver:
            temp_dir = solver + '/' + newmod
            if os.path.exists(temp_dir):
                os.system("rm -rf %s" % temp_dir)

        if is_delete_line_search_preproc:
            temp_dir = preproc_dir + '/' + newmod
            if os.path.exists(temp_dir):
                os.system("rm -rf %s" % temp_dir)

        if is_delete_line_search_meas:
            temp_dir = meas_dir + '/' + newmod
            if os.path.exists(temp_dir):
                os.system("rm -rf %s" % temp_dir)
else:
    for step in open('%s/tmp.dat' % WorkDir, 'r'):
        step = step.rstrip('\n')
        newmod = mod + '_slen' + step
        print('%s is processing ...' % newmod)

        job_file = '%s/job07_Rm_Ls_Mod_%s.sh' % (WorkDir, newmod)
        if os.path.exists(job_file):
            os.system('rm -f %s/%s' % (WorkDir, job_file))
        err_file = "job07_Rm_Ls_Mod.%s_err" % (newmod)
        out_file = "job07_Rm_Ls_Mod.%s_out" % (newmod)
        err_file_dir = WorkDir + '/' + err_file
        out_file_dir = WorkDir + '/' + out_file
        if os.path.exists(err_file_dir):
            os.system('rm -f %s' % (err_file_dir))
        if os.path.exists(out_file_dir):
            os.system('rm -f %s' % (out_file_dir))

        with open(job_file, 'w') as myfile:
            myfile.write("#!/bin/bash\n\n")

            myfile.write("#BSUB -J Rm_Ls_Mod.%s\n" % (newmod))
            myfile.write("#BSUB -q %s\n" % (job_mode))
            myfile.write("#BSUB -n %d\n" % (nproc_job))
            myfile.write("#BSUB -R \"span[ptile=40]\"\n")
            myfile.write("#BSUB -W %s\n" % (wall_time))
            myfile.write("#BSUB -e %s\n" % (err_file))
            myfile.write("#BSUB -o %s\n\n" % (out_file))

            myfile.write("module load %s\n\n" % (mpi_version))

            if is_delete_line_search_solver:
                temp_dir = solver + '/' + newmod
                if os.path.exists(temp_dir):
                    myfile.write("rm -rf %s\n" % temp_dir)

            if is_delete_line_search_preproc:
                temp_dir = preproc_dir + '/' + newmod
                if os.path.exists(temp_dir):
                    myfile.write("rm -rf %s\n" % temp_dir)

            if is_delete_line_search_meas:
                temp_dir = meas_dir + '/' + newmod
                if os.path.exists(temp_dir):
                    myfile.write("rm -rf %s\n" % temp_dir)

        os.system('bsub < %s' % (job_file))
os.system('rm -f %s/tmp.dat' % WorkDir)
############################################
job_dir = '%s/job_file/%s' % (WorkDir, mod)
if is_mv_job_file:
    if os.path.exists(job_dir):
        os.system('rm -rf %s' % job_dir)
    os.system('mkdir -p %s' % job_dir)
    os.system('mv %s/job0* %s/' % (WorkDir, job_dir))
    print("moving job file is done!")

