# preprocessing and measurement

import os, sys, linecache
sys.path.append("..")
from __Parameters__ import WorkDir
from __Parameters__ import mod


############################################
src_rec = WorkDir + '/src_rec'
sources = src_rec + '/sources.dat'
fwd_test = WorkDir + '/fwd_test'
preproc_dir = fwd_test +'/pre_proc/' + mod
meas_dir = fwd_test + '/measure_adj/' + mod
############################################
ik = 1      # index of shell script for preprocessing and measurement

# **************************************************************
# Run pre-processing
# **************************************************************
run_file = fwd_test + '/Pre_Mea.bash'
with open(run_file, 'w') as myfile:
    myfile.write("#!/bin/bash\n\n")
    myfile.write("cd %s\n\n" % (preproc_dir))
    myfile.write("./run %d\n\n" % (ik))

# **************************************************************
# Run measure_adj
# **************************************************************
    myfile.write("cd %s\n\n" % (meas_dir))
    myfile.write("./run %d\n\n" % (ik))

os.system("bash %s" % run_file)
os.system("rm -f %s" % run_file)
