# rename new vp, vs, rho in optimizer/update_models

import os
from __Parameters__ import WorkDir, ProcDir
from __Parameters__ import mod, step_range


############################################
optimize_dir = ProcDir + '/optimize/' + mod
update_dir = optimize_dir + '/update_models'
############################################
os.system('seq %s > %s/tmp.dat' % (step_range, WorkDir))

for step in open('%s/tmp.dat' % WorkDir, 'r'):
    step = step.rstrip('\n')

    # Change the name of new vp, vs and rho files
    tmpdir = update_dir + '/OUTPUT_MODEL_slen' + step
    if os.path.exists(tmpdir + '/proc000000_vs_new.bin'):
        os.system("rename vp_new.bin vp.bin %s/*vp_new.bin" % (tmpdir))
        os.system("rename vs_new.bin vs.bin %s/*vs_new.bin" % (tmpdir))
        os.system("rename rho_new.bin rho.bin %s/*rho_new.bin" % (tmpdir))
    else:
        print("new vs vp and rho dont't exist")

os.system('rm -rf %s/tmp.dat' % WorkDir)