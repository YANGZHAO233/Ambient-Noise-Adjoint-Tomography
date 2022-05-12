# check if forward computing of line-search is completed

import os
from __Parameters__ import WorkDir, ProcDir
from __Parameters__ import mod, nstep, step_range


# ============================================================
src_rec = WorkDir + '/src_rec'
sources = src_rec + '/sources.dat'
solver = ProcDir + '/solver'
# ============================================================
total_num = 0  # total number of not completed stations
os.system('seq %s > %s/tmp.dat' % (step_range, WorkDir))


for step in open('%s/tmp.dat' % WorkDir, 'r'):
    num = 0   # number of not completed stations
    step = step.rstrip('\n')
    newmod = mod + '_slen' + step
    newmod_dir = solver + '/' + newmod

    print("step%s is checking" % step)
    output_file = '%s/job05_LineSearch.%s.lst' % (WorkDir, newmod)
    os.system('cat /dev/null > %s' % output_file)

    for i, line in enumerate(open(sources, 'r')):
        stnm, ntwk = line.split()[0:2]
        evtid = ntwk + '.' + stnm
        evtid_dir = newmod_dir + '/' + evtid
        fwd_step = "%06d"%(nstep)
        fwd_file = evtid_dir + '/OUTPUT_FILES/timestamp_fwd' + str(fwd_step)
        if os.path.exists(fwd_file):
            continue
        else:
            print('	%s is not completed, please check!!' % evtid)
            os.system('echo %s >> %s' % (evtid, output_file))
            num += 1
    if num == 0:
        print('Step%s are completed!' % step)
        os.system('rm -f %s' % output_file)
    else:
        print('%s stations of Step%s are not completed!' % (num, step))

    total_num += num

if total_num == 0:
    print("Congratulations! All step is completed!")
else:
    print("Unfortunately, %s stations are not completed. Please check!!!!" % total_num)
os.system("rm -f %s/tmp.dat" % WorkDir)
