# plot data and synthetic waveforms

import os, sys, linecache
sys.path.append("..")
from __Parameters__ import WorkDir, ScriptsDir
from __Parameters__ import mod, bands


# ============================================================
seismo_plot = ScriptsDir + '/seismo_plot'
fwd_test = WorkDir + '/fwd_test'
data_syn_images = fwd_test + '/data_syn_images'
src_rec = WorkDir + '/src_rec'
sources = src_rec + '/sources.dat'
meas_dir = fwd_test + '/measure_adj'
misfit_dir = fwd_test + '/misfits/' + mod
# ============================================================
is_link_syn_M00 = True                          # link synthetics of M00 to current model in measurement directory
bands = ' '.join(bands)
# ============================================================
the_line = linecache.getline(sources, 1)        # get the line of first station
stnm, ntwk, evla, evlo = the_line.split()[0:4]  # station name, network name, longitude, latitude
evtid = ntwk + '.' + stnm
evtid_dir = fwd_test + '/' + evtid              # station directory

if is_link_syn_M00:
    for i, line in enumerate(open(sources, 'r')):
        target_dir = meas_dir + '/' + mod + '/' + evtid
        if os.path.exists('%s/SYN_M00' % target_dir):
            os.system('rm -rf %s/SYN_M00' % target_dir)
        os.system('ln -sf %s/OUTPUT_FILES %s/SYN_M00' % (evtid_dir, target_dir))

if os.path.exists(data_syn_images):
    os.system('rm -rf %s' % data_syn_images)


os.system('bash %s/plot_data_syn.sh '  % seismo_plot + meas_dir + ' ' + mod + ' ' \
             + evtid + ' ' + misfit_dir + ' ' + data_syn_images + ' "'+bands+'"\n')

