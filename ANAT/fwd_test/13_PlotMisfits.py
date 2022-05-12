# plot misfits between real waveforms and synthetics

import os, sys
sys.path.append("..")
from __Parameters__ import WorkDir, ScriptsDir
from __Parameters__ import mod, bands


####################################
fwd_test = WorkDir + '/fwd_test'
chi_dir = fwd_test + '/misfits'
data_out = fwd_test + '/misfits_data'
image_out = fwd_test +  '/misfits_images'
####################################
plot_region = "-R-8/8/0/500"   #coordinate axis size
if os.path.exists(data_out):
    os.system('rm -rf %s' % (data_out))
if os.path.exists(image_out):
   os.system('rm -rf %s' % (image_out))
####################################
bands = ' '.join(bands)
# plot_mt_cc_misfit.sh is provided by Dr Xing-li Fan
script = ScriptsDir + '/misfits_plot/plot_mt_cc_misfit.sh'
os.system('bash ' + script + ' ' + chi_dir + ' ' + mod + \
              ' ' + data_out + ' ' + image_out +' "' + bands + '" ' + '"' + plot_region + '"' + ' ' + data_out + ' \n')

