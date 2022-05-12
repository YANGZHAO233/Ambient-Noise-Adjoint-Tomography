# plot misfits between real waveforms and synthetics

import os
from __Parameters__ import WorkDir, ProcDir, ResultsDir, ScriptsDir
from __Parameters__ import mod, bands


####################################
chi_dir = ProcDir + '/misfits'
data_out = ResultsDir + '/' + mod + '/misfits_data'
image_out = ResultsDir + '/' + mod +  '/misfits_images'
####################################
plot_region = "-R-8/8/0/8000"   #coordinate axis size
if os.path.exists(data_out):
    os.system('rm -rf %s' % (data_out))
#if os.path.exists(image_out):
#    os.system('rm -rf %s' % (image_out))
####################################
bands = ' '.join(bands)
# plot_mt_cc_misfit.sh is provided by Dr Xing-li Fan
script = ScriptsDir + '/misfits_plot/plot_mt_cc_misfit.sh'
if mod == 'M00':
    os.system('bash ' + script + ' ' + chi_dir + ' ' + mod + \
              ' ' + data_out + ' ' + image_out +' "' + bands + '" ' + '"' + plot_region + '"' + ' ' + data_out + ' \n')
else:
    data_out_M00 = ResultsDir + '/M00' +  '/misfits_data'
    os.system('bash ' + script + ' ' + chi_dir + ' ' + mod + \
              ' ' + data_out + ' ' + image_out + ' "' + bands + '" ' + '"' + plot_region + '"' + ' ' + data_out_M00 + ' \n')

print('Figure saved in %s' % image_out)
