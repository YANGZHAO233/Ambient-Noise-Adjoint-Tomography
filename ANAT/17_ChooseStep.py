#  Choose step by computing tr-chi

import os
from __Parameters__ import ProcDir, ResultsDir, ScriptsDir
from __Parameters__ import mod, bands
import pandas as pd
import matplotlib.pyplot as plt

####################################
misfit_dir = ProcDir + '/misfits'
bands = ' '.join(bands)
outdir = ResultsDir + '/' + mod + '/tr_chi'
outfile = outdir + '/tr_chi.dat'
shell_dir = ScriptsDir + '/misfits_plot'
is_plot = True
            # if is_plot: please install pandas and matplotlib
            # e.g. 'pip install pandas, matplotlib'
####################################
os.system('mkdir -p ' + misfit_dir + '/temp')
os.system('mv %s* %s/' % (misfit_dir + '/' + mod, misfit_dir + '/temp'))
os.system('mv %s %s' % (misfit_dir + '/temp', misfit_dir + '/' + mod))
indir = misfit_dir + '/' + mod

if os.path.exists(outdir):
    os.system('rm -rf %s' % outdir)
os.system('mkdir -p ' + outdir)

# ####################################
os.system('bash %s/compute_tr_chi.sh %s "%s" %s %s' % (shell_dir, indir, bands, mod, outfile))

os.system('mv %s %s/' % (misfit_dir + '/' + mod, misfit_dir + '/temp'))
os.system('mv %s/* %s/' % (misfit_dir + '/temp', misfit_dir))
os.system('rm -rf %s' % (misfit_dir + '/temp'))

####################################
if is_plot:
    print('Plotting ...')
    df = pd.read_csv(outfile, sep=' ', header=0) # pandas dataframe
    columns = df.columns.values                  # bands and average
    savepath = outdir + '/tr_chi.png'

    fig = plt.figure()
    plt.rc('font', size=10)                      # global font size
    plt.title('Tr_chi Misfits over Steps', fontsize=15)

    color_map = {
        columns[1]:'green',
        columns[2]:'blue',
        columns[3]:'orange',
        columns[4]:'red'
    }

    for i in columns[1:]:
        plt.plot(df['TABLE'], df[i], color=color_map[i], label=i, marker='o', linestyle='-', alpha=0.5)

        for x, y in zip(df['TABLE'], df[i]):
            plt.text(
                x=x,
                y=y + 0.01,
                s='{:.2f}'.format(y),
                color=color_map[i])

    plt.legend()
    plt.xlabel('Step Range', fontsize=15)
    plt.ylabel('Tr_chi Misfits / s', fontsize=15)
    # plt.show()
    fig.savefig(savepath)
    print('Figure saved in %s' % outdir)