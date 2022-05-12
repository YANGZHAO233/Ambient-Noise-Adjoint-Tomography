# configure Data/Para_file and src_rec files
# if mod = 'M00': run this scrip; else: skip

import os
from __Parameters__ import WorkDir, ToolsDir
from __Parameters__ import mod, num_set, num_station
from __Parameters__ import nproc, nstep, dt, zone, nstep_between_output_info, sac_path


############################################
src_rec = WorkDir + '/src_rec'
sources = src_rec + '/sources.dat'
para_dir = WorkDir + '/parameter_files'
seis_process = ToolsDir + '/seis_process'
process_syn = seis_process + '/process_syn.pl'
process_data = seis_process + '/process_data.pl'
############################################

if mod == 'M00':
    # ************************************************************
    print('configure the Data/Par_file ...')
    os.system("sed -i '/NPROC                           =/c\\NPROC     "
              "                      = %s' %s" % (nproc, para_dir + '/DATA/Par_file'))

    os.system("sed -i '/NSTEP                           =/c\\NSTEP     "
              "                      = %s' %s" % (nstep, para_dir + '/DATA/Par_file'))

    os.system("sed -i '/DT                              =/c\\DT        "
              "                      = %s' %s" % (dt, para_dir + '/DATA/Par_file'))

    os.system("sed -i '/NTSTEP_BETWEEN_OUTPUT_INFO      =/c\\NTSTEP_BETWEEN_OUTPUT_INFO"
              "      = %s' %s" % (nstep_between_output_info, para_dir + '/DATA/Par_file'))

    print('configure tools/seis_process ...')
    content = open(process_syn, 'r').readlines()
    for i, line in enumerate(content):
        if "$ENV{'SACAUX'}" in line and '=' in line:
            content[i] = "$ENV{'SACAUX'}='%s';\n" % (sac_path)
            break
    with open(process_syn, 'w') as myfile:
        myfile.writelines(content)

    content = open(process_data, 'r').readlines()
    for i, line in enumerate(content):
        if "$ENV{'SACAUX'}" in line and '=' in line:
            content[i] = "$ENV{'SACAUX'}='%s';\n" % (sac_path)
            break
    with open(process_data, 'w') as myfile:
        myfile.writelines(content)

    # ************************************************************
    print('split dataset to several sets for parallel computing ...')

    lines = len(open(sources).readlines())       # number of all stations

    for i in range(1, num_set + 1):
        start_index = (i - 1) * num_station + 1  # start station in each set
        end_index = i * num_station              # end station in each set
        if end_index > lines:                    # the end index is less than or equal to the total number of stations
            end_index = lines
        print('start and end index: ', start_index, end_index)

        os.system('cat %s |sed -n "%s,%sp" > %s/sources_set%s.dat' %
                  (sources,  start_index, end_index, src_rec, i))
                                                 # save each set in sources_set file in src_rec folder

    # ************************************************************
    print('####################################')
    print('Generate cmt and force solutions ...')
    print('Create station file for each source ...')

    content = open(sources, 'r').readlines()
    for i, line in enumerate(open(sources)):
        stnm, ntwk, evla, evlo = line.split()[0:4]      # station name, network name, longitude, latitude
        evid = ntwk + '.' + stnm

        forcefile = src_rec + '/FORCESOLUTION_' + evid  # FORCE_SOLUTION file
        with open(forcefile, 'w') as myfile:
            myfile.write('FORCE  001\n')
            myfile.write('time shift:     0.0\n')
            myfile.write('f0:             1.0\n')
            myfile.write('latorUTM:       %s\n' % (evla))
            myfile.write('longorUTM:      %s\n' % (evlo))
            myfile.write('depth:          1.0\n')
            myfile.write('factor force source:             1.d15\n')
            myfile.write('component dir vect source E:     0.d0\n')
            myfile.write('component dir vect source N:     0.d0\n')
            myfile.write('component dir vect source Z_UP:  1.d0\n')

        cmtfile = src_rec + '/CMTSOLUTION_' + evid      # CMT_SOLUTION file
        with open(cmtfile, 'w') as myfile:
            myfile.write('PDE 2000 1 1 0 0 0.0 %s %s 6.4 4.2 4.2 ELIP\n' % (evla, evlo))
            myfile.write('event name:     ambient noise\n')
            myfile.write('time shift:     0.0\n')
            myfile.write('half duration:  0.0\n')
            myfile.write('latorUTM:       %s\n' % (evla))
            myfile.write('longorUTM:      %s\n' % (evlo))
            myfile.write('depth:          0.0\n')
            myfile.write('Mrr:      -0.100000e+23\n')
            myfile.write('Mtt:      -0.100000e+23\n')
            myfile.write('Mpp:       0.000000e+23\n')
            myfile.write('Mrt:      -0.000000e+23\n')
            myfile.write('Mrp:      -0.000000e+23\n')
            myfile.write('Mtp:       0.000000e+23\n')

        # Create station file for each source
        staidfile = src_rec + '/STATIONS_' + evid
        with open(staidfile, 'w') as myfile:
            for line2 in content:
                stnm0, ntwk0 = line2.split()[0:2]
                evid0 = ntwk0 + '.' + stnm0
                if evid != evid0:
                    myfile.write(line2)

        # copy one FORCESOLUTION file to DATA/, which is needed by post-processing
        if i == 1:
            os.system('cp %s %s/DATA/FORCESOLUTION' % (forcefile, para_dir))
else:
    print("if mod != 'M00': please skip this script!")