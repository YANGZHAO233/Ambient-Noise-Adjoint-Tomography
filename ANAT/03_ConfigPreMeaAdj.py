# configure preprocessing, measurement and adjoint files

import os
from glob import glob
from __Parameters__ import WorkDir, ProcDir, DataDir, ToolsDir
from __Parameters__ import mod, bands
from __Parameters__ import Vgroup, tspan, tstart, dt, nstep, df, channel, cc, tshift


############################################
mod_temp = mod                                           # current model name
src_rec = WorkDir + '/src_rec'
sources = src_rec + '/sources.dat'
solver_mod = ProcDir + '/solver/' + mod_temp
preproc_dir = ProcDir + '/pre_proc/' + mod_temp          # preprocessing directory
meas_dir = ProcDir + '/measure_adj/' + mod_temp          # measurement directory
meas_tool_dir = ToolsDir + '/ADJOINT_TOMOGRAPHY_TOOLS/measure_adj'
                                                         # measurement tools
misfit_dir = ProcDir + '/misfits/' + mod_temp            # misfit directory
############################################

# some parameters can be changed
is_preproc = True              # if preprocessing
is_meas = True                 # if measurement
is_adj = True                  # if adjoint
is_checkboard = False          # if checkboard process, not use -v in process_data.pl


# if using the group velocity calculated by user for each station-pair at each period band, set is_group_velocity = True
# group velocity is used to determine the window size of measurement
# if True, user should calculate the group velocity, and save the group velocity below the folder 'group_velocity' in ToolsDir
# if False using the fixed group velocity Vgroup in __Parameters__, which are different in different region
# I suggest that set 'is_group_velocity = True', and not use the fixed group velocity
is_group_velocity = True
group_velocity_dir = ToolsDir + '/group_velocity'

if os.path.exists(misfit_dir):
    if is_meas:
        os.system('rm -rf %s' % (misfit_dir))
        os.system('mkdir -p %s' % (misfit_dir))
else:
    os.system('mkdir -p %s' % (misfit_dir))
if os.path.exists(preproc_dir) and is_preproc:
    os.system('rm -rf %s' % (preproc_dir))
if os.path.exists(meas_dir) and is_meas:
    os.system('rm -rf %s' % (meas_dir))

# ************************************************************
for i, line in enumerate(open(sources, 'r')):
    stnm, ntwk = line.split()[0:2]
    evtid = ntwk + '.' + stnm
    istr = str(i + 1)
    print('%s is processing ...' % (evtid))

    if is_preproc:
        ############################################
        # Configure pre-processing files
        ############################################
        fwd_dir = solver_mod + '/' + evtid
        data_dir = DataDir + '/' + evtid

        evtid_dir = preproc_dir + '/' + evtid
        os.system('mkdir -p ' + evtid_dir)
        os.system('ln -sf %s/seis_process/* %s/' % (ToolsDir, preproc_dir))

        cmt_file = src_rec + '/CMTSOLUTION_' + evtid
        os.system('cp %s %s/CMTSOLUTION' % (cmt_file, evtid_dir))
        sta_file = src_rec + '/STATIONS_' + evtid
        os.system('cp %s %s/STATIONS' % (sta_file, evtid_dir))

        data_norm_dir = evtid_dir + '/DATA_NORM'
        os.system('mkdir ' + data_norm_dir)
        os.system('ln -sf %s/*.sac %s/' % (data_dir, data_norm_dir))

        ############################################
        # configure job.sh for each source
        ############################################
        job_file = preproc_dir + '/job%s.sh' % (istr)
        with open(job_file, 'w') as myfile:
            log_file = evtid + '/process.log'  # log file for processing
            myfile.write("cat /dev/null > %s\n" % (log_file))

            for item in sorted(os.listdir(data_dir)):
                prefix = item.replace('.sac', '')
                syn_file = solver_mod + '/' + evtid + '/OUTPUT_FILES/' + prefix + '.fwd.semd'
                norm_file = evtid_dir + '/DATA_NORM/' + item
                for band in bands:
                    hp = int(band[1:4])
                    lp = int(band[6:9])
                    myfile.write("perl process_syn.pl -m %s/CMTSOLUTION -a %s/STATIONS -s %f -l %s "
                                 "-t %d/%d -x %s %s >> %s\n" % (evtid, evtid, df, tspan, hp, lp, band, syn_file, log_file))
                    myfile.write("synmin=`saclst depmin f %s.sac.%s | awk '{print $2}'`\n" % (syn_file, band))
                    myfile.write("synmax=`saclst depmax f %s.sac.%s | awk '{print $2}'`\n" % (syn_file, band))
                    myfile.write("norm=`echo $synmin $synmax | awk '{if($1*$1>$2*$2) "
                                 "{print sqrt($1*$1);} else {print sqrt($2*$2)}}'`\n")
                    if is_checkboard:
                        myfile.write("perl process_data.pl -m %s/CMTSOLUTION -s %f -l %s -t %d/%d -n "
                                     "-A $norm -x %s %s >> %s\n" % (evtid, df, tspan, hp, lp, band, norm_file, log_file))
                    else:
                        myfile.write("perl process_data.pl -m %s/CMTSOLUTION -s %f -l %s -t %d/%d -v -n "
                                     "-A $norm -x %s %s >> %s\n" % (evtid, df, tspan, hp, lp, band, norm_file, log_file))

        os.system('chmod 755 ' + job_file)

        run_file = preproc_dir + '/run'
        with open(run_file, 'w') as myfile:
            myfile.write("id=$1\n")
            myfile.write("./job$id.sh\n")
        os.system('chmod 755 ' + run_file)

    if is_meas:
        ############################################
        # Configure measurement files
        ############################################
        evtid_dir = meas_dir + '/' + evtid
        os.system('mkdir -p ' + evtid_dir)

        cmt_file = src_rec + '/CMTSOLUTION_' + evtid
        os.system('cp %s %s/' % (cmt_file, evtid_dir))

        # adjoint tomography tools
        os.system('cp %s/measure_adj %s/' % (meas_tool_dir, evtid_dir))
        os.system('cp %s/PAR_FILE %s/' % (meas_tool_dir, evtid_dir))
        os.system('cp %s/scripts_tomo/prepare_measure_adj.pl %s/' % (meas_tool_dir, evtid_dir))
        os.system('cp %s/scripts_tomo/combine_2_adj_src.pl %s/' % (meas_tool_dir, evtid_dir))
        os.system('cp %s/scripts_tomo/combine_3_adj_src.pl %s/' % (meas_tool_dir, evtid_dir))
        os.system('cp %s/scripts_tomo/run_measure_adj.pl %s/' % (meas_tool_dir, evtid_dir))
        os.system('cp %s/*.pl %s/' % (meas_tool_dir, evtid_dir))
        os.system('cp %s/rotate_adj_src %s/' % (meas_tool_dir, evtid_dir))
        os.system('cp -r %s/UTIL %s/' % (meas_tool_dir, evtid_dir))

        os.system('mkdir %s/OUTPUT_FILES' % (evtid_dir))
        os.system('mkdir -p %s/PLOTS/RECON' % (evtid_dir))

        sta_file = src_rec + '/STATIONS_' + evtid
        os.system('cat %s | wc -l > %s/PLOTS/STATIONS_TOMO' % (sta_file, evtid_dir))
        os.system('cat %s >> %s/PLOTS/STATIONS_TOMO' % (sta_file, evtid_dir))
        os.system('cp %s/PLOTS/STATIONS_TOMO %s/STATIONS_ADJOINT' % (evtid_dir, evtid_dir))

        DATA_dir = evtid_dir + '/DATA'
        data_norm_dir = preproc_dir + '/' + evtid + '/DATA_NORM'
        fwd_dir = solver_mod + '/' + evtid
        os.system('ln -s %s %s' % (data_norm_dir, DATA_dir))
        os.system('ln -s %s/OUTPUT_FILES %s/SYN' % (fwd_dir, evtid_dir))

        job_file = meas_dir + '/job%s.sh' % (istr)
        with open(job_file, 'w') as myfile:
            myfile.write("cd %s\n" % (evtid))
            myfile.write("cat /dev/null > run.log\n")

            for i, band in enumerate(bands):
                hp = int(band[1:4])
                lp = int(band[6:9])

                mea_win_file = evtid_dir + '/MEASUREMENT.WINDOWS.' + band
                os.system('ls %s/*.sac | wc -l > %s' % (DATA_dir, mea_win_file))

                for rawsac in sorted(glob(DATA_dir + '/*.sac')):
                    item = os.path.basename(rawsac)
                    prefix = item.replace('.sac', '')

                    p = os.popen("saclst dist f %s | awk '{print $2}'" % (rawsac))
                    dist = p.read().rstrip('\n')  # distance between source and receiver
                    p.close()

                    # Determining minimum and maximum group velocities for the signal window
                    if is_group_velocity:
                        group_velocity_dir_band = group_velocity_dir + '/' + band
                                                # directory for each period band
                        group_velocity_receiver = prefix.replace('.' + channel + 'Z', '')
                                                # receiver name
                        group_velocity_source = group_velocity_dir_band + '/' + evtid
                                                # group velocity for one source file
                        if os.path.exists(group_velocity_source):
                            flag = False
                            for pred_line in open(group_velocity_source, 'r'):
                                if group_velocity_receiver in pred_line:
                                    vmin, vmax = map(float, pred_line.split()[1:])
                                    flag = True
                                    break
                            if not flag: # can't find source-receiver pair in source group velocity file
                                print('Warning: ' + evtid + ' and ' + group_velocity_receiver + ' not found in the group velocity files!')
                                print('instead of using default group velocity')
                                vmin = Vgroup[i][0]
                                vmax = Vgroup[i][1]
                        else: # group velocity file dosen't exist
                            print('Warning: No group velocity files! Defaults are used.')
                            vmin = Vgroup[i][0]
                            vmax = Vgroup[i][1]
                    else: # use default group velocity for all station pairs
                        vmin = Vgroup[i][0]
                        vmax = Vgroup[i][1]

                    t1 = float(dist) / vmax - (lp + hp) * 0.5
                    t2 = float(dist) / vmin + (lp + hp) * 0.5

                    datsac = 'DATA/' + item + '.' + band
                    synsac = 'SYN/' + prefix + '.fwd.semd.sac.' + band

                    os.system('echo %s >> %s' % (datsac, mea_win_file))
                    os.system('echo %s >> %s' % (synsac, mea_win_file))
                    os.system('echo 1 >> %s' % (mea_win_file))
                    os.system('echo %.4f %.4f >> %s' % (t1, t2, mea_win_file))

                myfile.write("cp MEASUREMENT.WINDOWS.%s MEASUREMENT.WINDOWS\n" % (band))

                if band == bands[0]:
                    myfile.write(
                        "perl run_measure_adj.pl %s %s 0 0 0 %.1f/%f/%d 7 %s %d/%d 0/0/0/1 %.2f/%.2f/-1.0/1.0/%.2f 1/1.0/0.5 "
                        "1/0.02/2.5/2.0/2.5/3.5/1.5 >> run.log\n" % (
                        mod_temp, tspan, tstart, dt, nstep, channel, hp, lp, -tshift[0], tshift[0], cc[0]))
                if band == bands[1]:
                    myfile.write(
                        "perl run_measure_adj.pl %s %s 0 0 0 %.1f/%f/%d 7 %s %d/%d 0/0/0/1 %.2f/%.2f/-1.0/1.0/%.2f 1/1.0/0.5 "
                        "1/0.02/2.5/2.0/2.5/3.5/1.5 >> run.log\n" % (
                        mod_temp, tspan, tstart, dt, nstep, channel, hp, lp, -tshift[1], tshift[1], cc[1]))
                if band == bands[2]:
                    myfile.write(
                        "perl run_measure_adj.pl %s %s 0 0 0 %.1f/%f/%d 7 %s %d/%d 0/0/0/1 %.2f/%.2f/-1.0/1.0/%.2f 1/1.0/0.5 "
                        "1/0.02/2.5/2.0/2.5/3.5/1.5 >> run.log\n" % (
                        mod_temp, tspan, tstart, dt, nstep, channel, hp, lp, -tshift[2], tshift[2], cc[2]))

                myfile.write("mv ADJOINT_SOURCES ADJOINT_SOURCES_%s\n" % (band))
                myfile.write("mv window_chi %s/%s_%s_%s_window_chi\n" % (misfit_dir, mod_temp, band, evtid))

            if is_adj:
                myfile.write("perl combine_3_adj_src.pl %s ADJOINT_SOURCES_%s ADJOINT_SOURCES_%s "
                             "ADJOINT_SOURCES_%s ADJOINT_SOURCES iker07 iker07 iker07 >> run.log\n"
                             % (channel, bands[0], bands[1], bands[2]))
                myfile.write("cd %s/%s\n" % (solver_mod, evtid))
                myfile.write("rm -rf SEM\n")
                myfile.write("mkdir -p SEM\n")
                myfile.write("cd SEM\n")
                myfile.write("for meas_adj in `ls %s/%s/ADJOINT_SOURCES/*.adj`;do\n" % (meas_dir, evtid))
                myfile.write("    adj=`echo $meas_adj | awk -F%s/%s/ADJOINT_SOURCES/ '{print $2}'`\n" % (meas_dir, evtid))
                myfile.write("    stnm=`echo $adj | awk -F. '{print $1}'`\n")
                myfile.write("    net=`echo $adj | awk -F. '{print $2}'`\n")
                myfile.write("    ch=`echo $adj | awk -F. '{print $3}'`\n")
                myfile.write("    adj_new=$net.$stnm.$ch.adj\n")
                myfile.write("    mv $meas_adj $adj_new\n")
                myfile.write(
                    "    cat %s/%s/ADJOINT_SOURCES/STATIONS_ADJOINT | sed -n '2,$p' > ../DATA/STATIONS_ADJOINT\n" % (meas_dir, evtid))
                myfile.write("done\n")
                myfile.write("cd %s\n" % (ProcDir))

        os.system('chmod 755 ' + job_file)

        runfile = meas_dir + '/run'
        with open(runfile, 'w') as myfile:
            myfile.write("id=$1\n")
            myfile.write("./job$id.sh\n")
        os.system('chmod 755 ' + runfile)
