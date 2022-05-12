#!/usr/bin/env python3

# **************************************************************************
import os
import numpy as np
from datetime import date
from obspy import UTCDateTime
from obspy.clients.fdsn import Client
# **************************************************************************

# **************************************************************************
def _mkdir(newdir):
    """ works the way a good mkdir should
         - already exists, silently complete
         - regular file in the way, raise an exception
         - parent directory(ies) does not exist, make them as well
    """
    if os.path.isdir(newdir):
        pass
    elif os.path.isfile(newdir):
        raise OSError("a file with the same name as the desired " \
                      "dir, '%s', already exists." % newdir)
    else:
        head, tail = os.path.split(newdir)
        if head and not os.path.isdir(head):
            _mkdir(head)
        if tail:
            os.mkdir(newdir)
# **************************************************************************


# **************************************************************************
def is_str_in_file(filepath, mystr):
    """ Examine if a file contains a specific string
    """
    for line in open(filepath, 'r'):
        if mystr in line:
            return True
    return False
# **************************************************************************


# **************************************************************************
def del_emptyline_file(filepath):
    """ Delete all the empty lines ('\n') in a file
    """
    with open(filepath, 'r') as myfile:
        data = myfile.readlines()
        while '\n' in data:
            data.remove('\n')
    with open(filepath, 'w') as myfile:
        myfile.writelines(data)
# **************************************************************************


def GetPZ2(infile, outfile):
    """ Convert a RESP file or full PZ file to a concise PZ file
    """

    if is_str_in_file(infile, 'Stage sequence number'):
        check_velocity = False
        sen_flag = False
        zero_done = False
        pole_done = False
        ncount1 = 0
        ncount2 = 0
        respfile = infile

        fpz = open(outfile, 'w')

        for line in open(respfile):
            if 'Network:' in line:
                network = line.split()[2]
                fpz.write('* **********************************\n')
                fpz.write('* NETWORK   (KNETWK): '+network+'\n')
                fpz.write('* STATION    (KSTNM): '+station+'\n')
                continue

            if 'Station:' in line:
                station = line.split()[2]
                continue

            if 'Location:' in line:
                if len(line.split()) > 2:
                    location = line.split()[2]
                else:
                    location = ''
                fpz.write('* LOCATION   (KHOLE): '+location+'\n')
                continue

            if 'Channel:' in line:
                channel = line.split()[2]
                fpz.write('* CHANNEL   (KCMPNM): '+channel+'\n')
                continue

            if 'Start date:' in line:
                syear = line.split()[3].split(',')[0]
                sjday = line.split()[3].split(',')[1]
                sdate = date.fromordinal(date(int(syear),1,1).toordinal()+int(sjday)-1)
                sdate = sdate.isoformat()
                if len(line.split()[3].split(',')) > 2:
                    stime = line.split()[3].split(',')[2].split('.')[0]
                    if len(stime) == 5:
                        stime = stime+':00'
                    if len(stime) == 2:
                        stime = stime+':00:00'
                else:
                    stime = '00:00:00'
                sdatetime = sdate+'T'+stime
                fpz.write('* START             : '+sdatetime+'\n')
                continue

            if 'End date:' in line:
                tmp = line.split()[3]
                if tmp != 'No':
                    eyear = tmp.split(',')[0]
                    ejday = tmp.split(',')[1]
                    edate = date.fromordinal(date(int(eyear),1,1).toordinal()+int(ejday)-1)
                    edate = edate.isoformat()
                    if len(line.split()[3].split(',')) > 2:
                        etime = tmp.split(',')[2].split('.')[0]
                        if len(etime) == 5:
                            etime = etime+':00'
                        if len(etime) == 2:
                            etime = etime+':00:00'
                    else:
                        etime = "23:59:59"
                    edatetime = edate+'T'+etime
                else:
                    edatetime = '2599-12-31T23:59:59'
                fpz.write('* END               : '+edatetime+'\n')
                continue

            if 'Stage sequence number:' in line and line.split()[4] == '0':
                sen_flag = True
                continue

            if 'Sensitivity:' in line and sen_flag:
                sensitivity = line.split()[2]
                sen_flag = False
                fpz.write('* **********************************\n')
                fpz.write('ZEROS	'+str(nzeros+1)+'\n')
                fpz.write('	0.000000E+00	 0.000000E+00\n')

                for x, y in zerodata:
                    fpz.write('%16s %16s\n' % (x, y))

                fpz.write('POLES	'+str(npoles)+'\n')
                for x, y in poledata:
                    fpz.write('%16s %16s\n' % (x, y))

                const = '%E' % (float(A0)*float(sensitivity))
                fpz.write('CONSTANT	'+const+'\n\n\n')
                continue

            if 'A0 normalization factor:' in line:
                A0 = line.split()[4]
                continue

            if 'Response in units lookup:' in line and line.split()[5] == 'M/S':
                check_velocity = True
                continue

            if 'Number of zeroes:' in line:
                nzeros = int(line.split()[4])
                zero_done = False
                continue

            if 'i' in line and 'real' in line and 'imag' in line and not zero_done:
                ncount1 = nzeros
                zerodata = []
                zero_done = True
                continue

            if ncount1 > 0:
                zerodata.append((line.split()[2], line.split()[3]))
                ncount1 = ncount1 - 1
                continue

            if 'Number of poles:' in line:
                npoles = int(line.split()[4])
                pole_done = False
                continue

            if 'i' in line and 'real' in line and 'imag' in line and not pole_done:
                ncount2 = npoles
                poledata = []
                pole_done = True
                continue

            if ncount2 > 0:
                poledata.append((line.split()[2], line.split()[3]))
                ncount2 = ncount2 - 1
                continue

        fpz.close()

        if not check_velocity:
            print('Error: '+respfile+' does not take velocity as input!')
            os.system('rm -rf '+outfile)
    else:
        os.system("sed '/CREATED/d' %s | sed '/DESCRIPTION/d' | sed '/LATITUDE/d' | sed '/LONGITUDE/d' \
        | sed '/ELEVATION/d' | sed '/DEPTH/d' | sed '/DIP/d' | sed '/AZIMUTH/d' | sed '/SAMPLE RATE/d' \
        | sed '/INPUT UNIT/d' | sed '/OUTPUT UNIT/d' | sed '/INSTTYPE/d' | sed '/INSTGAIN/d' \
        | sed '/COMMENT/d' | sed '/SENSITIVITY/d' | sed '/* A0/d' > %s" %(infile, outfile))



def GetPZ(indir, outdir):
    """ Convert RESP or PZ files under a directory to standard PZ files stored
    in another directory
    """

    os.system('rm -rf '+outdir)
    os.system('mkdir -p '+outdir)

    for eachfile in os.listdir(indir):
        print(eachfile+' is processsing...')

        filepath = indir+'/'+eachfile
        if is_str_in_file(filepath, 'Stage sequence number'):
            check_velocity = False
            sen_flag = False
            zero_done = False
            pole_done = False
            ncount1 = 0
            ncount2 = 0
            respfile = filepath

            for line in open(respfile):
                if 'Network:' in line:
                    network = line.split()[2]
                    continue

                if 'Station:' in line:
                    station = line.split()[2]
                    continue

                if 'Location:' in line:
                    if len(line.split()) > 2:
                        location = line.split()[2]
                    else:
                        location = ''
                    continue

                if 'Channel:' in line:
                    channel = line.split()[2]
                    pzfile = outdir+'/'+network+'.'+station+'.'+location+'.'+channel+'.PZ'
                    fpz = open(pzfile, 'a')
                    fpz.write('* **********************************\n')
                    fpz.write('* NETWORK   (KNETWK): '+network+'\n')
                    fpz.write('* STATION    (KSTNM): '+station+'\n')
                    fpz.write('* LOCATION   (KHOLE): '+location+'\n')
                    fpz.write('* CHANNEL   (KCMPNM): '+channel+'\n')
                    continue

                if 'Start date:' in line:
                    syear = line.split()[3].split(',')[0]
                    sjday = line.split()[3].split(',')[1]
                    sdate = date.fromordinal(date(int(syear),1,1).toordinal()+int(sjday)-1)
                    sdate = sdate.isoformat()
                    if len(line.split()[3].split(',')) > 2:
                        stime = line.split()[3].split(',')[2].split('.')[0]
                        if len(stime) == 5:
                            stime = stime+':00'
                        if len(stime) == 2:
                            stime = stime+':00:00'
                    else:
                        stime = '00:00:00'
                    sdatetime = sdate+'T'+stime
                    fpz.write('* START             : '+sdatetime+'\n')
                    continue

                if 'End date:' in line:
                    tmp = line.split()[3]
                    if tmp != 'No':
                        eyear = tmp.split(',')[0]
                        ejday = tmp.split(',')[1]
                        edate = date.fromordinal(date(int(eyear),1,1).toordinal()+int(ejday)-1)
                        edate = edate.isoformat()
                        if len(line.split()[3].split(',')) > 2:
                            etime = tmp.split(',')[2].split('.')[0]
                            if len(etime) == 5:
                                etime = etime+':00'
                            if len(etime) == 2:
                                etime = etime+':00:00'
                        else:
                            etime = "23:59:59"
                        edatetime = edate+'T'+etime
                    else:
                        edatetime = '2599-12-31T23:59:59'
                    fpz.write('* END               : '+edatetime+'\n')
                    continue

                if 'Stage sequence number:' in line and line.split()[4] == '0':
                    sen_flag = True
                    continue

                if 'Sensitivity:' in line and sen_flag:
                    sensitivity = line.split()[2]
                    sen_flag = False
                    fpz.write('* **********************************\n')
                    fpz.write('ZEROS	'+str(nzeros+1)+'\n')
                    fpz.write('	0.000000E+00	 0.000000E+00\n')

                    for x, y in zerodata:
                        fpz.write('%16s %16s\n' % (x, y))

                    fpz.write('POLES	'+str(npoles)+'\n')
                    for x, y in poledata:
                        fpz.write('%16s %16s\n' % (x, y))

                    const = '%E' % (float(A0)*float(sensitivity))
                    fpz.write('CONSTANT	'+const+'\n\n\n')
                    fpz.close()
                    continue

                if 'A0 normalization factor:' in line:
                    A0 = line.split()[4]
                    continue

                if 'Response in units lookup:' in line and line.split()[5] == 'M/S':
                    check_velocity = True
                    continue

                if 'Number of zeroes:' in line:
                    nzeros = int(line.split()[4])
                    zero_done = False
                    continue

                if 'i' in line and 'real' in line and 'imag' in line and not zero_done:
                    ncount1 = nzeros
                    zerodata = []
                    zero_done = True
                    continue

                if ncount1 > 0:
                    zerodata.append((line.split()[2], line.split()[3]))
                    ncount1 = ncount1 - 1
                    continue

                if 'Number of poles:' in line:
                    npoles = int(line.split()[4])
                    pole_done = False
                    continue

                if 'i' in line and 'real' in line and 'imag' in line and not pole_done:
                    ncount2 = npoles
                    poledata = []
                    pole_done = True
                    continue

                if ncount2 > 0:
                    poledata.append((line.split()[2], line.split()[3]))
                    ncount2 = ncount2 - 1
                    continue

            if not check_velocity:
                print('Error: '+respfile+' does not take velocity as input!')
                os.system('rm -rf '+pzfile)

        else:
            content = []

            for line in open(filepath, 'r'):

                if line != '\n':
                    content.append(line)

                if 'NETWORK' in line:
                    network = line.split(':')[1].rstrip('\n').strip()
                    continue

                if 'STATION' in line:
                    station = line.split(':')[1].rstrip('\n').strip()
                    continue

                if 'LOCATION' in line:
                    location = line.split(':')[1].rstrip('\n').strip()
                    continue

                if 'CHANNEL' in line:
                    channel = line.split(':')[1].rstrip('\n').strip()
                    continue

                if 'CONSTANT' in line:
                    content.append('\n\n')
                    pzfile = outdir+'/'+network+'.'+station+'.'+location+'.'+channel+'.PZ'
                    with open(pzfile, 'a') as myfile:
                        for tmpline in content:
                            myfile.write(tmpline)
                    content = []
                    continue


def find_bad_curves(inlistfile, outprefix, itype, threshold, ratio, lowerP, upperP, lowerV, upperV, skiprows, usecols):
    '''itype: Choose which type (1: mean value, 2: standard deviation) to find outliers (e.g. 2)
    threshold: Threshold value (percentage (e.g. 10) for mean type and times of deviation (e.g. 3) for std type)
    ratio: Threshold ratio value in percentage (e.g. 60) for the maximum allowed bad data points
    lowerP, upperP: Period band to investigate if the velocity are out of the bounds set by (lowerV, upperV)'''

    selfile = outprefix+'_sel.lst'
    badfile = outprefix+'_bad.lst'
    epsilon = 0.00001

    #####################################################
    # Calculate the mean and standard deviation values
    #####################################################
    os.system('rm -rf data.junk')
    for filepath in open(inlistfile, 'r'):
        filepath = filepath.rstrip('\n')
        print('Reading: '+filepath)
        data = np.loadtxt(filepath, skiprows=skiprows, usecols=usecols, ndmin=2)
        np.savetxt('tmp.junk', data)
        os.system('cat tmp.junk >> data.junk')

    data = np.loadtxt('data.junk')
    raw_periods = data[:,0]
    raw_velocities = data[:,1]
    periods = np.unique(raw_periods)

    means = np.zeros(shape=periods.shape)
    stds = np.zeros(shape=periods.shape)

    for i, period in enumerate(periods):
        select_velocities = raw_velocities[np.where(abs(raw_periods-period)<epsilon)]
        means[i] = select_velocities.mean()
        stds[i] = select_velocities.std()

    #####################################################
    # Do the rejection
    #####################################################
    file1 = open(selfile, 'w')
    file2 = open(badfile, 'w')

    for filepath in open(inlistfile, 'r'):
        filepath = filepath.rstrip('\n')
        data = np.loadtxt(filepath, skiprows=skiprows, usecols=usecols, ndmin=2)

        xrows = data.shape[0]
        maxpoints = max(1, round(xrows*ratio/100))

        flag = True
        badpoints = 0
        for raw_period, raw_velocity in data:
            if raw_velocity < lowerV and raw_period > lowerP:
                 flag = False
                 errmsg = 'Too low velocity!'
                 break

            if raw_velocity > upperV and raw_period < upperP:
                 flag = False
                 errmsg = 'Too High velocity!'
                 break

            if itype == 1:
                mean = means[np.where(abs(periods-raw_period)<epsilon)]
                diff = abs(raw_velocity-mean)/mean*100

                if diff > threshold:
                    badpoints += 1
                    if badpoints > maxpoints:
                        flag = False
                        errmsg = "%.1f%% points (%d) exceed %.1f%% deviation from the mean" %(ratio, badpoints, threshold)
                        break

            if itype == 2:
                mean = means[np.where(abs(periods-raw_period)<epsilon)]
                std = stds[np.where(abs(periods-raw_period)<epsilon)]
                diff = abs(raw_velocity-mean)

                if diff > threshold*std:
                    badpoints += 1
                    if badpoints > maxpoints:
                        flag = False
                        errmsg = "%.1f%% points (%d) exceed %.1f*std deviation from the mean" %(ratio, badpoints, threshold)
                        break

        if flag:
            file1.write(filepath+'\n')
        else:
            print(filepath.split('/')[-1]+'\t\t'+errmsg)
            file2.write(filepath+'\t\t'+errmsg+'\n')

    #########################
    file1.close()
    file2.close()
    #########################
    os.system('rm -rf tmp.junk data.junk')

def DataList(srcdir):
    """
    ==================================================================================================
    ==================================================================================================
    This function removes empty folder(s) and file(s), and lists all daily folder names consisting of
    the sac files for each particular station which are written to [network.station.lst] file.
    ==================================================================================================
    Required programs or packages:
    ==================================================================================================
    """

    ###################################
    folder = 'DataList'

    ###################################
    os.system('rm -rf '+folder)
    os.system('mkdir '+folder)

    ###################################
    # Remove empty file(s) and folder(s)
    ###################################
    os.system('find %s -name "*" -type f -size 0c | xargs -n 1 rm -f' % (srcdir))
    os.system('find %s -depth -type "d" -empty -exec rmdir {} \;' %(srcdir))

    ###################################
    for year in sorted(os.listdir(srcdir)):
        for month in sorted(os.listdir(srcdir+'/'+year)):
            for day in sorted(os.listdir(srcdir+'/'+year+'/'+month)):
                print(day+' is processing...')
                for sacfile in sorted(os.listdir(srcdir+'/'+year+'/'+month+'/'+day)):
                    net, sta = sacfile.split('.')[0:2]
                    os.system('echo %s >> %s' % (day, folder+'/'+net+'.'+sta+'.lst'))

def HeaderCheck(srcdir, outfile):
    """
    ==================================================================================================
    ==================================================================================================
    This function checks the sampling rate of all sac files.
    ==================================================================================================
    Required programs or packages:
      saclst
    ==================================================================================================
    """

    ###################################
    os.system('rm -rf junk.lst')

    ###################################
    for year in sorted(os.listdir(srcdir)):
        for month in sorted(os.listdir(srcdir+'/'+year)):
            for day in sorted(os.listdir(srcdir+'/'+year+'/'+month)):
                print(day+' is processing...')
                tmp = srcdir+'/'+year+'/'+month+'/'+day+'/*'
                os.system('saclst knetwk kstnm stlo stla delta kcmpnm f %s >> junk.lst' % (tmp))

    ###################################
    os.system("gawk '{printf \"%s %6s %10.4f %10.4f %8.4f %6s\\n\",$2,$3,$4,$5,$6,$7}' junk.lst | sort | uniq > "+outfile)

    ###################################
    os.system('rm -rf junk.lst')


def getPZandSTAinfo(net, stalist, loc, chan, startTime, endTime, clientName):
    """
    ==================================================================================================
    ==================================================================================================
    This script utilizes client.get_stations function in obspy to retrive station information
    (net, sta, lon, lat, height and timespan) and SAC PZ files.
    ==================================================================================================
    Note: The sac pz files produced by original sacpz.py in obspy can not be directly used in sac to
    remove instrument response due to time format error (sacpz.py gives UTC standard time format). The
    original sacpz.py in obsPy has been modified to generate correct sac pz files.
    ==================================================================================================
    """

    ###################################
    client = Client(clientName)
    start_time = UTCDateTime(startTime)
    end_time = UTCDateTime(endTime)

    ###################################
    stafile = net+'.sta.info'
    pzfile = net+'.pz.info'
    os.system('rm -rf '+stafile)
    os.system('rm -rf '+pzfile)

    with open(stafile, 'a') as fid:
        for sta in stalist:
            inventory = client.get_stations(network=net, station=sta, level='station')
            for stainfo in inventory[0]:
                fid.write('%4s  %6s %10.4f %10.4f %8d    %s    %s\n' % (net, stainfo.code, stainfo.longitude, \
                stainfo.latitude, stainfo.elevation, stainfo.creation_date, stainfo.termination_date))

            inventory = client.get_stations(starttime=start_time, endtime=end_time, network=net,\
             station=sta, location=loc, channel=chan, level='response')
            inventory.write('pz.tmp', format='SACPZ')
            os.system('cat pz.tmp >>'+pzfile)
            os.system('rm -rf pz.tmp')

    ###########################################################
    # Combine the records for one station at different time
    # periods together (with the same net, sta, lon, lat, ele).
    ###########################################################
    content = []
    for line in open(stafile, 'r'):
        net, sta, lon, lat, ele = line.split()[0:5]
        strname = '%4s  %6s %10.4f %10.4f %8d'%(net, sta, float(lon), float(lat), float(ele))
        content.append(strname)
    content = sorted(set(content))
    os.system('cp '+stafile+' junk.txt')
    with open(stafile,'w') as myfile:
        for line in content:
            starttime = UTCDateTime('2599-12-31')
            endtime = UTCDateTime('1900-01-01')
            for tmpline in open('junk.txt', 'r'):
                if line in tmpline:
                    startT = UTCDateTime(tmpline.split()[5])
                    if tmpline.split()[6] == "None":
                        endT = UTCDateTime('2599-12-31')
                    else:
                        endT = UTCDateTime(tmpline.split()[6])
                    if startT < starttime:
                        starttime = startT
                    if endT > endtime:
                        endtime = endT
            myfile.write('%s    %s    %s\n'%(line, starttime.date, endtime.date))
    os.system('rm -rf junk.txt')
