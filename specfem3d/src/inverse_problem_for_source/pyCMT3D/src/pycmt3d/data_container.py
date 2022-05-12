#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Methods that contains utils for adjoint sources

:copyright:
    Wenjie Lei (lei@princeton.edu), 2016
:license:
    GNU Lesser General Public License, version 3 (LGPLv3)
    (http://www.gnu.org/licenses/lgpl-3.0.en.html)
"""
from __future__ import (print_function, division, absolute_import)
import time
import os
from collections import defaultdict
import json
import numpy as np
from obspy import read
from . import logger
from collections import Sequence

from .constant import PARLIST
try:
    from pyasdf import ASDFDataSet
except ImportError:
    print("Can not import pyasdf. ASDF not supported then")


class MetaInfo(object):
    """
    Meta information associated with TraceWindow.
    The idea of class MetaInfo is to keep TraceWindow only contain
    raw traces and window information. All measurments will be kept
    in MetaInfo
    """

    def __init__(self, obsd_id=None, synt_id=None, weights=None,
                 Aws=None, bws=None, Aes=None, bes=None, prov=None):
        """
        :param obsd_id: observed trace id
        :param synt_id: synthetic trace id
        :param weights: window weights information
        :param Aws: matrix A of each window based on waveform misfit
        :param bws: vector b of each window based on waveform misfit
        :param Aes: matrix A of each window based on envelope misfit
        :param bes: vector b of each window based on envelope misfit
        :param prov: provenance(and other information) associated with
            traces and windows
        :return:
        """
        self.obsd_id = obsd_id
        self.synt_id = synt_id
        self.weights = weights
        self.Aws = Aws
        self.bws = bws
        self.Aes = Aes
        self.bes = bes

        # dictionary to store various measurements
        self.prov = prov

    def __repr__(self):
        string = self.id
        string += " -- weights: %s" % self.weights
        string += " -- provenance: %s" % self.prov
        return string

    @property
    def id(self):
        return "MetaInfo [ obsd: %s -- synt: %s]" % (
            self.obsd_id, self.synt_id)


class TraceWindow(object):
    """
    One obsd trace, one synt trace, deriv synt traces and window information
    from one component of one station. Also, station location and event
    location information should be provided(to calculate azimuth,
    epicenter distance and etc. init_weight is the window initial weight
    read from the window file.
    """

    def __init__(self, datalist=None, windows=None, init_weight=None,
                 longitude=None, latitude=None,
                 tags=None, source=None,
                 path_dict=None):
        """
        :param datalist: the datalist keeping all the traces
        :type datalist: dict
        :param windows: window time information
        :param init_weight: initial weight
        :param longitude: longitude of the stations
        :param latitude: latitude of the stations
        :param tags: tags assigned, could be used to label the category
        :param source: the original source of trace data, could be from
            "sac" or "asdf"
        :param path_dict: path dict to keep the path information of sac file
            or trace.id in asdf
        :return:
        """
        if datalist is None:
            self.datalist = {}
        elif isinstance(datalist, dict):
            self.datalist = datalist
        else:
            raise TypeError("datalist for TraceWindow should be dict: "
                            "{'obsd': obspy.Trace, 'synt': obspy.Trace, ...}")

        if windows is None:
            self.windows = np.array([])  # window time
        else:
            self.windows = np.array(windows)

        self.init_weight = init_weight  # window initial weight

        # station location
        self.latitude = latitude
        self.longitude = longitude

        # Provenance information
        self.tags = tags
        self.source = source
        # sac file path
        self.path_dict = path_dict

        self._sanity_check()

    def _sanity_check(self):

        if self.windows.shape[0] > 0:
            if self.windows.shape[1] != 2:
                raise ValueError("Shape of windows(%s) should be (nwin, 2), "
                                 "which contains starttime and endtime"
                                 % (self.windows.shape))

            if self.windows.shape[0] != self.init_weight.shape[0]:
                raise ValueError("Number of rows(%d) in windows should be "
                                 "consistent with init_weight(%d)"
                                 % (self.windows.shape[0],
                                    self.init_weight.shape[0]))

    def __repr__(self):
        string = "TraceWindow(id: %s -- tag:%s -- source:%s):\n" \
                 % (self.obsd_id, self.tags['obsd'], self.source)
        string += "\tTraces from %s\n" % self.datalist.keys()
        string += "\tNumber of Windows:%d\n" % len(self.windows)
        string += "\tWindow time: %s\n" % self.windows
        string += "\tStation latitude and longitude: [%s, %s]\n" \
                  % (self.latitude, self.longitude)
        return string

    @property
    def data_keys(self):
        return self.datalist.keys()

    @property
    def nwindows(self):
        return len(self.windows)

    @property
    def obsd_id(self):
        try:
            return self.datalist["obsd"].id
        except:
            return None

    @property
    def synt_id(self):
        try:
            return self.datalist["synt"].id
        except:
            return None

    @property
    def station(self):
        try:
            return self.datalist["obsd"].stats.station
        except:
            return None

    @property
    def network(self):
        try:
            return self.datalist["obsd"].stats.network
        except:
            return None

    @property
    def location(self):
        try:
            return self.datalist["obsd"].stats.location
        except:
            return None

    @property
    def channel(self):
        try:
            return self.datalist["obsd"].stats.channel
        except:
            return None

    @property
    def obsd_energy(self):
        """
        Calculate energy inside the window of obsd data
        """
        energy = np.zeros(self.nwindows)
        obsd = self.datalist['obsd']
        dt = obsd.stats.delta
        for _idx in range(self.nwindows):
            istart = int(self.windows[_idx, 0] / dt)
            iend = int(self.windows[_idx, 1] / dt)
            if iend - istart <= 1:
                raise ValueError("Window length < 1, incorrect!")
            energy[_idx] = np.sum(obsd.data[istart:iend] ** 2 * dt)
        return energy


def load_winfile_txt(flexwin_file, initial_weight=1.0):
    """
    Read the txt format of  window file(see the documentation
    online).

    :param flexwin_file:
    :param initial_weight:
    :return:
    """
    trwins = []
    with open(flexwin_file, "r") as f:
        try:
            ntrwins = int(f.readline().strip())
        except Exception as err:
            raise ValueError("Error load in flexwin_file(%s) due to: %s"
                             % (flexwin_file, err))
        if ntrwins == 0:
            logger.warning("Nothing in flexwinfile: %s" % flexwin_file)
            return []

        for idx in range(ntrwins):
            # keep the old format of cmt3d input
            obsd_path = f.readline().strip()
            synt_path = f.readline().strip()
            path_dict = {"obsd": obsd_path, "synt": synt_path}
            nwindows = int(f.readline().strip())
            win_time = np.zeros((nwindows, 2))
            win_weight = np.zeros(nwindows)
            for iwin in range(nwindows):
                content = f.readline().strip().split()
                win_time[iwin, 0] = float(content[0])
                win_time[iwin, 1] = float(content[1])
                if len(content) == 3:
                    win_weight[iwin] = float(content[2])
                else:
                    win_weight[iwin] = initial_weight
            trace_obj = TraceWindow(windows=win_time,
                                    init_weight=win_weight,
                                    path_dict=path_dict)
            trwins.append(trace_obj)
    return trwins


def load_winfile_json(flexwin_file, initial_weight=1.0):
    """
    Read the json format of window file

    :param flexwin_file:
    :param initial_weight:
    :return:
    """
    trwins = []
    with open(flexwin_file, 'r') as fh:
        content = json.load(fh)
        for _sta, _channel in content.iteritems():
            for _chan_win in _channel.itervalues():
                num_wins = len(_chan_win)
                if num_wins <= 0:
                    continue
                obsd_id = _chan_win[0]["channel_id"]
                synt_id = _chan_win[0]["channel_id_2"]
                win_time = np.zeros([num_wins, 2])
                win_weight = np.zeros(num_wins)
                for _idx, _win in enumerate(_chan_win):
                    win_time[_idx, 0] = _win["relative_starttime"]
                    win_time[_idx, 1] = _win["relative_endtime"]
                    if "initial_weighting" in _win.keys():
                        win_weight[_idx] = _win["initial_weighting"]
                    else:
                        win_weight[_idx] = initial_weight
                path_dict = {"obsd": obsd_id, "synt": synt_id}
                trace_obj = TraceWindow(windows=win_time,
                                        init_weight=win_weight,
                                        path_dict=path_dict)
                trwins.append(trace_obj)
    return trwins


def _calibrate_window_time_for_sac(trace_obj):
    """
    In the old FLEXWIN, it uses relative time(compared to CMT time).
    Here, we count window time from the first point. So window time
    should be calibrated using the "b" header value in sac header
    """
    b_tshift = trace_obj.datalist["obsd"].stats.sac['b']
    for _ii in range(trace_obj.nwindows):
        for _jj in range(2):
            trace_obj.windows[_ii, _jj] -= b_tshift
            # WJL: not a good way
            # trace_obj.win_time[_ii, _jj] = \
            #    max(trace_obj.win_time[_ii, _jj], 0.0)
            if trace_obj.windows[_ii, _jj] < 0:
                raise ValueError("Window time(%s) of trace is "
                                 "smaller than zero: %s"
                                 % (trace_obj.obsd_id,
                                    trace_obj.windows))


def load_station_from_text(stationfile):
    """
        Load station information from specfem-like STATIONS file
        """
    station_dict = {}
    with open(stationfile, 'r') as f:
        content = f.readlines()
        content = [line.rstrip('\n') for line in content]
        for line in content:
            info = line.split()
            key = "_".join([info[1], info[0]])
            station_dict[key] = \
                [float(info[2]), float(info[3]), float(info[4])]
    return station_dict


class DataContainer(Sequence):
    """
    Class that contains methods that load data and window information
    """

    def __init__(self, parlist=None):
        """
        :param parlist: derivative parameter name list, the full list
            is in constant.PARLIST
        """
        if parlist is None:
            parlist = []
        if not self._check_parlist(parlist):
            raise ValueError("parlist(%s) not within %s"
                             % (parlist, PARLIST))

        self.parlist = parlist
        self.trwins = []

        # stores the load information
        self._load_info = {}
        # store asdf dataset if asdf mode
        self._asdf_file_dict = None

    @staticmethod
    def _check_parlist(parlist):
        for par in parlist:
            if par not in PARLIST:
                return False
        return True

    @property
    def npar(self):
        """ number of parameters """
        return len(self.parlist)

    def __len__(self):
        return len(self.trwins)

    def __getitem__(self, index):
        return self.trwins[index]

    def __repr__(self):
        string = "DataContainer(npar=%d, ntrace_pair=%d, nwindows=%d)" \
                 % (self.npar, self.__len__(), self.nwindows)
        string += "\nparlist: %s" % self.parlist
        string += "\nLoad info: %s" % self._load_info

    @property
    def nwindows(self):
        nwin = 0
        for _trwin in self.trwins:
            nwin += _trwin.nwindows
        return nwin

    @staticmethod
    def _get_counts(trwins):
        """
        Get counts from trace list
        """
        ntrwins = len(trwins)
        nwins = 0
        for _trace in trwins:
            nwins += _trace.nwindows
        return ntrwins, nwins

    def add_measurements_from_sac(self, flexwinfile, tag="untaged",
                                  initial_weight=1.0,
                                  external_stationfile=None,
                                  window_time_mode="relative_time",
                                  file_format="txt"):
        """
        Add measurments(window and seismograms) from the given flexwinfile
        and the data format should be sac

        :param flexwinfile: input flexwin file
        :param tag: the tag given to the windows in the flexwinfile, usually
            one flexwin is associated to one period band. So you can set the
            tag as the period band, like "27_60s". The tag is used to sort
            the windows into different categories.
        :param initial_weight: the initial weight you assigned to the windows
            in the flexwinfile.
        :param external_stationfile: the external station file that provides
            the coordinate of stations, if they are not in the sac header.
            The stationfile should be in SPECFEM3D_globe fashion, with
            columns "Station Network Latitude Longitude Elevation"
        :param window_time_mode: this is legacy for older flexwin window
            output:
            1) "relative time": window time uses event time as reference(
                event time as 0, which is old flexwin way(legacy).
            2) "absolute_time": window time uses the first point in
                seismograms as starting point, which is the better way
                adopted in pyflex.
        :param file_format: the file format of window file, "txt" or "json".
        :return:
        """
        t1 = time.time()

        _options = ["obsolute_time", "relative_time"]
        window_time_mode = window_time_mode.lower()
        if window_time_mode not in _options:
            raise ValueError("load_winfile mode(%s) incorrect: %s"
                             % (window_time_mode, _options))

        # load the window file and create TraceWindow list
        trwins = self.load_winfile(flexwinfile, initial_weight=initial_weight,
                                   file_format=file_format)
        self.trwins += trwins

        if external_stationfile is not None:
            station_info = \
                load_station_from_text(external_stationfile)
        else:
            station_info = None

        # load the waveform data
        for _trace in trwins:
            self.load_data_from_sac(_trace, tag=tag, mode=window_time_mode,
                                    station_dict=station_info)

        ntrwins, nwins = self._get_counts(trwins)

        t2 = time.time()
        self._load_info[flexwinfile] = {"ntrwins": ntrwins, "nwindows": nwins,
                                        "elapsed_time": t2 - t1}
        logger.info("=" * 10 + " Measurements Loading " + "=" * 10)
        logger.info("Data loaded in sac format: %s" % flexwinfile)
        logger.info("Elapsed time: %5.2f s" % (t2 - t1))
        logger.info("Number of trwins and windows added: [%d, %d]"
                    % (ntrwins, nwins))

    def add_measurements_from_asdf(self, flexwinfile, asdf_file_dict,
                                   obsd_tag=None, synt_tag=None,
                                   initial_weight=1.0,
                                   external_stationfile=None,
                                   file_format="json"):
        """
        Add measurments(window and data) from the given flexwinfile and
        the data format should be asdf. Usually, you can leave the
        obsd_tag=None and synt_tag=None unless if you have multiple tags in
        asdf file.

        :param flexwinfile: input flexwin file. If load from asdf file,
            the window time is assumed to start from the beginning of
            the seismograms(time 0 means the first point).
            There is no legacy mode here.
        :param asdf_file_dict: the dictionary which provides the path
            information of asdf file, for example:
            {"obsd":"/path/obsd/asdf", "synt":"/path/synt/asdf",
             "Mrr": "/path/Mrr/synt/asdf", ...}
        :param obsd_tag: the obsd tag to retrieve the obsd trace from asdf
            file. The tag is like "ds.waveforms.II_AAK.$tag". However, you
            could leave this it to None if you only have one tagged
            seismograms in an asdf file.
        :param synt_tag: same as obsd_tag, used to retrieve the synthetic
            seismograms.
        :param initial_weight: the initial weight you assigned to the windows
            in the flexwinfile.
        :param external_stationfile: the external station file that provides
            the coordinate of stations, if they are not in the sac header.
            The stationfile should be in SPECFEM3D_globe fashion, with
            columns "Station Network Latitude Longitude Elevation"
        :param file_format: the file format of window file, "txt" or "json".
        :return:
        """
        t1 = time.time()

        # load trace and window information
        trwins = self.load_winfile(flexwinfile,
                                   initial_weight=initial_weight,
                                   file_format=file_format)
        self.trwins += trwins

        # load in the asdf data
        asdf_dataset = self.check_and_load_asdf_file(asdf_file_dict)
        self._asdf_file_dict = asdf_file_dict
        if external_stationfile is not None:
            station_info = \
                load_station_from_text(external_stationfile)
        else:
            station_info = None

        # load data for each window
        for _trace in trwins:
            self.load_data_from_asdf(
                _trace, asdf_dataset, obsd_tag=obsd_tag,
                synt_tag=synt_tag, station_dict=station_info)

        ntrwins, nwins = self._get_counts(trwins)

        t2 = time.time()
        self._load_info[flexwinfile] = {"ntrwins": ntrwins, "nwindows": nwins,
                                        "elapsed_time": t2 - t1}

        logger.info("=" * 10 + " Measurements Loading " + "=" * 10)
        logger.info("Data loaded in asdf format: %s" % flexwinfile)
        logger.info("Elapsed time: %5.2f s" % (t2 - t1))
        logger.info("Number of trwins and windows added: [%d, %d]"
                    % (ntrwins, nwins))

    def check_and_load_asdf_file(self, asdf_file_dict):

        if not isinstance(asdf_file_dict, dict):
            raise TypeError("asdf_file_dict should be dictionary. Key from "
                            "parlist and value is the asdf file name")

        necessary_keys = ["obsd", "synt"] + list(self.parlist)
        for key in necessary_keys:
            if key not in asdf_file_dict.keys():
                raise ValueError("key(%s) in parlist is not in "
                                 "asdf_file_dict(%s)"
                                 % (key, asdf_file_dict.keys()))

        dataset = dict()
        for key in necessary_keys:
            dataset[key] = ASDFDataSet(asdf_file_dict[key])
        return dataset

    def load_winfile(self, flexwin_file, initial_weight=1.0,
                     file_format="txt"):
        """
        loading window file. Currently supports two format:
        1) txt; 2) json
        """
        file_format = file_format.lower()
        _options = ["txt", "json"]
        if file_format not in _options:
            raise ValueError("window file format(%s) incorrect: %s"
                             % (file_format, _options))

        if file_format == "txt":
            win_list = load_winfile_txt(flexwin_file,
                                        initial_weight=initial_weight)
        elif file_format == "json":
            win_list = load_winfile_json(flexwin_file,
                                         initial_weight=initial_weight)
        else:
            raise NotImplementedError("Window file format not support:"
                                      "%s" % file_format)
        return win_list

    def load_data_from_sac(self, trace_obj, tag=None, mode=None,
                           station_dict=None):
        """
        Old way of loading obsd and synt data...

        :param trace_obj:
        :return:
        """
        trace_obj.datalist = {}
        trace_obj.tags = {}
        obsd_path = trace_obj.path_dict["obsd"]
        synt_path = trace_obj.path_dict["synt"]
        # obsd
        obsd = read(obsd_path)[0]
        trace_obj.datalist['obsd'] = obsd
        trace_obj.tags['obsd'] = tag

        # calibrate window time if needed
        if mode == "relative_time":
            _calibrate_window_time_for_sac(trace_obj)

        # synt
        trace_obj.datalist['synt'] = read(synt_path)[0]
        trace_obj.tags['synt'] = tag
        # other synt data will be referred as key value:
        # Mrr, Mtt, Mpp, Mrt, Mrp, Mtp, dep, lat, lon, ctm, hdr
        # The path of derived synt follows the CMT3D convetion.
        # for example, if synt data path is "data/II.AAK.00.BHZ.sac",
        # then derived synts are: ["data/II.AAK.00.BHZ.sac.Mrr", ...]
        for deriv_par in self.parlist:
            synt_dev_fn = synt_path + "." + deriv_par
            trace_obj.datalist[deriv_par] = read(synt_dev_fn)[0]
            trace_obj.tags[deriv_par] = tag

        # station information
        if station_dict is None:
            # extract station information from sac header
            trace_obj.longitude = trace_obj.datalist['synt'].stats.sac['stlo']
            trace_obj.latitude = trace_obj.datalist['synt'].stats.sac['stla']
        else:
            key = "_".join([trace_obj.network, trace_obj.station])
            trace_obj.latitude = station_dict[key][0]
            trace_obj.longitude = station_dict[key][1]

        # specify metadata info
        trace_obj.source = "sac"

    def load_data_from_asdf(self, trace_obj, asdf_ds, obsd_tag=None,
                            synt_tag=None, station_dict=None):
        """
        load data from asdf file

        :return:
        """
        # trace
        trace_obj.datalist = dict()
        trace_obj.tags = dict()

        obsd_id = trace_obj.path_dict["obsd"]
        synt_id = trace_obj.path_dict["synt"]
        trace_obj.datalist['obsd'], trace_obj.tags['obsd'] = \
            self._get_trace_from_asdf(obsd_id, asdf_ds['obsd'],
                                      obsd_tag)
        trace_obj.datalist['synt'], trace_obj.tags['synt'] = \
            self._get_trace_from_asdf(synt_id, asdf_ds['synt'],
                                      synt_tag)

        for deriv_par in self.parlist:
            trace_obj.datalist[deriv_par], trace_obj.tags[deriv_par] = \
                self._get_trace_from_asdf(synt_id, asdf_ds[deriv_par],
                                          synt_tag)

        # load station information
        if station_dict is None:
            trace_obj.latitude, trace_obj.longitude = \
                self._get_station_loc_from_asdf(obsd_id,
                                                asdf_ds['synt'])
        else:
            key = "_".join([trace_obj.network, trace_obj.station])
            trace_obj.latitude = station_dict[key][0]
            trace_obj.longitude = station_dict[key][1]

        # specify metadata infor
        trace_obj.source = "asdf"

    @staticmethod
    def _get_station_loc_from_asdf(station_string, asdf_handle):
        """
        Used to extract station location information from stationxml in asdf
        """
        station_info = station_string.split(".")
        if len(station_info) == 4:
            [network, station, _, _] = station_info
        else:
            raise ValueError("Station string should be 'NW.STA.LOC.COMP'."
                             "But current is not correct:%s" % station_info)

        if len(network) >= 3 or len(station) <= 2:
            raise ValueError("Station string should be 'NW.STA.LOC.COMP'"
                             "But current is: %s" % station_info +
                             "You may place the network and station name in"
                             "the wrong order")

        station_name = network + "_" + station
        # get the tag
        st = getattr(asdf_handle.waveforms, station_name)
        if "coordinates" in dir(st):
            latitude = st.coordinates["latitude"]
            longitude = st.coordinates["longitude"]
        elif "StationXML" in dir(st):
            inv = getattr(st, 'StationXML')
            latitude = float(inv[0][0].latitude)
            longitude = float(inv[0][0].longitude)
        else:
            raise ValueError("Can't extract station location")
        return latitude, longitude

    @staticmethod
    def _get_trace_from_asdf(station_string, asdf_handle, tag):
        """
        Used to extract a specific trace out of an asdf file.

        :param station_string:
        :param asdf_handle:
        :param tag:
        :return:
        """
        # just in case people put the whole path, which has no meaning
        # if pyasdf is used
        station_string = os.path.basename(station_string)
        station_info = station_string.split(".")
        if len(station_info) == 4:
            [network, station, loc, channel] = station_info
        else:
            raise ValueError("Station string should be 'NW.STA.LOC.COMP'."
                             "But current is not correct:%s" % station_info)

        if len(network) >= 3 and len(station) <= 2:
            raise ValueError("Station string should be 'NW.STA.LOC.COMP'"
                             "But current is: %s" % station_info +
                             "You may place the network and station name in"
                             "the wrong order")

        station_name = network + "_" + station
        # get the tag
        st = getattr(asdf_handle.waveforms, station_name)
        tag_list = st.get_waveform_tags()
        if tag is None:
            if len(tag_list) != 1:
                raise ValueError("More that 1 data tags in obsd asdf file. "
                                 "For this case, you need specify obsd_tag:%s"
                                 % tag_list)
            stream = getattr(st, tag_list[0])
            tag = tag_list[0]
        else:
            stream = getattr(st, tag)
        tr = stream.select(network=network, station=station, location=loc,
                           channel=channel)[0]
        return tr.copy(), tag

    def write_new_synt_sac(self, outputdir, suffix=None):
        if not os.path.exists(outputdir):
            os.makedirs(outputdir)

        new_synt_dict = self._sort_new_synt()
        for tag, win_array in new_synt_dict.iteritems():
            for window in win_array:
                sta = window.station
                nw = window.network
                component = window.channel
                location = window.location
                filename = "%s.%s.%s.%s.%s.%s.sac" \
                           % (sta, nw, location, component, suffix, tag)
                outputfn = os.path.join(outputdir, filename)
                new_synt = window.datalist['new_synt']
                new_synt.write(outputfn, format='SAC')

    def write_new_synt_asdf(self, file_prefix):
        new_synt_dict = self._sort_new_synt()

        for tag, win_array in new_synt_dict.iteritems():
            filename = "%s.%s.h5" % (file_prefix, tag)
            if os.path.exists(filename):
                os.remove(filename)
                logger.info("Output file exists, removed: %s" % filename)
            else:
                logger.info("Output new synt asdf: %s" % filename)

            ds = ASDFDataSet(filename, mode='w')
            added_list = []
            for window in win_array:
                synt_id = window.datalist['new_synt'].id
                # skip duplicate obsd location id.
                # for example, II.AAK.00.BHZ and II.AAK.10.BHZ will
                # be treated as different traces. But the synt and
                # new synt will be the same. So we only add one
                if synt_id in added_list:
                    continue
                else:
                    added_list.append(synt_id)
                ds.add_waveforms(window.datalist['new_synt'], tag=tag)
            # add stationxml
            _staxml_asdf = self._asdf_file_dict['synt']
            ds_sta = ASDFDataSet(_staxml_asdf)
            self.__add_staxml_from_other_asdf(ds, ds_sta)
            ds.flush()

    def _sort_new_synt(self):
        """
        sort the new synthetic data to to solve reduante output
        """
        new_synt_dict = defaultdict(list)
        for trwin in self.trwins:
            tag = trwin.tags['synt']
            new_synt_dict[tag].append(trwin)
            if "new_synt" not in trwin.datalist:
                raise ValueError("new synt is not in trwin(%s) datalist: %s"
                                 % (trwin, trwin.datalist.keys()))
        return new_synt_dict

    @staticmethod
    def __add_staxml_from_other_asdf(ds, ds_sta):
        sta_tag_list = dir(ds.waveforms)
        for sta_tag in sta_tag_list:
            _sta_data = getattr(ds_sta.waveforms, sta_tag)
            staxml = _sta_data.StationXML
            ds.add_stationxml(staxml)

    def print_summary(self):
        """
        Print summary of data container

        :return:
        """
        nfiles_r = 0
        nfiles_t = 0
        nfiles_z = 0
        nwins_r = 0
        nwins_t = 0
        nwins_z = 0
        for window in self.trwins:
            if window.component[2:3] == "R":
                nfiles_r += 1
                nwins_r += window.num_wins
            elif window.component[2:3] == "T":
                nfiles_t += 1
                nwins_t += window.num_wins
            elif window.component[2:3] == "Z":
                nfiles_z += 1
                nwins_z += window.num_wins
            else:
                raise ValueError(
                    "Unrecognized compoent in windows: %s.%s.%s"
                    % (window.station, window.network, window.component))

        logger.info("=" * 10 + "  Data Summary  " + "=" * 10)
        logger.info("Number of Deriv synt: %d" % len(self.parlist))
        logger.info("   Par: [%s]" % (', '.join(self.parlist)))
        logger.info("Number of data pairs: %d" % self.__len__())
        logger.info("   [Z, R, T] = [%d, %d, %d]"
                    % (nfiles_z, nfiles_r, nfiles_t))
        logger.info("Number of windows: %d" % self.nwindows)
        logger.info("   [Z, R, T] = [%d, %d, %d]"
                    % (nwins_z, nwins_r, nwins_t))
