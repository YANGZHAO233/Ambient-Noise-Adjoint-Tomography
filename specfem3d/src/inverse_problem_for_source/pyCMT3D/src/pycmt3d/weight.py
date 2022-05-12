#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function, division, absolute_import
import numpy as np
from spaceweight import SphereAziBin, SpherePoint
from . import logger
from .util import distance, get_window_idx, get_trwin_tag
from .measure import _energy_, _envelope
from .data_container import MetaInfo


def calculate_energy_weighting(trwin, mode="window"):
    mode = mode.lower()
    _options = ["window", "all"]
    if mode not in _options:
        raise ValueError("Input mode(%s) must be in: %s" % (mode, _options))

    obsd = trwin.datalist["obsd"]
    dt = obsd.stats.delta
    win_idx = get_window_idx(trwin.windows, dt)
    wav_energy = np.zeros(trwin.nwindows)
    env_energy = np.zeros(trwin.nwindows)

    if mode == "all":
        average_energy = _energy_(obsd.data) / obsd.stats.npts
        wav_energy = \
            average_energy * (win_idx[:, 1] - win_idx[:, 0]) * dt
        average_energy = _energy_(_envelope(obsd.data)) / obsd.stats.npts
        env_energy = \
            average_energy * (win_idx[:, 1] - win_idx[:, 0]) * dt
    elif mode == "window":
        for idx, _win in enumerate(win_idx):
            wav_energy[idx] = \
                _energy_(obsd.data[_win[0]:_win[1]]) * dt
            env_energy[idx] = \
                _energy_(_envelope(obsd.data[_win[0]:_win[1]])) * dt
    return wav_energy, env_energy


def setup_energy_weight(metas, data_container):
    for meta, trwin in zip(metas, data_container):
        we, ee = calculate_energy_weighting(trwin, mode="all")
        meta.prov['wav_energy'] = we
        meta.prov['env_energy'] = ee


class Weight(object):
    """
    Class that handles the solver part of source inversion

    :param cmtsource: earthquake source
    :type cmtsource: :class:`pycmt3d.CMTSource`
    :param data_container: all data and window
    :type data_container: :class:`pycmt3d.DataContainer`
    :param config: configuration for source inversion
    :type config: :class:`pycmt3d.Config`
    """
    def __init__(self, cmtsource, data_container, config):

        self.cmtsource = cmtsource
        self.data_container = data_container

        self.config = config

        # center point set for cmtsource
        self.center = SpherePoint(
            self.cmtsource.latitude, self.cmtsource.longitude,
            tag="cmtsource")
        # keep category information
        self.point_bins = {}

        # init meta list for store weight information
        self._init_metas()

    def _init_metas(self):
        self.metas = []
        for trwin in self.data_container:
            meta = MetaInfo(obsd_id=trwin.obsd_id, synt_id=trwin.synt_id,
                            weights=np.ones(trwin.nwindows),
                            prov={})
            self.metas.append(meta)

    def setup_weight(self):
        """
        Use Window information to setup weight.

        :returns:
        """
        self.sort_into_category()

        self.setup_weight_for_location()

        if self.config.mode == "default":
            self.setup_weight_for_epicenter_distance()
            # according to original version of cmt3d, which has weighting on:
            # components(Z, R, T)
            self.setup_weight_for_component()

        self.normalize_weight()

        logger.debug("Detailed Weighting information")
        for meta in self.metas:
            logger.debug("meta: %s" % meta)

    def normalize_weight(self):
        """
        Normalize all weight value. Normalize the average weighting
        (for each window) to 1.
        """
        weight_sum = 0
        for meta in self.metas:
            weight_sum += sum(meta.weights)
        factor = self.data_container.nwindows / weight_sum
        for meta in self.metas:
            meta.weights *= factor

    def sort_into_category(self):
        """
        Sort data into different cateogeries, by the trwin.tag and
        trwin.channel. trwin.tag is usually the period band, so
        category would be like "27_60.BHZ", "27_60.BHR", "27_60.BHT",
        and "60_120.BHZ", "60_120.BHR", "60_120.BHT".
        """
        pbins = {}
        for idx, trwin in enumerate(self.data_container):
            if self.config.normalize_by_category:
                cat = get_trwin_tag(trwin)
            else:
                cat = "all"
            if cat not in pbins:
                pbins[cat] = []
            pbins[cat].append(
                SpherePoint(trwin.latitude, trwin.longitude, tag=idx))

        logger.info("Category: %s" % pbins.keys())

        self.point_bins = pbins

    def setup_weight_for_component(self):
        for cat, points in self.point_bins.iteritems():
            for point in points:
                comp = self.data_container.trwins[point.tag].channel[-1]
                comp_weight = self.config.comp_weight[comp]
                meta = self.metas[point.tag]
                meta.weights *= comp_weight
                meta.prov["component_weight"] = comp_weight

    def setup_weight_for_azimuth(self):
        """
        Sort station azimuth into bins and assign weight to each bin
        """
        weight_dict = {}
        idx_dict = {}
        for cat, points in self.point_bins.iteritems():
            weight = SphereAziBin(
                points, center=self.center, bin_order=self.config.azi_exp_idx,
                nbins=self.config.azi_bins, remove_duplicate=False,
                normalize_mode="average")
            weight.calculate_weight()
            weight_dict[cat] = weight.points_weights
            idx_dict[cat] = weight.points_tags
        return weight_dict, idx_dict

    def setup_weight_for_epicenter_distance(self):
        """
        This is just a courtesy functions which works the same as CMT3D
        distance weighting
        """
        ref_dist = self.config.ref_dist
        for cat, points in self.point_bins.iteritems():
            for point in points:
                trwin = self.data_container.trwins[point.tag]
                comp = trwin.channel[-1]
                dist = distance(
                    self.center.coordinate[0], self.center.coordinate[1],
                    point.coordinate[0], point.coordinate[1])
                meta = self.metas[point.tag]
                epi_weights = np.zeros(trwin.nwindows)
                for win_idx in range(trwin.nwindows):
                    if comp == "T":
                        epi_weights[win_idx] = \
                            (dist/ref_dist) ** self.config.love_dist_weight
                    elif win_idx == 0:
                        epi_weights[win_idx] = \
                            (dist/ref_dist) ** self.config.pnl_dist_weight
                    else:
                        epi_weights[win_idx] = \
                            (dist/ref_dist) ** self.config.rayleigh_dist_weight
                # print("comp, dist, ref_dist:", comp, dist, ref_dist)
                # print("epi_weights:", comp, epi_weights)
                meta.weights /= epi_weights
                meta.prov["epi_dist_factor"] = epi_weights

    def setup_weight_for_location(self):
        """
        setup weight from station location information, including distance,
        component and azimuth. This weight applies on station level.

        :param window:
        :param naz_bin:
        :param naz_bin_all:
        :return:
        """
        # set up weight based on azimuth distribution
        weight_dict, idx_dict = self.setup_weight_for_azimuth()
        # set up weight based on station locations
        for cat in weight_dict:
            weights = weight_dict[cat]
            idxs = idx_dict[cat]
            for idx, weight in zip(idxs, weights):
                self.metas[idx].weights *= weight
                self.metas[idx].prov["azimuth_weight"] = weight
