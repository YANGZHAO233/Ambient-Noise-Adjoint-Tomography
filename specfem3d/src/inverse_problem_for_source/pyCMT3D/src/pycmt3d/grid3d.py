#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Class for grid search for origin time and scalar moment for CMT source

:copyright:
    Wenjie Lei (lei@princeton.edu), 2016
:license:
    GNU Lesser General Public License, version 3 (LGPLv3)
    (http://www.gnu.org/licenses/lgpl-3.0.en.html)
"""
from __future__ import print_function, division, absolute_import
import os
import numpy as np
import matplotlib.pyplot as plt
from copy import deepcopy

from . import logger
from .weight import Weight
from .data_container import MetaInfo
from .measure import calculate_variance_on_trace
from .plot_util import PlotStats


class Grid3dConfig(object):
    def __init__(self, origin_time_inv=True, time_start=-5.0, time_end=5.0,
                 dt_over_delta=1, energy_inv=True,
                 energy_start=0.8, energy_end=1.2, denergy=0.1,
                 energy_keys=None, energy_misfit_coef=None,
                 weight_data=False, weight_config=None,
                 taper_type="tukey"):

        self.origin_time_inv = origin_time_inv
        self.time_start = time_start
        self.time_end = time_end
        self.dt_over_delta = dt_over_delta

        self.energy_inv = energy_inv
        self.energy_start = energy_start
        self.energy_end = energy_end
        self.denergy = denergy

        # energy_keys could contain ["power_l1", "power_l2", "cc_amp", "chi"]
        if energy_keys is None:
            energy_keys = ["power_l1", "power_l2", "cc_amp"]
            if energy_misfit_coef is None:
                energy_misfit_coef = [0.75, 0.25, 1.0]
        else:
            if energy_misfit_coef is None:
                raise ValueError("energy_misfit_coef must be provided"
                                 "according to energy_keys")

        if len(energy_misfit_coef) != len(energy_keys):
            raise ValueError("Length of energy keys and coef must be"
                             " the same: %s, %s" %
                             (energy_keys, energy_misfit_coef))

        self.energy_keys = energy_keys
        self.energy_misfit_coef = np.array(energy_misfit_coef)

        self.weight_data = weight_data
        self.weight_config = weight_config

        self.taper_type = taper_type


class Grid3d(object):
    """
    Class that handle the grid search solver for origin time and moment scalar
    """
    def __init__(self, cmtsource, data_container, config):

        self.cmtsource = cmtsource
        self.data_container = data_container
        self.config = config

        self.metas = []

        self.new_cmtsource = None

        self.t00_best = None
        self.t00_misfit = None
        self.t00_array = None

        self.m00_best = None
        self.m00_misfit = None
        self.m00_array = None

    def setup_window_weight(self):
        """
        Use Window information to setup weight for each window.

        :returns:
        """
        self._init_metas()

        logger.info("*" * 15)
        logger.info("Start weighting...")
        weight_obj = Weight(self.cmtsource, self.data_container,
                            self.config.weight_config)
        weight_obj.setup_weight()

        for meta, weight_meta in zip(self.metas, weight_obj.metas):
            if meta.obsd_id != weight_meta.obsd_id or \
                    meta.synt_id != weight_meta.synt_id:
                raise ValueError("weight.metas and self.metas are different"
                                 "on meta: %s %s" % (meta.id, weight_meta.id))
            meta.weights = weight_meta.weights
            meta.prov.update(weight_meta.prov)

    def _init_metas(self):
        """
        Initialize the self.metas list. Keep the same order with the
        data container
        """
        for trwin in self.data_container:
            metainfo = MetaInfo(obsd_id=trwin.obsd_id, synt_id=trwin.synt_id,
                                weights=trwin.init_weight, Aws=[], bws=[],
                                Aes=[], bes=[], prov={})
            self.metas.append(metainfo)

    def search(self):

        self.setup_window_weight()
        self.grid_search_origin_time()
        self.grid_search_energy()

        self.prepare_new_cmtsource()
        self.prepare_new_synthetic()

    def prepare_new_cmtsource(self):
        newcmt = deepcopy(self.cmtsource)

        logger.info("Preparing new cmtsource...")
        if self.config.origin_time_inv:
            newcmt.cmt_time += self.t00_best
            logger.info("\tadding time shift to cmt origin time:"
                        "%s + %fsec= %s"
                        % (self.cmtsource.cmt_time, self.t00_best,
                           newcmt.cmt_time))

        if self.config.energy_inv:
            attrs = ["m_rr", "m_tt", "m_pp", "m_rt", "m_rp", "m_tp"]
            for attr in attrs:
                newval = self.m00_best * getattr(newcmt, attr)
                setattr(newcmt, attr, newval)
            logger.info("\tmultiply scalar moment change by %f%%"
                        % (self.m00_best * 100))

        self.new_cmtsource = newcmt

    def prepare_new_synthetic(self):
        logger.info("Reconstruct new synthetic seismograms...")
        for trwin in self.data_container:
            new_synt = trwin.datalist["synt"].copy()
            if self.config.origin_time_inv:
                new_synt.stats.starttime += self.t00_best
            if self.config.energy_inv:
                new_synt.data *= self.m00_best
            trwin.datalist["new_synt"] = new_synt

        for meta, trwin in zip(self.metas, self.data_container):
            obsd = trwin.datalist["obsd"]
            new_synt = trwin.datalist["new_synt"]
            meta.prov["new_synt"] = \
                calculate_variance_on_trace(obsd, new_synt, trwin.windows,
                                            self.config.taper_type)
            # becuase calculate_variance_on_trace assumes obsd and new_synt
            # starting at the same time(which is not the case since we
            # correct the starting time of new_synt)
            if self.config.origin_time_inv:
                meta.prov["new_synt"]["tshift"] -= self.t00_best

    def calculate_tshift(self):
        """
        This step actually calculate the whole measurements between
        obsd and synt
        """
        for meta, trwin in zip(self.metas, self.data_container):
            obsd = trwin.datalist["obsd"]
            synt = trwin.datalist["synt"]
            meta.prov["synt"] = \
                calculate_variance_on_trace(obsd, synt, trwin.windows,
                                            self.config.taper_type)

    def grid_search_origin_time(self):

        logger.info("Origin time grid search")
        self.calculate_tshift()

        t00_s = self.config.time_start
        t00_e = self.config.time_end
        dt00 = self.config.dt_over_delta * \
            self.data_container[0].datalist['obsd'].stats.delta

        logger.info("Grid search time start and end: [%8.3f, %8.3f]"
                    % (t00_s, t00_e))
        logger.info("Grid search time interval:%10.3f" % dt00)

        tshifts = []
        weights = []
        for meta in self.metas:
            tshifts.extend(meta.prov["synt"]["tshift"])
            weights.extend(meta.weights)
        tshifts = np.array(tshifts)

        if self.config.weight_data:
            weights = np.array(weights)
        else:
            weights = np.ones(len(tshifts))

        t00_array = np.arange(t00_s, t00_e+dt00, dt00)
        nt00 = t00_array.shape[0]
        final_misfits = np.zeros(nt00)

        for i in range(nt00):
            t00 = t00_array[i]
            final_misfits[i] = np.sum(weights * (tshifts - t00) ** 2)

        min_idx = final_misfits.argmin()
        t00_best = t00_array[min_idx]

        logger.info("Minimum t00(relative to cmt origin time): %6.3f"
                    % t00_best)
        if min_idx == 0 or min_idx == (nt00 - 1):
            logger.warning("Origin time search hit boundary, which means"
                           "search range should be reset")

        self.t00_best = t00_best
        self.t00_array = t00_array
        self.t00_misfit = final_misfits

    def calculate_misfit_for_m00(self, m00):
        power_l1s = []
        power_l2s = []
        cc_amps = []
        chis = []

        for trwin in self.data_container:
            obsd = trwin.datalist["obsd"]
            synt = trwin.datalist["synt"].copy()
            synt.data *= m00
            measures = \
                calculate_variance_on_trace(obsd, synt, trwin.windows)
            power_l1s.extend(measures["power_l1"])
            power_l2s.extend(measures["power_l2"])
            cc_amps.extend(measures["cc_amp"])
            chis.extend(measures["chi"])

        measures = {"power_l1": np.array(power_l1s),
                    "power_l2": np.array(power_l2s),
                    "cc_amp": np.array(cc_amps),
                    "chi": np.array(chis)}
        return measures

    def grid_search_energy(self):

        logger.info('Energy grid Search')

        m00_s = self.config.energy_start
        m00_e = self.config.energy_end
        dm00 = self.config.denergy
        logger.info("Grid search energy start and end: [%6.3f, %6.3f]"
                    % (m00_s, m00_e))
        logger.info("Grid search energy interval: %6.3f" % dm00)

        m00_array = np.arange(m00_s, m00_e+dm00, dm00)
        nm00 = m00_array.shape[0]

        final_misfits = np.zeros(nm00)
        cat_misfits = {}
        for key in self.config.energy_keys:
            cat_misfits[key] = np.zeros(nm00)

        if self.config.weight_data:
            weights = []
            for meta in self.metas:
                weights.extend(meta.weights)
            weights = np.array(weights)
        else:
            weights = np.ones(len(self.data_container.nwindows))

        for i in range(nm00):
            m00 = m00_array[i]
            logger.info("Looping on m00: %f" % m00)
            measures = \
                self.calculate_misfit_for_m00(m00)
            for key_idx, key in enumerate(self.config.energy_keys):
                cat_val = np.sum(measures[key]**2 * weights)
                cat_misfits[key][i] = cat_val
                final_misfits[i] += \
                    self.config.energy_misfit_coef[key_idx] * cat_val

        # find minimum
        min_idx = final_misfits.argmin()
        m00_best = m00_array[min_idx]

        if min_idx == 0 or min_idx == (nm00 - 1):
            logger.warning("Energy search reaches boundary, which means the"
                           "search range should be reset")
        logger.info("best m00: %6.3f" % m00_best)
        self.m00_best = m00_best
        self.m00_array = m00_array
        self.m00_misfit = final_misfits
        self.m00_cat_misfit = cat_misfits

    def write_new_cmtfile(self, outputdir="."):
        suffix = "grid"
        if self.config.origin_time_inv:
            suffix += ".time"
        if self.config.energy_inv:
            suffix += ".energy"
        fn = os.path.join(outputdir, "%s.%s.inv" % (self.cmtsource.eventname,
                                                    suffix))
        logger.info("New cmtsource file: %s" % fn)
        self.new_cmtsource.write_CMTSOLUTION_file(fn)

    def plot_stats_histogram(self, outputdir=".", figure_format="png"):
        """
        Plot the histogram of meansurements inside windows for
        old and new synthetic seismograms
        """
        figname = os.path.join(outputdir, "window.stats.%s" % figure_format)
        plot_util = PlotStats(self.data_container, self.metas, figname)
        plot_util.plot_stats_histogram()

    def plot_misfit_summary(self, outputdir=".", figure_format="png"):
        """
        Plot histogram and misfit curve of origin time result

        :param outputdir:
        :return:
        """
        figname = os.path.join(outputdir, "tshift.misfit.%s" % figure_format)
        logger.info("tshift misfit figure: %s" % figname)
        self.plot_tshift_misfit_summary(figname)

        figname = os.path.join(outputdir, "energy.misfit.%s" % figure_format)
        logger.info("energy misfit figure: %s" % figname)
        self.plot_energy_misfit_summary(figname)

    def plot_tshift_misfit_summary(self, figname):
        plt.figure()
        plt.plot(self.t00_array, self.t00_misfit, label="misfit values")

        idx = np.where(self.t00_array == self.t00_best)[0]
        plt.plot(self.t00_best, self.t00_misfit[idx], "r*",
                 markersize=10, label="min misfit")

        plt.xlabel("time shift(sec)")
        plt.ylabel("misfit values")
        plt.grid()
        plt.legend(numpoints=1)
        plt.savefig(figname)

    def plot_energy_misfit_summary(self, figname):
        """
        Plot histogram of dlnA

        :param outputdir:
        :return:
        """
        keys = self.config.energy_keys
        nkeys = len(keys)
        ncols = nkeys + 1
        plt.figure(figsize=(5*ncols, 5))

        min_idx = np.where(self.m00_array == self.m00_best)[0]

        for idx, key in enumerate(self.config.energy_keys):
            plt.subplot(1, nkeys+1, idx+1)
            plt.plot(self.m00_array, self.m00_cat_misfit[key], label="misfit")
            plt.plot(self.m00_best, self.m00_cat_misfit[key][min_idx], "r*",
                     markersize=10, label="min misfit")
            plt.xlabel("scalar moment")
            plt.ylabel("%s misift" % key)
            plt.legend(numpoints=1)
            plt.grid()

        plt.subplot(1, nkeys+1, nkeys+1)
        plt.plot(self.m00_array, self.m00_misfit, label="misfit values")

        plt.plot(self.m00_best, self.m00_misfit[min_idx], "r*",
                 markersize=10, label="min misfit")

        plt.xlabel("scalar moment change")
        plt.ylabel("Overall misfit")
        plt.grid()
        plt.legend(numpoints=1)
        plt.tight_layout()
        plt.savefig(figname)
