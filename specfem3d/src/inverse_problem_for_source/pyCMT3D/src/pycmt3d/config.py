#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Config classes for weighting and inversion

:copyright:
    Wenjie Lei (lei@princeton.edu), 2016
:license:
    GNU Lesser General Public License, version 3 (LGPLv3)
    (http://www.gnu.org/licenses/lgpl-3.0.en.html)
"""
from __future__ import print_function, division, absolute_import
import numpy as np
from .util import _float_array_to_str
from .constant import DEFAULT_SCALE_VECTOR, NM, NML, PARLIST


class WeightConfigBase(object):
    """
    Base class of weight config. Shouldn't be used for most cases.
    Since we introduce complex weighting strategies here, so I think
    it might be worth to seperate WeightConfig from the Config.
    """
    def __init__(self, mode, normalize_by_energy=False,
                 normalize_by_category=False):
        self.mode = mode.lower()
        self.normalize_by_energy = normalize_by_energy
        self.normalize_by_category = normalize_by_category

    def __repr__(self):
        string = "Weight Strategy:\n"
        string += "mode: %s\n" % self.mode
        string += "normalize_by_energy: %s\n" % self.normalize_by_energy
        string += "normalize_by_category: %s\n" % self.normalize_by_category
        return string

    def __str__(self):
        return self.__repr__()


class WeightConfig(WeightConfigBase):
    def __init__(self, normalize_by_energy=False,
                 normalize_by_category=False,
                 azi_bins=12, azi_exp_idx=0.5):
        WeightConfigBase.__init__(
            self, "classic", normalize_by_energy=normalize_by_energy,
            normalize_by_category=normalize_by_category)
        self.azi_bins = azi_bins
        self.azi_exp_idx = azi_exp_idx

    def __repr__(self):
        string = "Weight Strategy:\n"
        string += "mode: %s\n" % self.mode
        string += "normalize_by_energy: %s\n" % self.normalize_by_energy
        string += "normalize_by_category: %s\n" % self.normalize_by_category
        string += "Azimuth bins and exp index: %d, %f" % (self.azi_bins,
                                                          self.azi_exp_idx)
        return string


class DefaultWeightConfig(WeightConfigBase):
    """
    Weight config in original CMT3D packages
    """
    def __init__(self, normalize_by_energy=False, normalize_by_category=False,
                 comp_weight=None,
                 love_dist_weight=0.78, pnl_dist_weight=1.15,
                 rayleigh_dist_weight=0.55,
                 azi_exp_idx=0.5, azi_bins=12,
                 ref_dist=1.0):
        WeightConfigBase.__init__(self, "default",
                                  normalize_by_energy=normalize_by_energy,
                                  normalize_by_category=normalize_by_category)
        if comp_weight is None:
            self.comp_weight = {"Z": 2.0, "R": 1.0, "T": 2.0}
        else:
            self.comp_weight = comp_weight

        self.love_dist_weight = love_dist_weight
        self.pnl_dist_weight = pnl_dist_weight
        self.rayleigh_dist_weight = rayleigh_dist_weight
        self.azi_exp_idx = azi_exp_idx
        self.azi_bins = azi_bins
        self.ref_dist = ref_dist

    def __repr__(self):
        string = "Weight Strategy:\n"
        string += "mode: %s\n" % self.mode
        string += "normalize_by_energy: %s\n" % self.normalize_by_energy
        string += "normalize_by_category: %s\n" % self.normalize_by_category
        string += "component weight: %s\n" % self.comp_weight
        string += "pnl, rayleigh and love distance weights: %f, %f, %f\n" % (
            self.pnl_dist_weight, self.rayleigh_dist_weight,
            self.love_dist_weight)
        string += "number of azimuth bins: %d\n" % self.azi_bins
        string += "azimuth exponential index: %f\n" % self.azi_exp_idx
        return string


class Config(object):
    """
    Configuration for source inversion


    """

    def __init__(self, npar, dlocation=0.0, ddepth=0.0, dmoment=0.0,
                 scale_vector=None, zero_trace=True, double_couple=False,
                 envelope_coef=0.5,  max_nl_iter=60,
                 damping=0.0, station_correction=True,
                 weight_data=True, weight_config=None,
                 bootstrap=True, bootstrap_repeat=300,
                 bootstrap_subset_ratio=0.4,
                 taper_type="tukey"):
        """
        :param npar: number of parameters to be inverted
        :param dlocation: location perturbation when calculated perturbed
            synthetic data, unit is degree
        :param ddepth: depth perturbation, unit is meter
        :param dmoment: moment perturbation, unit is dyne * cm
        :param scale_vector: the scaling vector for d***. If none, then
            it will use the default
        :param zero_trace: bool value of whether applies zero-trace constraint
        :param double_couple: bool value of whether applied double-couple
            constraint
        :param envelope_coef: the coefficient of envelope misfit function,
            should be within [0, 1]
        :param max_nl_iter: max number of non-linear iterations
        :param damping: damping coefficient
        :param station_correction: bool value of whether applies station
            correction
        :param weight_data: bool value of weighting data
        :param weight_config: the weighting configuration
        :param bootstrap: bool value of whether applied bootstrap method
        :param bootstrap_repeat: bootstrap iterations
        :param bootstrap_subset_ratio: the subset ratio for bootstrap runs
        :param taper_type: the taper type used for taper the seismograms
            in the windows
        """

        _options = [6, 7, 9, 10, 11]
        if npar not in _options:
            print('Error: the current npar (number of parameters) is: %d'
                  % self.npar)
            print('The npar (number of parameters) must be within %s'
                  % _options)
            print('When npar is 6: moment tensor')
            print('When npar is 7: moment tensor + depth')
            print('When npar is 9: moment tensor + depth + location'
                  '(e.g. latitude and longitude)')
            print('When npar is 10(not implemented yet): '
                  'moment tensor + depth + location + time')
            print('When npar is 11(not implemented yet): '
                  'moment tensor + depth + location + time + half duration')
            raise ValueError("Re-enter npar")
        if npar in [10, 11]:
            raise NotImplementedError("Not implemented with npar=%d" % npar)

        self.npar = npar
        self.parlist = PARLIST[:npar]

        self.dlocation = dlocation
        self.ddepth = ddepth
        self.dmoment = dmoment
        self._check_perturbation_sanity()

        self.weight_data = weight_data
        self.weight_config = weight_config

        self.station_correction = station_correction
        self.zero_trace = zero_trace
        self.double_couple = double_couple

        if envelope_coef < 0.0 or envelope_coef > 1.0:
            raise ValueError("Envelope coefficient must be within [0, 1]")
        self.envelope_coef = envelope_coef

        if max_nl_iter <= 0:
            raise ValueError("max_nl_iter(%d) must be larger than 0"
                             % max_nl_iter)
        self.max_nl_iter = max_nl_iter
        self.damping = damping

        # scaling term
        if scale_vector is None:
            self.scale_vector = DEFAULT_SCALE_VECTOR[:npar]
        elif len(scale_vector) != npar:
            raise ValueError("Length of scale_vector(%d) must be %d"
                             % (len(scale_vector), npar))
        else:
            self.scale_vector = scale_vector

        # original cmt perturbation
        self.dcmt_par = np.array(
            [self.dmoment, self.dmoment, self.dmoment, self.dmoment,
             self.dmoment, self.dmoment, self.ddepth, self.dlocation,
             self.dlocation, 1.0, 1.0])[:npar]
        # scaled cmt perturbation
        self.dcmt_par_scaled = self.dcmt_par / self.scale_vector

        self.bootstrap = bootstrap
        self.bootstrap_repeat = bootstrap_repeat
        self.bootstrap_subset_ratio = bootstrap_subset_ratio

        self.taper_type = taper_type

    def _check_perturbation_sanity(self):
        """
        Check cmt perturbation is set according to npar
        """
        if self.npar >= NM:
            if not self.dmoment:
                raise ValueError("npar(%d) requires dmoment(%s) > 0" %
                                 (self.npar, self.dmoment))
        if self.npar >= (NM + 1):
            if not self.ddepth:
                raise ValueError("npar(%d) requires ddepth(%s) > 0" %
                                 (self.npar, self.ddepth))

        if self.npar >= NML:
            if not self.dlocation:
                raise ValueError("npar(%d) requires dlocation(%s) > 0" %
                                 (self.npar, self.ddepth))

    def __repr__(self):
        npar = self.npar
        string = "="*10 + "  Config Summary  " + "="*10 + "\n"

        string += "Number of inversion params: %d\n" % npar
        string += "Deriv params: [%s]\n" % ",".join(self.parlist)
        string += \
            "CMT perturbation: %s\n" % _float_array_to_str(self.dcmt_par)
        string += \
            "CMT scaling term: %s\n" % _float_array_to_str(self.scale_vector)

        string += "-" * 5 + "\nInversion Schema\n"
        string += "Zero trace: %s  Doulbe couple: %s\n" % (
            self.zero_trace, self.double_couple)
        string += "Damping:%s\n" % self.damping
        string += "Bootstrap:%s\n" % self.bootstrap
        if self.bootstrap:
            string += "Bootstrap repeat times: %d\n" % self.bootstrap_repeat

        string += "-" * 5 + "\nWeight Schema\n"
        string += "%s" % str(self.weight_config)
        return string
