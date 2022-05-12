#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Pycmt3d test suite.

Run with pytest.

:copyright:
    Wenjie Lei (lei@princeton.edu)
:license:
    GNU General Public License, Version 3
    (http://www.gnu.org/copyleft/gpl.html)
"""
from __future__ import print_function, division
import inspect
import os
import pycmt3d.config as conf
import numpy.testing as npt


# Most generic way to get the data folder path.
DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(
        inspect.getfile(inspect.currentframe()))), "data")
OBSD_DIR = os.path.join(DATA_DIR, "data_T006_T030")
SYNT_DIR = os.path.join(DATA_DIR, "syn_T006_T030")


def test_weightconfig():
    config = conf.WeightConfigBase(mode="default", normalize_by_energy=True,
                                   normalize_by_category=True)
    assert config.mode == "default"
    assert config.normalize_by_energy
    assert config.normalize_by_category


def test_defaultweightconfig():
    config = conf.DefaultWeightConfig(
        normalize_by_energy=True, normalize_by_category=True)

    assert config.mode == "default"
    assert config.normalize_by_energy
    assert config.normalize_by_category
    assert config.comp_weight == {"Z": 2.0, "R": 1.0, "T": 2.0}
    assert config.love_dist_weight == 0.78
    assert config.pnl_dist_weight == 1.15
    assert config.rayleigh_dist_weight == 0.55
    assert config.azi_exp_idx == 0.5


def test_config():
    weight_config = conf.DefaultWeightConfig(
        normalize_by_energy=True, normalize_by_category=True)

    config = conf.Config(9, dlocation=0.03, ddepth=3.0, dmoment=2.0e23,
                         zero_trace=True, double_couple=False,
                         damping=0.01, station_correction=True,
                         weight_data=True, weight_config=weight_config,
                         bootstrap=True, bootstrap_repeat=500)
    assert config.npar == 9
    assert config.parlist == ("Mrr", "Mtt", "Mpp", "Mrt", "Mrp", "Mtp",
                              "dep", "lon", "lat")
    assert config.dlocation == 0.03
    assert config.ddepth == 3.0
    assert config.dmoment == 2.0e23
    assert config.zero_trace
    assert not config.double_couple
    assert config.damping == 0.01
    assert config.station_correction
    assert config.weight_data
    assert config.weight_config == weight_config
    assert config.bootstrap
    assert config.bootstrap_repeat == 500

    npt.assert_allclose(config.dcmt_par, [2.0e23, 2.0e23, 2.0e23, 2.0e23,
                                          2.0e23, 2.0e23,
                                          3.0, 0.03, 0.03])
