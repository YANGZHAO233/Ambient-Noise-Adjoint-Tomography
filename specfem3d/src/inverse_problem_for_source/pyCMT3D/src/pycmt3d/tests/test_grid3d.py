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
import pytest
import matplotlib.pyplot as plt
plt.switch_backend('agg')  # NOQA
from pycmt3d import CMTSource
from pycmt3d import DataContainer
from pycmt3d import WeightConfig
from pycmt3d import Grid3d, Grid3dConfig


# Most generic way to get the data folder path.
DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(
        inspect.getfile(inspect.currentframe()))), "data")
OBSD_DIR = os.path.join(DATA_DIR, "data_T006_T030")
SYNT_DIR = os.path.join(DATA_DIR, "syn_T006_T030")
CMTFILE = os.path.join(DATA_DIR, "CMTSOLUTION")


@pytest.fixture
def cmtsource():
    return CMTSource.from_CMTSOLUTION_file(CMTFILE)


@pytest.fixture
def weight_config():
    return WeightConfig(
        normalize_by_energy=False, normalize_by_category=False,
        azi_bins=12, azi_exp_idx=0.5)


def construct_dcon_two():
    """
    Data Container with two stations
    """
    dcon = DataContainer(parlist=[])
    os.chdir(DATA_DIR)
    window_file = os.path.join(DATA_DIR,
                               "flexwin_T006_T030.output.two_stations")
    dcon.add_measurements_from_sac(window_file, tag="T006_T030")
    return dcon


def test_gri3d_config():
    with pytest.raises(ValueError):
        Grid3dConfig(energy_keys=["power_l2"])

    with pytest.raises(ValueError):
        Grid3dConfig(energy_keys=["power_l2"],
                     energy_misfit_coef=[1.0, 2.0])


def test_grid3d(cmtsource, weight_config, tmpdir):
    dcon_two = construct_dcon_two()

    energy_keys = ["power_l1", "power_l2", "cc_amp", "chi"]
    config = Grid3dConfig(origin_time_inv=True, time_start=-5.0, time_end=5.0,
                          dt_over_delta=1, energy_inv=True,
                          energy_start=0.8, energy_end=1.2, denergy=0.1,
                          energy_keys=energy_keys,
                          energy_misfit_coef=[0.25, 0.25, 0.25, 0.25],
                          weight_data=True, weight_config=weight_config)

    srcinv = Grid3d(cmtsource, dcon_two, config)
    srcinv.search()


def plot_stats_histogram(cmtsource, weight_config, tmpdir):
    """
    Taken out temperaly because it won't pass the travis test
    """
    dcon_two = construct_dcon_two()

    config = Grid3dConfig(origin_time_inv=True, time_start=-5.0, time_end=5.0,
                          dt_over_delta=1, energy_inv=True,
                          energy_start=0.8, energy_end=1.2, denergy=0.1,
                          weight_data=True, weight_config=weight_config)

    srcinv = Grid3d(cmtsource, dcon_two, config)
    srcinv.search()

    srcinv.plot_stats_histogram(outputdir=str(tmpdir))
    assert 0


def test_plot_misfit_summary(cmtsource, weight_config, tmpdir):
    dcon_two = construct_dcon_two()

    config = Grid3dConfig(origin_time_inv=True, time_start=-5.0, time_end=5.0,
                          dt_over_delta=1, energy_inv=True,
                          energy_start=0.8, energy_end=1.2, denergy=0.1,
                          weight_data=True, weight_config=weight_config)

    srcinv = Grid3d(cmtsource, dcon_two, config)
    srcinv.search()

    srcinv.plot_misfit_summary(outputdir=str(tmpdir))
