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
import numpy as np
import numpy.testing as npt
import pytest
from pycmt3d.source import CMTSource
from pycmt3d.data_container import DataContainer
from pycmt3d.config import DefaultWeightConfig
from pycmt3d.constant import PARLIST
from pycmt3d.weight import Weight


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
def default_config():
    return DefaultWeightConfig(
        normalize_by_energy=False, normalize_by_category=False,
        comp_weight={"Z": 2.0, "R": 1.0, "T": 2.0},
        love_dist_weight=0.78, pnl_dist_weight=1.15,
        rayleigh_dist_weight=0.55, azi_exp_idx=0.5)


@pytest.fixture
def dcon_one():
    """
    Data container with only one station
    """
    dcon = DataContainer(parlist=PARLIST[:9])
    os.chdir(DATA_DIR)
    window_file = os.path.join(DATA_DIR,
                               "flexwin_T006_T030.output.one_station")
    dcon.add_measurements_from_sac(window_file, tag="T006_T030")
    return dcon


def construct_dcon_two():
    """
    Data Container with two stations
    """
    dcon = DataContainer(parlist=PARLIST[:9])
    os.chdir(DATA_DIR)
    window_file = os.path.join(DATA_DIR,
                               "flexwin_T006_T030.output.two_stations")
    dcon.add_measurements_from_sac(window_file, tag="T006_T030")
    return dcon


def weight_sum(metas):
    sumw = 0
    for meta in metas:
        sumw += np.sum(meta.weights)
    return sumw


def test_weighting_one_1(cmtsource, dcon_one):
    config = DefaultWeightConfig(
        normalize_by_energy=False, normalize_by_category=False,
        comp_weight={"Z": 1.0, "R": 1.0, "T": 1.0},
        love_dist_weight=1.0, pnl_dist_weight=1.0,
        rayleigh_dist_weight=1.0, azi_exp_idx=0.5)

    weight = Weight(cmtsource, dcon_one, config)
    weight.setup_weight()

    for meta in weight.metas:
        for w in meta.weights:
            npt.assert_allclose(w, 1.0)


def test_weighting_one_2(cmtsource, dcon_one):
    config = DefaultWeightConfig(
        normalize_by_energy=False, normalize_by_category=False,
        comp_weight={"Z": 2.0, "R": 1.0, "T": 2.0},
        love_dist_weight=1.0, pnl_dist_weight=1.0,
        rayleigh_dist_weight=1.0, azi_exp_idx=0.5)

    weight = Weight(cmtsource, dcon_one, config)
    weight.setup_weight()

    sumw = weight_sum(weight.metas)
    npt.assert_allclose(sumw, dcon_one.nwindows)

    npt.assert_allclose(weight.metas[0].weights, 5/9.0)
    npt.assert_allclose(weight.metas[1].weights,
                        [10./9, 10./9])
    npt.assert_allclose(weight.metas[2].weights,
                        [10./9, 10./9])


def test_weighting_one_3(cmtsource, dcon_one):
    config = DefaultWeightConfig(
        normalize_by_energy=False, normalize_by_category=False,
        comp_weight={"Z": 2.0, "R": 1.0, "T": 2.0},
        love_dist_weight=0.78, pnl_dist_weight=1.15,
        rayleigh_dist_weight=0.5, azi_exp_idx=0.5)

    weight = Weight(cmtsource, dcon_one, config)
    weight.setup_weight()

    sumw = weight_sum(weight.metas)
    npt.assert_allclose(sumw, dcon_one.nwindows)

    for meta in weight.metas:
        print(meta)
    npt.assert_allclose(weight.metas[0].weights, 0.46054087)
    npt.assert_allclose(weight.metas[1].weights,
                        [1.13969125, 1.13969125])
    npt.assert_allclose(weight.metas[2].weights,
                        [0.92108173, 1.3389949])


def test_weighting_two(cmtsource):
    dcon_two = construct_dcon_two()

    config = DefaultWeightConfig(
        normalize_by_energy=True, normalize_by_category=True,
        comp_weight={"Z": 1.0, "R": 1.0, "T": 1.0},
        love_dist_weight=1.0, pnl_dist_weight=1.0,
        rayleigh_dist_weight=1.0, azi_exp_idx=0.5)

    weight = Weight(cmtsource, dcon_two, config)
    weight.setup_weight()

    sumw = weight_sum(weight.metas)
    npt.assert_allclose(sumw, dcon_two.nwindows)
