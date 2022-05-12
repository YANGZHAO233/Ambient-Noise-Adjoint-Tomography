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
import pytest
from obspy import Trace
import pycmt3d.util as util
import numpy.testing as npt


# Most generic way to get the data folder path.
DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(
        inspect.getfile(inspect.currentframe()))), "data")
OBSD_DIR = os.path.join(DATA_DIR, "data_T006_T030")
SYNT_DIR = os.path.join(DATA_DIR, "syn_T006_T030")


def test_distance():
    assert util.distance(0, 0, 10, 0) == 10
    assert util.distance(0, 0, 0, 10) == 10


def normalize_array():
    array = np.array([1.0, 2])
    npt.assert_allclose(util.normalize_array(array, 0.5),
                        [0.5, 1.0])


def test_get_window_idx():
    win_time = [10, 20]
    dt = 0.5
    result = util.get_window_idx(win_time, dt)
    assert result.dtype == np.int64
    npt.assert_allclose(result, [20, 40])

    win_time = [-10, 20]
    dt = 0.5
    with pytest.raises(ValueError) as excinfo:
        util.get_window_idx(win_time, dt)
    assert 'smaller than 0' in str(excinfo.value)

    win_time = [30, 20]
    dt = 0.5
    with pytest.raises(ValueError) as excinfo:
        util.get_window_idx(win_time, dt)
    assert 'larger or equal than' in str(excinfo.value)


def test_check_trace_consistent():
    tr1 = Trace()
    tr1.data = np.zeros(10)
    tr2 = Trace()
    tr1.data = np.zeros(10)
    assert util.check_trace_consistent(tr1, tr2) is None

    tr2.data = np.zeros(20)
    assert util.check_trace_consistent(tr1, tr2) is None
    with pytest.raises(ValueError):
        util.check_trace_consistent(tr1, tr2, mode="full")

    tr2.stats.delta = 2 * tr1.stats.delta
    with pytest.raises(ValueError):
        util.check_trace_consistent(tr1, tr2, mode="")

    tr2.stats.delta = tr1.stats.delta
    tr2.stats.starttime = tr1.stats.starttime + 1
    with pytest.raises(ValueError):
        util.check_trace_consistent(tr1, tr2, mode="test")


def test_sum_matrix():
    mat = np.ones([3, 3])
    data = [mat, 2 * mat.copy(), 3 * mat.copy()]
    npt.assert_allclose(util.sum_matrix(data), 6 * mat)

    coef = [3, 2, 1]
    npt.assert_allclose(util.sum_matrix(data, coef=coef), 10 * mat)


def test_random_select():
    subset_array = util.random_select(100, 30)
    assert np.sum(subset_array) == 30


def test_float_to_str():
    assert util._float_to_str(5.1) == "5.10000"


def test_float_array_to_str():
    answer = "[  1.000e+00,1.000e+01,]"
    assert util._float_array_to_str([1.0, 10.0]) == answer


def test_construct_taper_boxcar():
    npts = 21
    taper = util.construct_taper(npts, taper_type="boxcar")
    npt.assert_allclose(taper, np.ones(21))


def test_construct_taper_hann():
    npts = 21
    taper = util.construct_taper(npts, taper_type="hann")
    assert len(taper) == npts
    assert taper[0] == 0.0 and taper[20] == 0.0
    assert taper[10] == 1.0


def test_construct_taper_tukey():
    npts = 21
    result = np.ones(npts)
    result[0] = 0.0
    result[1] = 0.5
    result[npts-1] = 0.0
    result[npts-2] = 0.5
    taper = util.construct_taper(npts, taper_type="tukey", alpha=0.2)
    npt.assert_allclose(taper, result)
