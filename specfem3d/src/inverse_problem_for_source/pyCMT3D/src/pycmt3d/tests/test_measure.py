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
import pycmt3d.measure as meas
import numpy.testing as npt


# Most generic way to get the data folder path.
DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(
        inspect.getfile(inspect.currentframe()))), "data")
OBSD_DIR = os.path.join(DATA_DIR, "data_T006_T030")
SYNT_DIR = os.path.join(DATA_DIR, "syn_T006_T030")


def test_xcorr_win_():
    arr1 = np.array([0., 0, 0, 1, 2, 1, 0, 0, 0, 0])
    arr2 = np.array([0., 0, 0, 0, 1, 2, 1, 0, 0, 0])
    cc, nshift = meas._xcorr_win_(arr1, arr2)
    assert cc == 1.0
    assert nshift == -1


def test_dlnA_win_():
    arr1 = np.array([0., 0, 0, 1, 2, 1, 0, 0, 0, 0])
    arr2 = np.array([0., 0, 0, 0, 1, 2, 1, 0, 0, 0])
    dlnA = meas._power_l2_win_(arr1, arr2)
    assert dlnA == 0.0

    arr1 = np.array([0., 0, 0, 1, 2, 1, 0, 0, 0, 0])
    arr2 = np.array([0., 0, 0, 0, 0.5, 1, 0.5, 0, 0, 0])
    dlnA = meas._power_l2_win_(arr1, arr2)
    assert dlnA == 10 * np.log10(4)


def test_cc_amp_():
    arr1 = np.array([0., 0, 0, 1, 2, 1, 0, 0, 0, 0])
    arr2 = np.array([0., 0, 0, 1, 2, 1, 0, 0, 0, 0])
    cc_amp = meas._cc_amp_(arr1, arr2)
    assert cc_amp == 0.0

    arr1 = np.array([0., 0, 0, 1, 2, 1, 0, 0, 0, 0])
    arr2 = np.array([0., 0, 0, 0.5, 1, 0.5, 0, 0, 0, 0])
    cc_amp = meas._cc_amp_(arr1, arr2)
    assert cc_amp == 10 * np.log10(2)


def test_correct_window_index():
    arr1 = np.array([0., 0, 0, 0, 1, 2, 1, 0, 0, 0])
    arr2 = np.array([0., 0, 0, 0.5, 1, 0.5, 0, 0, 0, 0])

    with pytest.raises(ValueError) as excinfo:
        meas.correct_window_index(arr1, arr2, 0, len(arr1))
        assert 'After correction' in excinfo

    istart_d, iend_d, istart_s, iend_s, max_cc, nshift = \
        meas.correct_window_index(arr1, arr2, 2, 7)
    assert istart_d == 3 and iend_d == 8
    assert istart_s == 2 and iend_s == 7
    assert nshift == 1
    assert max_cc == 1.0


def test_measure_window():
    arr1 = np.array([0., 0, 0, 0, 0, 1, 2, 1, 0, 0, 0, 0, 0, 0])
    arr2 = np.array([0., 0, 0, 0, 0, 0, 0.5, 1, 0.5, 0, 0, 0, 0, 0])

    nshift, max_cc, power_l1, power_l2, cc_amp = \
        meas.measure_window(arr1, arr2, 2, 10, station_correction=False)
    assert nshift == -1 and max_cc == 1.0
    assert power_l2 == 10 * np.log10(4)
    assert power_l1 == 10 * np.log10(2)
    assert cc_amp == 10 * np.log10(4/3)

    nshift, max_cc, power_l1, power_l2, cc_amp = \
        meas.measure_window(arr1, arr2, 2, 10, station_correction=True)
    assert nshift == -1 and max_cc == 1.0
    assert power_l2 == 10 * np.log10(4)
    assert power_l1 == 10 * np.log10(2)
    assert cc_amp == 10 * np.log10(2)


def generate_datalist():
    datalist = {}
    datalist["obsd"] = Trace(np.array([0., 0, 0, 0, 1, 0, 0, 0]))
    datalist["synt"] = Trace(np.array([0., 0, 0, 1, 0, 0, 0, 0]))
    datalist["Mrr"] = Trace(np.array([0., 0, 0, 1, 0, 0, 0, 0]))
    datalist["Mtt"] = Trace(np.array([0., 0, 0, 2, 0, 0, 0, 0]))
    datalist["Mpp"] = Trace(np.array([0., 0, 0, 3, 0, 0, 0, 0]))
    datalist["Mrt"] = Trace(np.array([0., 0, 0, 4, 0, 0, 0, 0]))
    datalist["Mrp"] = Trace(np.array([0., 0, 0, 5, 0, 0, 0, 0]))
    datalist["Mtp"] = Trace(np.array([0., 0, 0, 6, 0, 0, 0, 0]))
    datalist["dep"] = Trace(np.array([0., 0, 0, 1, 1, 0, 0, 0]))
    datalist["lon"] = Trace(np.array([0., 0, 0, 1, 2, 0, 0, 0]))
    datalist["lat"] = Trace(np.array([0., 0, 0, 1, 3, 0, 0, 0]))
    return datalist


def test_calculate_dsyn():
    parlist = ['Mrr', 'Mtt', 'Mpp', 'Mrt', 'Mrp', 'Mtp', 'dep', 'lon', 'lat']
    datalist = generate_datalist()
    win_idx = [1, 6]
    dcmt_par = np.ones(len(parlist))
    dsyn = meas.calculate_dsyn(datalist, win_idx, parlist, dcmt_par)
    for idx in range(6):
        _arr = np.zeros(5)
        _arr[2] = idx + 1
        npt.assert_allclose(dsyn[idx], _arr)

    for idx in range(6, 9):
        _arr = np.zeros(5)
        _arr[3] = idx - 5
        npt.assert_allclose(dsyn[idx], _arr)


def test_compute_A_b():
    parlist = ['Mrr', 'Mtt', 'Mpp', 'Mrt', 'Mrp', 'Mtp', 'dep', 'lon', 'lat']
    datalist = generate_datalist()
    win_time = [1, 6]
    dcmt_par = np.ones(len(parlist))

    A, b, Ae, be = meas.compute_derivatives(datalist, win_time, parlist,
                                            dcmt_par)

    A_true = np.zeros([9, 9])
    for ii in range(0, 6):
        for jj in range(0, 6):
            A_true[ii, jj] = (ii + 1) * (jj + 1)
    for ii in range(6, 9):
        for jj in range(6, 9):
            A_true[ii, jj] = (ii - 5) * (jj - 5)

    npt.assert_allclose(A, A_true)


def test_calculate_variance_on_trace():
    obsd = Trace(np.zeros(100))
    obsd.data[22:25] = [1, 2, 1]
    obsd.data[56:61] = [1, 3, 5, 3, 1]

    synt = Trace(np.zeros(100))
    synt.data[24:27] = [0.5, 1, 0.5]
    synt.data[52:57] = [1.5, 4.5, 7.5, 4.5, 1.5]

    win_time = [[20, 30], [50, 65]]

    measure = \
        meas.calculate_variance_on_trace(obsd, synt, win_time)

    npt.assert_allclose(measure["v"], [1.5, 11.25])
    npt.assert_allclose(measure["d"], [6.0, 45])
    npt.assert_allclose(measure["tshift"], [-2, 4])
    npt.assert_allclose(measure["cc"], [1.0, 1.0])
    npt.assert_allclose(measure["power_l2"],
                        [20*np.log10(2), 20*np.log10(2/3.0)])
    npt.assert_allclose(measure["power_l1"],
                        [10*np.log10(2), 10*np.log10(2/3.0)])
    npt.assert_allclose(measure["cc_amp"],
                        [10*np.log10(2), 10*np.log10(2/3.0)])
