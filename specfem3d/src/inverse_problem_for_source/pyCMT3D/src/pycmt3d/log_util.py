#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
log util

:copyright:
    Wenjie Lei (lei@princeton.edu), 2016
:license:
    GNU Lesser General Public License, version 3 (LGPLv3)
    (http://www.gnu.org/licenses/lgpl-3.0.en.html)
"""

from __future__ import (print_function, division, absolute_import)
import numpy as np
from . import logger
from .util import get_cmt_par


def inversion_result_table(npar, cmtsource, new_cmtsource,
                           bootstrap_flag=False, bootstrap_mean=None,
                           bootstrap_std=None,
                           bootstrap_std_over_mean=None):
    """
    Print out the inversion table

    :return:
    """
    if npar < 6 or npar > 11:
        raise ValueError("npar(%d) should be within [6, 11]")

    title = "*" * 20 + " Inversion Result Table(%d npar) " % \
        npar + "*" * 20
    logger.info(title)

    mattrs = ["m_rr", "m_tt", "m_pp", "m_rt", "m_rp", "m_tp"]
    lattrs = ["depth_in_m", "longitude", "latitude", "time_shift",
              "half_duration"]
    lattr_names = {"depth_in_m": "dep", "longitude": "lon",
                   "latitude": "lat", "time_shift": "tshift",
                   "half_duration": "hdr"}

    if not bootstrap_flag:
        logger.info("PAR         Old_CMT        New_CMT")
        for attr in mattrs:
            logger.info("%s:  %15.6e  %15.6e" % (
                attr, getattr(cmtsource, attr), getattr(new_cmtsource, attr)))

        for idx in range(npar - 6):
            attr = lattrs[idx]
            logger.info("%s:  %15.3f  %15.3f" % (
                lattr_names[attr], getattr(cmtsource, attr),
                getattr(new_cmtsource, attr)))
    else:
        logger.info("PAR         Old_CMT          New_CMT     "
                    "Bootstrap_Mean     Bootstrap_STD     STD/Mean")

        for idx, attr in enumerate(mattrs):
            logger.info(
                "%s:  %15.6e  %15.6e  %15.6e  %15.6e   %10.2f%%" % (
                    attr, getattr(cmtsource, attr),
                    getattr(new_cmtsource, attr),
                    bootstrap_mean[idx], bootstrap_std[idx],
                    bootstrap_std_over_mean[idx] * 100))

        for idx in range(npar - 6):
            attr = lattrs[idx]
            logger.info("%s:  %15.3f  %15.3f  %15.3f  %15.3f   %10.2f%%" % (
                lattr_names[attr],
                getattr(cmtsource, attr),
                getattr(new_cmtsource, attr),
                bootstrap_mean[idx + 6],
                bootstrap_std[idx + 6],
                bootstrap_std_over_mean[idx + 6]))


def print_inversion_summary(npar, cmtsource, new_cmtsource,
                            bootstrap=False, bmean=None, bstd=None,
                            bstd_over_mean=None):
    """
    Print out the inversion summary

    :return:
    """
    logger.info("*" * 20)
    logger.info("Invert cmt parameters(%d par)" % npar)

    cmt_par = get_cmt_par(cmtsource)[:npar]
    new_cmt_par = get_cmt_par(new_cmtsource)[:npar]
    logger.info("Old CMT par: [%s]" % (
        ', '.join(map(str, cmt_par))))
    logger.info("dm: [%s]" % (
        ', '.join(map(str, new_cmt_par - cmt_par))))
    logger.info("New CMT par: [%s]" % (
        ', '.join(map(str, new_cmt_par))))

    logger.info("Trace: %e" % (np.sum(new_cmt_par[0:3])))
    logger.info("Energy change(scalar moment): %5.2f%%" % (
        (new_cmtsource.M0 - cmtsource.M0) /
        cmtsource.M0 * 100.0))

    inversion_result_table(
        npar, cmtsource, new_cmtsource, bootstrap_flag=bootstrap,
        bootstrap_mean=bmean, bootstrap_std=bstd,
        bootstrap_std_over_mean=bstd_over_mean)
