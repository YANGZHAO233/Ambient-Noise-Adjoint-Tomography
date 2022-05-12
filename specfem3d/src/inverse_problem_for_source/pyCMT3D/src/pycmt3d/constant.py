#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
All the constants used in pycmt3d

:copyright:
    Wenjie Lei (lei@princeton.edu), 2016
:license:
    GNU Lesser General Public License, version 3 (LGPLv3)
    (http://www.gnu.org/licenses/lgpl-3.0.en.html)
"""
from __future__ import absolute_import
import numpy as np

# DON'T change the order here unless you are 100% sure what you are
# doing here. All the array in the code is organized by this order.
PARLIST = ("Mrr", "Mtt", "Mpp", "Mrt", "Mrp", "Mtp",
           "dep", "lon", "lat", "ctm", "hdr")

# Scale of cmt parameters
# (latitude, longtitude, depth and moment centroid time and half duration)
SCALE_LONGITUDE = 0.001  # degree
SCALE_LATITUDE = 0.001   # degree
SCALE_DEPTH = 1000.0     # m, convert m to km
SCALE_MOMENT = 1.0e+22   # dyns*cm
SCALE_CTIME = 1.0        # seconds, please don't change
SCALE_HDUR = 1.0         # seconds, please don't change

# DEFAULT_SCALE_VECTOR in [Mrr, Mtt, Mpp, Mrt, Mrp, Mtp, Depth, Latitude,
#   Longitude, Time shift, Half Duration]
DEFAULT_SCALE_VECTOR = np.array(
    [SCALE_MOMENT, SCALE_MOMENT, SCALE_MOMENT, SCALE_MOMENT,
     SCALE_MOMENT, SCALE_MOMENT, SCALE_DEPTH, SCALE_LONGITUDE,
     SCALE_LATITUDE, 1.0, 1.0])

# Maximum number of parameters
NPARMAX = 11
# Number of pars for moment only
NM = 6
# number of pars for moment+location only
NML = 9

# Max step in non-linear solvoer
NMAX_NL_ITER = 60
