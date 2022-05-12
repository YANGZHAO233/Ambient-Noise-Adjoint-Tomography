from __future__ import (absolute_import, print_function, division)
import logging

# setup the logger
logger = logging.getLogger("pycmt3d")
logger.setLevel(logging.INFO)
logger.propagate = 0

ch = logging.StreamHandler()
# Add formatter
FORMAT = "%(name)s - %(levelname)s: %(message)s"
formatter = logging.Formatter(FORMAT)
ch.setFormatter(formatter)
logger.addHandler(ch)

from .source import CMTSource  # NOQA
from .data_container import DataContainer  # NOQA
from .config import WeightConfig, DefaultWeightConfig, Config  # NOQA
from .cmt3d import Cmt3D  # NOQA
from .grid3d import Grid3dConfig, Grid3d  # NOQA
