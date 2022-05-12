#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
The only purpose of this file is to be able to run the pyflex test suite with

python -m pyflex.tests

:copyright:
    Lion Krischer (krischer@geophysik.uni-muenchen.de), 2014
:license:
    GNU General Public License, Version 3
    (http://www.gnu.org/copyleft/gpl.html)
"""
if __name__ == "__main__":
    import inspect
    import os
    import pytest
    import sys
    PATH = os.path.dirname(os.path.dirname(
        os.path.abspath(inspect.getfile(inspect.currentframe()))))

    sys.exit(pytest.main(PATH))
