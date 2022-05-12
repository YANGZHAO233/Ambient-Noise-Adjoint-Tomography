#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
pycmt3d - a Python package for 3-dimenional centroid moment inversion

:copyright:
    Wenjie Lei (lei@Princeton.EDU), 2015
:license:
    GNU Lesser General Public License, Version 3
    (http://www.gnu.org/copyleft/lgpl.html)
'''
# Importing setuptools monkeypatches some of distutils commands so things like
# 'python setup.py develop' work. Wrap in try/except so it is not an actual
# dependency. Inplace installation with pip works also without importing
# setuptools.

import os
import glob
import sys
from setuptools import setup
from setuptools import find_packages
from setuptools.command.test import test as TestCommand


class PyTest(TestCommand):
    user_options = [('pytest-args=', 'a', "Arguments to pass to py.test")]

    def initialize_options(self):
        TestCommand.initialize_options(self)
        self.pytest_args = []

    def run_tests(self):
        import pytest
        errno = pytest.main(self.pytest_args)
        sys.exit(errno)


# Utility function to read the README file.
# Used for the long_description.  It's nice, because now 1) we have a top level
# README file and 2) it's easier to type in the README file than to put a raw
# string in below ...
def read(fname):
    try:
        return open(os.path.join(os.path.dirname(__file__), fname)).read()
    except Exception as e:
        return "Can't open %s" % fname


long_description = """
Source code: https://github.com/wjlei1990/pycmt3d
Documentation: http://wjlei1990.github.io/pycmt3d/

%s""".strip() % read("README.md")

setup(
    name='pycmt3d',
    version='0.1.0',
    license='GNU Lesser General Public License, Version 3',
    description='a python port of cmt3d softward',
    long_description=long_description,
    author='Wenjie Lei, Xin Song',
    author_email='lei@Princeton.EDU, songxin@physics.utoronto.ca',
    url='https://github.com/wjlei1990/pycmt3d',
    packages=find_packages("src"),
    package_dir={"": "src"},
    tests_require=['pytest'],
    cmdclass={'test': PyTest},
    include_package_data=True,
    zip_safe=False,
    classifiers=[
        'Development Status :: 3 - Alpha',
        'Intended Audience :: Science/Research',
        'Intended Audience :: Developers',
        'Operating System :: Unix',
        'Operating System :: POSIX',
        'Topic :: Scientific/Engineering',
        'Topic :: Scientific/Engineering :: Physics',
        'License :: OSI Approved :: GNU General Public License v3 (GPLv3)',
        'Programming Language :: Python :: 2.6',
        'Programming Language :: Python :: 2.7',
    ],
    keywords=['seismology', 'cmt3d', 'moment tensor',
              'centroid moment inversion'],
    install_requires=[
        "obspy>=1.0.0", "numpy", "future>=0.14.1", "flake8", "geographiclib",
        "scipy", "spaceweight"
    ],
    extras_require={
        "docs": ["sphinx"]
    }
)
