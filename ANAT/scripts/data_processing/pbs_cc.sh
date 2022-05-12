#!/bin/bash

#PBS -N XIMA_CC
#PBS -l nodes=6:ppn=24
#PBS -l walltime=10:00:00
#PBS -q cal-s
#PBS -S /bin/bash
#PBS -V

# script runs mesher,database generation and solver
# using this example setup
#
#prog=~/specfem3d
###################################################
#module load intel/15.0.2 openmpi/intel/1.10.6
#module load /opt/software/openmpi-1.10.6-intel/bin/mpirun
#=====
cd $PBS_O_WORKDIR
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/intel/lib/intel64:/usr/local/openupi/lib
#=====
./Run.py 144





























































































































