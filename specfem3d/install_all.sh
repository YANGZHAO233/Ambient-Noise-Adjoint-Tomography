#!/bin/bash

make clean

# intel
#module load mpi/openmpi/3.1.2_intel
#./configure FC=ifort CC=icc MPIFC=mpif90 --with-mpi

# g
module load mpi/openmpi/3.1.2_gcc
./configure FC=gfortran CC=gcc MPIFC=mpif90 --with-mpi

make all
