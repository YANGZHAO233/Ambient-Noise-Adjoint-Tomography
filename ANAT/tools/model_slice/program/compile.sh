#!/bin/bash

cd /work/ose-zhaoy/packageForANAT/ANAT/tools/model_slice/program

module load mpi/openmpi/3.1.2_gcc

mpif90 -O3 -o sem_model_slice sem_model_slice.f90 exit_mpi.f90 read_basin_topo_bathy_file.f90 utm_geo.f90