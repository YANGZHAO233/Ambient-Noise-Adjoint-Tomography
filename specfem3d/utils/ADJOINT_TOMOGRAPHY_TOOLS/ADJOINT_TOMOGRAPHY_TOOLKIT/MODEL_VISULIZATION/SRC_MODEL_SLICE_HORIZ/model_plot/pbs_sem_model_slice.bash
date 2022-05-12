#!/bin/bash

#PBS -N SLICE.M02
#PBS -l nodes=4:ppn=24
#PBS -l walltime=01:00:00
#PBS -q cal
#PBS -S /bin/bash
#PBS -V

# script runs mesher,database generation and solver
# using this example setup
#
###################################################
#=====
#module load intel/15.0.2 openmpi/intel/1.6.4
cd $PBS_O_WORKDIR
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/intel/lib/intel64:/usr/local/openmpi/lib
#=====
# number of processes
NPROC=81

prog=/home/ose-guoz/tengchong/full_adjoint/plots/model_plot/model_slice/sem_model_slice


#model_dir=model_dlnm_M21-M16_narrowband
#model_dir=../../optimize/SD_M16/OUTPUT_MODEL_slen0.12
#model_dir=../../optimize/SD_M18/OUTPUT_MODEL_slen0.03
model_dir=../../optimize/SD_M01/OUTPUT_MODEL_slen0.04

topo_dir=../../specfem3d/OUTPUT_FILES/DATABASES_MPI
# xyz_infile topo_dir model_dir data_name gmt_outfile
# Note: set the right NSPEC_AB when compile the code
#mpirun -np $NPROC $prog xyz.dat $topo_dir $model_dir beta_kernel_smooth raydensity.sm.${newmod}.regridded.xyz 
#mpirun -np $NPROC $prog xyz.dat $topo_dir $model_dir beta_kernel grad.prec.M21.regridded.xyz 
mpirun -machinefile $PBS_NODEFILE -np $NPROC $prog xyz.dat $topo_dir $model_dir vs  model.M02.narrowband.regridded.xyz 
#mpirun -np $NPROC $prog xyz.dat $topo_dir $model_dir vs  model.M19.narrowband.regridded.xyz 
