# get vp/vs/rho model from update_models for plotting

import os, sys, linecache, re
sys.path.append("..")
from __Parameters__ import WorkDir, ScriptsDir, ToolsDir
from __Parameters__ import mpi_version, zone, nproc
from glob import glob

#############################################################
src_rec = WorkDir + '/src_rec'
sources = src_rec + '/sources.dat'
fwd_test = WorkDir + '/fwd_test'
model_slice_dir = ToolsDir + '/model_slice'
model_slice_dir_mod = fwd_test + '/vs_model'
model_slice_dir_program = ToolsDir + '/model_slice/program'
Mesh_Par_file = WorkDir + '/parameter_files/DATA/meshfem3D_files/Mesh_Par_file'
lonlat2utm_dir = ScriptsDir + '/LonLatAndUtm'
#############################################################
dx, dy, dz = 0.05, 0.05, 10000                               # interval distance in x/degree y/degree z/m direction
max_points = 1000000                                        # number of max points in sem_model_slice.f90
is_compile_Sem_Model_Slice = True                           # compile it in first iteration
is_get_xyz = True                                           # get xyz coordinates for model plotting in first iteration
is_get_model = True                                         # get models from update_models
# nproc_job = 40
nproc_job = nproc
wall_time = '00:20'                                         # limit time for one job
job_mode = 'debug'                                          # computing mode for submitting jobs
                                                            # 'debug','short','medium','large' in SUSTECH Taiyi supercomputer
if os.path.exists(model_slice_dir_mod):
    os.system('rm -rf %s' % model_slice_dir_mod)
os.system('mkdir -p %s' % model_slice_dir_mod)

the_line = linecache.getline(sources, 1)  # get the line of first station
stnm, ntwk, evla, evlo = the_line.split()[0:4]  # station name, network name, longitude, latitude
evtid = ntwk + '.' + stnm
evtid_dir = fwd_test + '/' + evtid  # station directory

# ============ compile sem_model_slice ======================
flag_compile = False
if is_compile_Sem_Model_Slice:

    # get NSPEC_ADJOINT and NGLOB_ADJOINT in values_from_mesher.h of particular station
    values_from_mesher = evtid_dir + '/OUTPUT_FILES/values_from_mesher.h'
    for line in open(values_from_mesher, 'r'):
        if 'NSPEC_ADJOINT' in line and '=' in line:
            NSPEC_ADJOINT = int(line.split()[3])
        if 'NGLOB_ADJOINT' in line and '=' in line:
            NGLOB_ADJOINT = int(line.split()[3])

    # replace NSPEC_AB and NGLOB_AB by NSPEC_ADJOINT and NGLOB_ADJOINT in values_from_mesher.h of model slice program
    values_from_mesher_model_slice = model_slice_dir_program + '/values_from_mesher.h'
    content = open(values_from_mesher_model_slice, 'r').readlines()
    for i, line in enumerate(content):
        if 'NSPEC_AB' in line and '=' in line and 'NSPEC_ATTENUATION' not in line:
            content[i] = 'integer, parameter :: NSPEC_AB =         %d\n' % (NSPEC_ADJOINT)
        if 'NGLOB_AB' in line and '=' in line and 'NSPEC_ATTENUATION' not in line:
            content[i] = 'integer, parameter :: NGLOB_AB =         %d\n' % (NGLOB_ADJOINT)
    with open(values_from_mesher_model_slice, 'w') as myfile1:
        myfile1.writelines(content)

    # reset NMAXPTS in sem_model_slice.f90
    model_slice_code = model_slice_dir_program + '/sem_model_slice.f90'
    content2 = open(model_slice_code, 'r').readlines()
    for i, line in enumerate(content2):
        if i > 10:
            break
        if 'NMAXPTS' in line and '=' in line:
            content2[i] = '  integer, parameter :: NMAXPTS = %d\n' % (max_points)
    with open(model_slice_code, 'w') as myfile2:
        myfile2.writelines(content2)

    # compile sem_model_slice
    compile_file = model_slice_dir_program + '/compile.sh'
    model_slice_run = model_slice_dir_program + '/sem_model_slice'
    if os.path.exists(compile_file):
        os.system('rm -f %s' % compile_file)
    if os.path.exists(model_slice_run):
        os.system('rm -f %s' % model_slice_run)

    with open(compile_file, 'w') as myfile3:
        myfile3.write("#!/bin/bash\n\n")
        myfile3.write("cd %s\n\n" % model_slice_dir_program)
        myfile3.write("module load %s\n\n" % (mpi_version))
        myfile3.write("mpif90 -O3 -o sem_model_slice sem_model_slice.f90 exit_mpi.f90 "
                      "read_basin_topo_bathy_file.f90 utm_geo.f90")
    try:
        os.system('bash %s' % compile_file)
        flag_compile = True
    except:
        print('Error occurs when compiling sem_model_slice!'
              'Please check code!!!!!!')

# ============ get xyz coordinates ======================
flag_xyz = False
if is_compile_Sem_Model_Slice is True and flag_compile is not True:
    pass
else:
    if is_get_xyz:
        for line in open(Mesh_Par_file, 'r'):
            if 'LONGITUDE_MIN' in line and '=' in line:
                xs = float(line.split()[2].split('d')[0])
            if 'LONGITUDE_MAX' in line and '=' in line:
                xe = float(line.split()[2].split('d')[0])
            if 'LATITUDE_MIN' in line and '=' in line:
                ys = float(line.split()[2].split('d')[0])
            if 'LATITUDE_MAX' in line and '=' in line:
                ye = float(line.split()[2].split('d')[0])
            if 'DEPTH_BLOCK_KM' in line and '=' in line:
                zs = float(line.split()[2].split('d')[0]) * -1000
        ze = 0

        nx = int(abs(xe - xs) / dx) + 1
        ny = int(abs(ye - ys) / dy) + 1
        nz = int(abs(ze - zs) / dz) + 1

        print('the total number of grid points = %s' % (nx * ny * nz))

        if nx * ny * nz > max_points:
            print(
                'xyz.dat is not generated!!!\n'
                'The number of grid points (nx*ny*nz) must be smaller than max_points = %s '
                'in sem_model_slice program which can be changed.\n'
                'Please reset dx, dy and dz to decrease the nx*ny*nz or increase the value of max_points '
                'to be larger than nx*ny*nz and then recompile the sem_model_slice program\n'
                'I suggest the dx, dy is smaller than 0.1 degree' % max_points
                  )
        else:
            if os.path.exists('%s/xyz.dat' % model_slice_dir):
                os.system('rm -f %s/xyz.dat' % model_slice_dir)
            with open('%s/xyz.dat' % model_slice_dir, 'w') as myfile4:
                for iz in range(nz):
                    zz = zs + iz * dz
                    for iy in range(ny):
                        yy = ys + iy * dy
                        for ix in range(nx):
                            xx = xs + ix * dx
                            myfile4.write("%18.4f %18.4f %16.4f\n" % (xx, yy, zz))
            os.system('perl %s/convert_lonlat2utm_3.pl %s/xyz.dat %d > %s/%s'
                      % (lonlat2utm_dir, model_slice_dir, zone, model_slice_dir, 'xyz_utm.dat'))
            flag_xyz = True
            print('xyz_utm.dat is generated!')

# ============ get models ======================
if is_get_xyz is True and flag_xyz is not True:
    pass
else:
    if is_get_model:
        job_file = '%s/job02_GetModel.sh' % (fwd_test)
        if os.path.exists(job_file):
            os.system('rm -f %s' % (job_file))
        err_file = "job02_GetModel_err"
        out_file = "job02_GetModel_out"
        err_file_dir = fwd_test + '/' + err_file
        out_file_dir = fwd_test + '/' + out_file
        if os.path.exists(err_file_dir):
            os.system('rm -f %s' % (err_file_dir))
        if os.path.exists(out_file_dir):
            os.system('rm -f %s' % (out_file_dir))

        target = fwd_test + '/' + evtid + '/OUTPUT_FILES'
        with open(job_file, 'w') as myfile4:
            myfile4.write("#!/bin/bash\n\n")

            myfile4.write("#BSUB -J job02_GetModels\n")
            myfile4.write("#BSUB -q %s\n" % (job_mode))
            myfile4.write("#BSUB -n %d\n" % (nproc_job))
            myfile4.write("#BSUB -R \"span[ptile=40]\"\n")
            myfile4.write("#BSUB -W %s\n" % (wall_time))
            myfile4.write("#BSUB -e %s\n" % (err_file))
            myfile4.write("#BSUB -o %s\n\n" % (out_file))

            myfile4.write("module load %s\n\n" % (mpi_version))

            myfile4.write("cd %s\n" % model_slice_dir)

            myfile4.write("mpirun -np %s ./program/sem_model_slice xyz_utm.dat "
                          "%s/DATABASES_MPI %s/DATABASES_MPI vs vs_M00_check_utm\n\n"
                          % (nproc, target, target))

            myfile4.write("perl %s/convert_utm2lonlat_5.pl vs_M00_check_utm %d > vs_M00_check_geo\n\n"
                          % (lonlat2utm_dir, zone))

            myfile4.write("mv vs_M00_check_utm %s/\n" % (model_slice_dir_mod))
            myfile4.write("mv vs_M00_check_geo %s/" % (model_slice_dir_mod))

        os.system('bsub < %s' % (job_file))