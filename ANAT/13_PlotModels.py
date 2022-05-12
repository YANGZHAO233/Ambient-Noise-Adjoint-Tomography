# plot models for horizontal and vertical direction

import os
from __Parameters__ import WorkDir, ProcDir, ScriptsDir, ResultsDir
from __Parameters__ import mod, zone, nproc


########################################################################
M00_dir = WorkDir + '/parameter_files/DATA/tomo_files'
M00_utm = M00_dir + '/tomography_model.xyz'
M00_geo = M00_dir + '/tomography_model.xyz_geo'
indir = ResultsDir + '/' + mod + '/vs_data'
outdir1 = ResultsDir + '/' + mod + '/slice_images'
outdir2 = ResultsDir + '/' + mod + '/profile_images'
cptfile = ScriptsDir + '/model_plot/vel_zy.cpt'
topofile = ScriptsDir + '/model_plot/etopo2.grd'
utm2lonlat_dir = ScriptsDir + '/LonLatAndUtm'
Mesh_Par_file = WorkDir + '/parameter_files/DATA/meshfem3D_files/Mesh_Par_file'
#cutfile = ScriptsDir + '/20_path.dat'                    # cut edge file
                                                          # can be generated follow the distribution of sensetive kernels
########################################################################
is_generate_M00_lonlat = True
is_plot_horizontal = True
is_plot_vertical = True
########################################################################
if os.path.exists(outdir1):
    os.system('rm -rf %s' % outdir1)
if os.path.exists(outdir2):
    os.system('rm -rf %s' % outdir2)
########################################################################
#simulation region
xs, xe, ys, ye, zs = None, None, None, None, None
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
        zs = float(line.split()[2].split('d')[0])
print("the simulation region is xmin=%.2f, xmax=%.2f, ymin=%.2f, ymax=%.2f, zmax=%.2f"
      % (xs, xe, ys, ye, zs))

# plot region, can be defined by user
xs += 0.5
xe -= 0.5
ys += 0.5
ye -= 1.5
zs -= 20
#

print("the plot region is xmin=%.2f, xmax=%.2f, ymin=%.2f, ymax=%.2f, zmax=%.2f"
      % (xs, xe, ys, ye, zs))
plot_region_horizontal = "-R%f/%f/%s/%f" %  (xs, xe, ys, ye)  #coordinate axis size for GMT
# colour bar scale
c10 = "3.40/3.75/0.05"  # 10 km depth
c20 = "3.30/3.80/0.05"  # 20 km depth
c30 = "3.30/3.75/0.05"  # 30 km depth
c40 = "3.50/4.30/0.05"  # 40 km depth
c50 = "3.60/4.40/0.05"  # 50 km depth
c60 = "3.70/4.50/0.05"  # 60 km depth

# profile region (if is_plot_vertical = True)
depth = 60
# cross_sections = ["A_A'", "B_B'", "C_C'", "D_D'"]
# cross_sections_lon = ["110.5_114.5", "110.5_114.5", "110.5_114.5", "113.0_113.0"]
# cross_sections_lat = ["38.0_41.0", "41.0_38.0", "40.0_40.0", "38.0_41.0"]
cross_sections = ["A_A'", "B_B'"]
cross_sections_lon = ["100.74_103.06", "99.6_104.28"]
cross_sections_lat = ["27.73_24.51", "26.38_26.38"]

# ================ generate M00 with lon-lat coordinate ================
if is_generate_M00_lonlat:
    os.system("tail -n +4 %s > %s/temp" % (M00_utm, M00_dir))
                                                          # delete the head of tomography_model.xyz
    os.system("perl %s/convert_utm2lonlat_5.pl %s/temp %d > %s"
              % (utm2lonlat_dir, M00_dir, zone, M00_geo))
    os.system("rm -f %s/temp" % M00_dir)

# =================== generate input.lst for plotting ===================
input_file = indir + '/input.lst'
if os.path.exists(input_file):
    os.system('rm -f %s' % input_file)

with open(input_file, 'w') as myfile:
    for item in sorted(os.listdir(indir)):
        if item != 'input.lst':
            step = item.split('_')[1]
            if is_plot_horizontal:
                tmpdir1 = outdir1 + '/' + step
                myfile.write('bash %s/model_plot/plot_models.sh ' % ScriptsDir + M00_geo + ' ' + indir + ' ' + tmpdir1 + \
                             ' ' + step + ' ' + cptfile + ' ' + topofile + ' ' + mod + \
                             ' "' + plot_region_horizontal + '" ' + c10 + ' ' + c20 + ' ' + c30 + ' ' + \
                             c40 + ' ' + c50 + ' ' + c60 + '\n')

            if is_plot_vertical:
                tmpdir2 = outdir2 + '/' + step
                for i in range(len(cross_sections)):
                    myfile.write('bash %s/model_plot/plot_profiles.sh ' % ScriptsDir + indir + ' ' + tmpdir2 + \
                                ' ' + step + ' ' + topofile + ' ' + mod + ' ' +  plot_region_horizontal + \
                                ' ' + '"' + cross_sections[i] + '"'+ ' ' + cross_sections_lon[i] + \
                                ' ' + cross_sections_lat[i] + ' ' + str(depth) + '\n')

os.system('bash %s' % input_file)
os.system('rm -f %s' % input_file)
print('Figures saved in %s and %s' % (outdir1, outdir2))