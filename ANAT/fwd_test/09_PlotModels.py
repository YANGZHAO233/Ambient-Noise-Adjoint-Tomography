# plot models for horizontal direction
# plot region for GMT and colour bar can be defined by user

import os, sys
sys.path.append("..")
from __Parameters__ import WorkDir, ScriptsDir
from __Parameters__ import zone


########################################################################
M00_dir = WorkDir + '/parameter_files/DATA/tomo_files'
M00_utm = M00_dir + '/tomography_model.xyz'
M00_geo = M00_dir + '/tomography_model.xyz_geo'
fwd_test = WorkDir + '/fwd_test'
indir = fwd_test + '/vs_model'
mod_file = indir + '/vs_geo'
outdir = indir + '/vs_slices'
cptfile = ScriptsDir + '/model_plot/vel_zy.cpt'
topofile = ScriptsDir + '/model_plot/etopo2.grd'
utm2lonlat_dir = ScriptsDir + '/LonLatAndUtm'
Mesh_Par_file = WorkDir + '/parameter_files/DATA/meshfem3D_files/Mesh_Par_file'
########################################################################
is_generate_M00_lonlat = True
########################################################################
if os.path.exists(outdir):
    os.system('rm -rf %s' % outdir)
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
#plot region, can be defined by user
xs += 0.5
xe -= 0.5
ys += 0.5
ye -= 0.5
zs -= 20
print("the plot region is xmin=%.2f, xmax=%.2f, ymin=%.2f, ymax=%.2f, zmax=%.2f"
      % (xs, xe, ys, ye, zs))
plot_region = "-R%f/%f/%s/%f" %  (xs, xe, ys, ye)  #coordinate axis size for GMT
########################################################################
# colour bar scale
c10 = "3.00/3.70/0.05"  # 10 km depth
c20 = "3.30/3.85/0.05"  # 20 km depth
c30 = "3.40/4.05/0.05"  # 30 km depth
c40 = "3.75/4.65/0.05"  # 40 km depth
c50 = "3.90/4.65/0.05"  # 50 km depth
c60 = "3.95/4.60/0.05"  # 60 km depth
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

step = 'M00_check'
mod = 'M00'
with open(input_file, 'w') as myfile:
    myfile.write('bash %s/model_plot/plot_models.sh ' % ScriptsDir + M00_geo + ' ' + indir + ' ' + outdir + \
                 ' ' + step + ' ' + cptfile + ' '  + topofile + ' ' + mod + \
                 ' "' + plot_region + '" ' + c10 + ' ' + c20 + ' ' + c30 + ' ' + \
                 c40 + ' ' + c50 + ' ' + c60+'\n')

os.system('bash %s' % input_file)
os.system('rm -f %s' % input_file)