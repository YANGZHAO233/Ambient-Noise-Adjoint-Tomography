#!/bin/bash

############################
cptfile="vel_zy.cpt"
outdir="dem_nc_files"

############################
rm -rf $outdir
mkdir $outdir

############################
ele1="0.3"
ele2="3.0"
filename="dem3.nc"
range="100.5/102.3/25.5/27.3"
R="-R$range"
R2="-R$range/${ele1}/${ele2}"
J="-JM4.64i"
Jz="-JZ0.5i"
X="-K -O"
PS="plot3dTopo.ps"

############################
gmt gmtset MAP_FRAME_TYPE plain
gmt gmtset FORMAT_GEO_MAP ddd.x
gmt gmtset FONT_ANNOT_PRIMARY +10p
gmt gmtset FONT_LABEL +12p

############################
#gmt grdcut etopo1.nc $R -G${outdir}/dem1.nc
#gmt grdcut e100n90.nc $R -G${outdir}/dem2.nc
gmt grdcut ../model_plot/etopo2.grd $R -G${outdir}/dem3.nc

############################
gmt psxy $R $J -K -T -X0.8i -Y7.5i -P > $PS

############################
cp $outdir/$filename ./junk.nc
gmt grdmath junk.nc 1000 DIV = data.nc
gmt grd2cpt data.nc -Crainbow -Z > color.cpt         # -Crainbow
gmt grdgradient data.nc -Nt0.75 -A0 -fg -Gcut_i.nc

############################
# gmt grdview data.nc $R2 $J $Jz $X -Icut_i.nc -Ccolor.cpt -Qi300 -N${ele1}+g150/150/150\
#  -p-150/25 -BWSenZ -Bxa -Bya -Bza+l"Elevation (km)" >> $PS

############################
gmt grdfilter data.nc -D4 -Fb3 -Gdata_filter.nc        # -m: median; -g: gaussian
gmt grdview data_filter.nc $R2 $J $Jz $X -Icut_i.nc -Ccolor.cpt -Qi300 -N${ele1}+g150/150/150 -p205/12 -BWSenZ -Bza1+l"Elevation (km)" -Y-3.5i >> $PS

#############################
gmt psxy $R $J -O -T >> $PS

#############################
gmt psconvert $PS -E300 -A -Tj -P
gmt psconvert $PS -E300 -A -Tf -P

#############################
rm -rf color.cpt gmt.* *.nc
eog plot3dTopo.jpg
