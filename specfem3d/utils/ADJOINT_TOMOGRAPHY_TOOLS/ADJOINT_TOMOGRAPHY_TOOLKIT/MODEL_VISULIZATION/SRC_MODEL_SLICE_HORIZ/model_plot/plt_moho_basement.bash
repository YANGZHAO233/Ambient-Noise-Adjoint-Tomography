#!/bin/bash

gmtset HEADER_FONT_SIZE 24p
gmtset BASEMAP_TYPE plain 
indir=../../specfem3d/DATA/meshfem3D_files

PS=Moho_basement.ps
width=2.2
PROJ=M${width}i
tick=a2f1/a1
lonmin=-121.6
lonmax=-114.7
latmin=32.2
latmax=36.8
RMAP=$lonmin/$lonmax/$latmin/$latmax
CPT=v.cpt

cp $indir/moho.geo slow.slice 
blockmedian slow.slice -R$RMAP -I0.1 >slow.slice.m
surface slow.slice.m -R$RMAP -I0.1 -Gslice.grd
grdgradient slice.grd -Gslice.grad -A315 -Nt
#triangulate slow.slice -R$RMAP -I0.25/0.25 -Gslice.grd -E > tria.out
psbasemap -R$RMAP -J$PROJ -B"$tick":."Moho variation":WenN -X1.5 -Y18 -K -P > $PS
makecpt -Cseis -T21/37/0.1 -D > $CPT
grdimage slice.grd -J$PROJ -R$RMAP -C$CPT -B"$tick"::wens -O -K >> $PS
psxy jennings.xy -J -R -m -W0.5p,0/0/0 -O -K >> $PS
pscoast -J -R  -O -K -W1p,black -Na/1p,black -Dh >> $PS
pstext -R -J -O -K -N <<EOF >> $PS
-121 32.6 12 0 1 ML Moho
EOF
psscale -D1.1i/-1/2.2i/0.3h -C$CPT -Ba4f1:"Depth (km)": -E -O -N -K >> $PS
###
cp $indir/base.geo slow.slice 
#blockmedian slow.slice -R$RMAP -I0.01 >slow.slice.m
#surface slow.slice.m -R$RMAP -I0.01 -Gslice.grd
triangulate slow.slice -R-119.00/-117.257/33.030/34.325 -I0.01 -Gslice.grd -E > tria.out
psbasemap -R$RMAP -J$PROJ -B"$tick":."basement variation":WenN -X8 -O -K -P >> $PS
makecpt -Cseis -T5000/8000/100 -D > $CPT
grdimage slice.grd -J$PROJ -R$RMAP -C$CPT -B"$tick"::wens -O -K >> $PS
psxy jennings.xy -J -R -m -W0.5p,0/0/0 -O -K >> $PS
psxy -J -R -W1p,0/0/0 -O -K >> $PS <<eof
-119.00 33.020
-117.257 33.020
-117.257 34.325
-119.00 34.325
-119.00 33.020
eof
pscoast -J -R  -O -K -W1p,black -Na/1p,black -Dh >> $PS
pstext -R -J -O -K -N <<EOF >> $PS
-121 32.6 12 0 1 ML Basement
EOF
psscale -D1.1i/-1/2.2i/0.3h -C$CPT -Ba1000:"Depth (m)": -E -O -N >> $PS

