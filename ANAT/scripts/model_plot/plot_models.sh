#!/bin/bash

##################################
m00=${1}
indir=${2}
outdir=${3}
step=${4}
cptfile=${5}
topofile=${6}
mod=${7}
R=${8}
c10=${9}
c20=${10}
c30=${11}
c40=${12}
c50=${13}
c60=${14}
#cutfile=${15}

##################################
pathcut="1000"

##################################
infile=${indir}/vs_${step}_geo # input current model

##################################
rm -rf $outdir
mkdir -p $outdir
cd $outdir

##################################
##################################
#R="-R99/105/24/29"
J="-JM4i"
I="-I0.05"
X="-K -O"

#############################
gmt gmtset MAP_FRAME_TYPE plain
gmt gmtset FONT_LABEL +14p
gmt gmtset FONT_ANNOT_PRIMARY +10p

#############################
gmt grdcut $topofile $R -Gcut.nc
gmt grdgradient cut.nc -Ne0.8 -A100 -fg -Gcut_i.nc
#gmt makecpt -T2.55/3.35/0.05 -C$cptfile -I -D -Z > color_02km.cpt
#gmt makecpt -T2.85/3.60/0.05 -C$cptfile -I -D -Z > color_04km.cpt
#gmt makecpt -T2.95/3.70/0.05 -C$cptfile -I -D -Z > color_05km.cpt
gmt makecpt -T$c10 -C$cptfile -I -D -Z > color_10km.cpt
#gmt makecpt -T3.15/3.83/0.05 -C$cptfile -I -D -Z > color_15km.cpt
gmt makecpt -T$c20 -C$cptfile -I -D -Z > color_20km.cpt
#gmt makecpt -T3.35/3.90/0.05 -C$cptfile -I -D -Z > color_25km.cpt
gmt makecpt -T$c30 -C$cptfile -I -D -Z > color_30km.cpt
#gmt makecpt -T3.55/4.40/0.05 -C$cptfile -I -D -Z > color_35km.cpt
gmt makecpt -T$c40 -C$cptfile -I -D -Z > color_40km.cpt
gmt makecpt -T$c50 -C$cptfile -I -D -Z > color_50km.cpt
gmt makecpt -T$c60 -C$cptfile -I -D -Z > color_60km.cpt
#gmt blockmean $R $I ${cutfile} | gmt surface $R $I -Gjunk.nc
#gmt grdclip junk.nc -Gclip_mask.nc -Sb${pathcut}/NaN -Sa${pathcut}/1 -Sr${pathcut}/1

#############################
echo "Step: ${step}"
for depth in 10 20 30 40 50 60
#for depth in 10
do
    echo "    Plot depth: ${depth} km ..."
	  depth2=`printf "%02d" $depth`
    PS=plot_${depth2}km.ps
    gmt psxy $R $J -K -T -Yc > $PS

    #############################
    gmt grdimage cut.nc $R $J $X -Cgray -Icut_i.nc -BWN -Bxa2 -Bya1 >> $PS
    depth11=`echo "$depth*-1000" | bc`
    cat $m00 | gawk '{if($3==tmp) print $1,$2,$5/1000}' tmp=$depth11 | gmt blockmean $R $I | gmt surface $R $I -Gjunk.nc
    #gmt blockmean $R $I ${m00dir}/${depth}km.dat | gmt surface $R $I -Gjunk.nc
    #gmt grdmath junk.nc clip_mask.nc MUL = data.nc
    gmt grdview $R $J $X junk.nc -Ccolor_${depth2}km.cpt -Qc -t30 >> $PS
    gmt pscoast $R $J $X -N1/0.5p -W0.5p -A0/0/1 -Di -Bwsen -B0 >> $PS
    echo "${depth2} km" | gmt pstext $R $J $X -F+cBR+f16p,black -D-0.04i/0.06i -N -T -Gwhite -W0.3p -C0.04i/0.04i >> $PS
	  echo "M00" | gmt pstext $R $J $X -F+cTL+f16p,black -D0.035i/-0.055i -N -T -Gwhite -W0.3p -C0.04i/0.04i >> $PS

	  #############################
    gmt grdimage cut.nc $R $J $X -Cgray -Icut_i.nc -BN -Bxa2 -X4.2i >> $PS
	  depth12=`echo "$depth*-1000" | bc`
	  cat $infile | gawk '{if($3==tmp) print $1,$2,$4/1000}' tmp=$depth12 | gmt blockmean $R $I | gmt surface $R $I -Gjunk.nc
    #gmt grdmath junk.nc clip_mask.nc MUL = data.nc
    gmt grdview $R $J $X junk.nc -Ccolor_${depth2}km.cpt -Qc -t30 >> $PS
    gmt pscoast $R $J $X -N1/0.5p -W0.5p -A0/0/1 -Di -Bwsen -B0 >> $PS
    echo "${depth2} km" | gmt pstext $R $J $X -F+cBR+f16p,black -D-0.04i/0.06i -N -T -Gwhite -W0.3p -C0.04i/0.04i >> $PS
	  echo $step | gmt pstext $R $J $X -F+cTL+f16p,black -D0.035i/-0.055i -N -T -Gwhite -W0.3p -C0.04i/0.04i >> $PS

    gmt psscale $R $J $X -Ccolor_${depth2}km.cpt -X-2.1i -DJBC+w2.5i/0.1i+h+o0i/0.2i -Ba+l"Vs (km/s)" >> $PS

    #############################
    gmt psxy $R $J -O -T >> $PS
    gmt psconvert $PS -E300 -A -Tj -P
    rm -rf *.ps

done
#############################
rm -rf gmt.* *.nc *.cpt
