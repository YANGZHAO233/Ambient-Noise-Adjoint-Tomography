#!/bin/bash
indir=${1}
outdir=${2}
step=${3}
topofile=${4}
mod=${5}
R=${6}
name=${7}
lons=${8}
lats=${9}
depth=${10}
px1=`echo $lons | awk -F "_" '{print $1}'`
px2=`echo $lons | awk -F "_" '{print $2}'`
py1=`echo $lats | awk -F "_" '{print $1}'`
py2=`echo $lats | awk -F "_" '{print $2}'`
s1=`echo $name | awk -F "_" '{print $1}'`
s2=`echo $name | awk -F "_" '{print $2}'`
PS="${s1}_${s1}.ps"
DTV_loc="113.65386486 40.07945059"
#echo $indir $outdir $step $topofile $mod $R $name $lons $lats $depth
#echo $px1 $py1 $px2 $py2 $s1 $s2 $PS
#echo " "
echo "    plot cross section:" ${s1}_${s1} 

[ ! -d $outdir ] && mkdir -p $outdir
cd $outdir

J="-JM10c"
X="-K -O"
I="-I0.05"
G="-G1"

gmt gmtset MAP_FRAME_TYPE plain
gmt gmtset FONT_LABEL +14p
gmt gmtset FONT_ANNOT_PRIMARY +10p

gmt psxy $R $J -K -T -Yc > $PS

########## plot research region
gmt grdcut $topofile $R -Gcut.nc
gmt grdgradient cut.nc -Ne0.8 -A100 -fg -Gcut_i.nc
gmt makecpt -Cgray -T-20000/10000/1000 -D -Z > ttopo.cpt
gmt grdimage cut.nc $R $J $X -BWeSn -Bxa1 -Bya1 -Cttopo.cpt -Icut_i.nc >> $PS
echo $DTV_loc | gmt psxy $R $J $X -Skvolcano/0.5c -Gred >> $PS 
gmt psxy $R $J $X -Cblack -W2p >> $PS <<EOF
$px1 $py1
$px2 $py2
EOF
gmt pstext $R $J $X -F+f16p,4,black+jTL >> $PS <<EOF
$px1 $py1 $s1
$px2 $py2 $s2
EOF

########## get cross section topo and vs
gmt project -C$px1/$py1 -E$px2/$py2 $G -Q > line.dat
sed -i '$d' line.dat
max_dist=`tail -n 1 line.dat | awk '{print $3}'`
echo "0 0" > topo.dat
gmt grdtrack line.dat -G$topofile | gawk '{print $3, $4}' >> topo.dat
echo "$max_dist 0" >> topo.dat

[ -e line_absVS.dat ] && rm line_absVS.dat
for ((i = 0; i <= ${depth}; i=i+10));
do
#   echo "    Depth: ${i} is processing ..."
   depth2=`echo "${i}*-1000" | bc`
   cat $indir/vs_${step}_geo | gawk '{if($3==tmp) print $1,$2,$4/1000}' tmp=$depth2 > tmp.dat
   gmt surface tmp.dat $R $I -Gjunk.nc
   gmt grdtrack line.dat -Gjunk.nc | gawk -v dep=$i '{print $3, dep, $4}' >> line_absVS.dat
done

########## plot topo and Vs
R1="-R0/$max_dist/0/3"
J1="-JX12c/2c"
R2="-R0/$max_dist/0/${depth}"
J2="-JX12c/-8c"

cat topo.dat | awk '{print $1,$2/1000}' | gmt psxy $R1 $J1 $X -BWe -Bya1 -Cblack -W0.5p -Ggray -X12c -Y8c >> $PS
echo $s1 | gmt pstext $R1 $J1 $X -F+f16p,4+cTL -Dj0.2c/0c >> $PS
echo $s2 | gmt pstext $R1 $J1 $X -F+f16p,4+cTR -Dj0.2c/0c >> $PS
gmt makecpt -Cseis -T3.3/4.3/0.05 -D -Z > color_abs.cpt
gmt blockmean $R2 $I line_absVS.dat | gmt surface $R2 $I -Gdata.nc
gmt grdimage $R2 $J2 $X data.nc -Ccolor_abs.cpt -BWeSn -Bxa+l"Distance (km)" -Bya20 -Y-8.0c >> $PS
echo $step | gmt pstext $R $J $X -F+cBL+f12p,black -D0.1c/0.2c -N -T -Gwhite -W0.3p,white >> $PS
gmt psscale $R2 $J2 $X -Ccolor_abs.cpt -DJRC+w5c/0.2c -Ba+l"Vs (km/s)" -X7c >> $PS

gmt psxy $R $J -O -T >> $PS
gmt psconvert $PS -E300 -A -Tj -P

rm -rf gmt.* *.cpt *.nc *.dat *.ps
