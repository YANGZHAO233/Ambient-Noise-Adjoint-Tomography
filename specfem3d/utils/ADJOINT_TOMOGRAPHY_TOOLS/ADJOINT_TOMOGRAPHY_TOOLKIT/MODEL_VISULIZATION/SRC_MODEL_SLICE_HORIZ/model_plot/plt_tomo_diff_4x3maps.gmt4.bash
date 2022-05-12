#!/bin/bash
module load gcc
gmtset MEASURE_UNIT cm
gmtset HEADER_FONT_SIZE 24p
gmtset BASEMAP_TYPE plain 
mod=$1
mod1=$2
mod2=$3
indir=.
indir1=.
tomo=model.${mod}.regridded.xyz
tomo1=model.${mod1}.regridded.xyz
tomo2=model.${mod2}.regridded.xyz
PS=vs_diff_${mod}_${mod1}_${mod2}.ps
width=2.2
PROJ=M${width}i
tick=a2f1/a1
lonmin=-121.
lonmax=-114.5
latmin=32.5
latmax=36.8
RMAP=$lonmin/$lonmax/$latmin/$latmax

# create color table for dvp
#CPT=vs.cpt
cmax=10
#$PDIR/svcpt <<! >> junk
#-$cmax $cmax
#!
#mv svel13.cpt $CPT
#makecpt -Cseis -T-$cmin/$cmax/0.2 -D >$CPT
# get xyz points form vtk file
i=1
for dep in 10 15 20 25 ;do
#for dep in 25 30 35 40 ;do
#for dep in `seq $3 5 $4` ;do
    echo $i
    z=`echo $dep |awk '{print -$1*1000}'`
    # slice 1
    cat $indir/$tomo |awk '{if($3==a) print $1,$2,$4/1000}' a=$z >$indir/dep$dep.xyz
    perl convert_utm2lonlat.pl $indir/dep$dep.xyz 11 > $indir/dep$dep.dat
    ref_vs=`cat $indir/dep$dep.dat |awk 'BEGIN{sum=0.0;num=0} {{sum=sum+$3;num=num+1}} END{print sum/num}' a=$lonmin b=$lonmax c=$latmin d=$latmax`
    echo "tomo: $dep $ref_vs "
    #cat $indir/dep$dep.dat |awk '{print $1,$2,($3-a)/a*100}' a=$ref_vs >slow.slice1
    cat $indir/dep$dep.dat |awk '{print $1,$2,$3}' a=$ref_vs >slow.slice1
    blockmedian slow.slice1 -R$RMAP -I0.02 >slow.slice.m
    surface slow.slice.m -R$RMAP -I0.02 -Gslice.grd.1
    #triangulate slow.slice -R$RMAP -I0.25/0.25 -Gslice.grd -E > tria.out
    rm dep$dep.xyz dep$dep.dat

    # slice 2
    cat $indir1/$tomo1 |awk '{if($3==a) print $1,$2,-$4*100}' a=$z >$indir1/dep$dep.xyz
    perl convert_utm2lonlat.pl $indir1/dep$dep.xyz 11 > $indir1/dep$dep.dat
    ref_vs1=`cat $indir1/dep$dep.dat |awk 'BEGIN{sum=0.0;num=0} {{sum=sum+$3;num=num+1}} END{print sum/num}' a=$lonmin b=$lonmax c=$latmin d=$latmax`
    #cat $indir1/dep$dep.dat |awk '{print $1,$2,($3-a)/a*100}' a=$ref_vs >slow.slice2
    echo "tomo1: $dep $ref_vs1 "
    cat $indir1/dep$dep.dat |awk '{print $1,$2,$3}' a=$ref_vs >slow.slice2
    blockmedian slow.slice2 -R$RMAP -I0.02 >slow.slice.m
    surface slow.slice.m -R$RMAP -I0.02 -Gslice.grd.2
    #triangulate slow.slice -R$RMAP -I0.25/0.25 -Gslice.grd -E > tria.out
    rm dep$dep.xyz dep$dep.dat

    # diff
#    echo slow.slice2 > tempinp1
#    echo slow.slice1 >> tempinp1
#    echo slow.slice3 >> tempinp1
#    echo "diff: "
#    /home/l/liuqy/kai/progs/seistools/cal_xyz_diff_comp < tempinp1 > temp 2>&1
#    awk -v a=${ref_vs} '{ print $1,$2,$3/a*100}' slow.slice3 | triangulate -Gslice.grd.3 -I0.02 -R$RMAP
    cat $indir1/$tomo2 |awk '{if($3==a) print $1,$2,-$4*100}' a=$z >$indir1/dep$dep.xyz
    perl convert_utm2lonlat.pl $indir1/dep$dep.xyz 11 > $indir1/dep$dep.dat
    ref_vs2=`cat $indir1/dep$dep.dat |awk 'BEGIN{sum=0.0;num=0} {{sum=sum+$3;num=num+1}} END{print sum/num}' a=$lonmin b=$lonmax c=$latmin d=$latmax`
    #cat $indir1/dep$dep.dat |awk '{print $1,$2,($3-a)/a*100}' a=$ref_vs >slow.slice2
    echo "tomo2: $dep $ref_vs1 "
    cat $indir1/dep$dep.dat |awk '{print $1,$2,$3}' a=$ref_vs >slow.slice3
    blockmedian slow.slice3 -R$RMAP -I0.02 >slow.slice.m
    surface slow.slice.m -R$RMAP -I0.02 -Gslice.grd.3
    #triangulate slow.slice -R$RMAP -I0.25/0.25 -Gslice.grd -E > tria.out
    rm dep$dep.xyz dep$dep.dat



    #grd2cpt slice.grd.3 -Cseis -S-12/12/0.1 -L-12/12 -D -Z > mydata.cpt
    #dvmax=`awk -v a=${ref_vs} '{ print $1,$2,sqrt($3*$3/a/a*100*100)}' slow.slice3 |minmax -C |awk '{print $6}'` 
    dvmax1=`awk -v a=${ref_vs} '{ print $1,$2,sqrt($3*$3)}' slow.slice2 |minmax -C |awk '{print int($6+0.5)}'` 
    makecpt -Cseis -T-$dvmax1/$dvmax1/0.01 -D >dvs1.cpt
    dvmax2=`awk -v a=${ref_vs} '{ print $1,$2,sqrt($3*$3)}' slow.slice3 |minmax -C |awk '{print int($6+0.5)}'` 
    makecpt -Cseis -T-$dvmax2/$dvmax2/0.01 -D >dvs2.cpt
    #makecpt -CWhite_Blue -T0/$dvmax/0.01 -D >dvs.cpt
    #makecpt -CRed_Yellow_White.cpt -T-$dvmax/0/0.01 -D >dvs.cpt
    
    # make cpt
    vmin=`echo $ref_vs |awk '{print $1*(1-a/100) }' a=$cmax`
    vmax=`echo $ref_vs |awk '{print $1*(1+a/100) }' a=$cmax`
    echo "vel: $vmin $ref_vs $vmax"
    makecpt -Cseis -T$vmin/$vmax/0.01 -D >vs.cpt
    
    for imap in 1 2 3;do
    if [ $imap -eq 1 -a $i -eq 1 ];then
       psbasemap -R$RMAP -J$PROJ -B"$tick"WesN -X1.5 -Y23 -K -P > $PS
    elif [ $imap -eq 1 -a $i -gt 1 ];then
       psbasemap -R$RMAP -J$PROJ -B"$tick"::Wens -X-4.9i -Y-6.5 -O -K >> $PS
    else
       psbasemap -R$RMAP -J$PROJ -B"$tick"::wens -X2.45i -O -K >> $PS
    fi
    if [ $imap -eq 2 ];then 
        CPT=dvs1.cpt
    elif [ $imap -eq 3 ];then
        CPT=dvs2.cpt
    else
        CPT=vs.cpt 
    fi
##### using ray density map to maks
    cat $indir/raydensity.M21.regridded.xyz |awk '{if($3==a) print $1,$2,$4/1000}' a=$z >$indir/dep$dep.xyz
    perl convert_utm2lonlat.pl $indir/dep$dep.xyz 11 > $indir/dep$dep.dat
    cat $indir/dep$dep.dat |awk '{if($3*10**13>0.5) print $1,$2,1}' >mask.dat
    rm dep$dep.xyz dep$dep.dat
    psmask mask.dat -I0.02 -S0.06 -J -R -Ggray -O -K >>$PS
#
    grdimage slice.grd.$imap -J$PROJ -R$RMAP -C$CPT -B"$tick"::wens -Sl -O -K >> $PS
    psmask -I0.02 -J -R -C -O -K >>$PS 

    cat sources.dat |awk '{print $4,$3}' |psxy -J -R -St0.1 -O -K >>$PS
    psxy jennings.xy -J -R -m -W0.2p,0/0/0 -O -K >> $PS
    pscoast -J -R  -O -K -W0.5p,black -Na/0.5p,black -Dh >> $PS
    if [ $imap -eq 1 ];then
    pstext -R -J -O -K -N <<EOF >> $PS
-120.8 32.7 12 0 1 ML $mod
EOF
    elif [ $imap -eq 2 ];then
    pstext -R -J -O -K -N <<EOF >> $PS
-120.8 32.7 12 0 1 ML ln(M17/M16)
EOF
    else
    pstext -R -J -O -K -N <<EOF >> $PS
-120.8 32.7 12 0 1 ML ln(M19/M16)
EOF
    fi
    pstext -R -J -W255/255/255,o,1p -O -K -N <<EOF >> $PS
-120.8 36.6 12 0 1 ML ${dep}km
EOF

    done # end loop of imap
    #
    dvstick1=`echo $dvmax1 |awk '{printf"%.1f",$1}'`
    dvstick2=`echo $dvmax2 |awk '{printf"%.1f",$1}'`
    vstick=`echo $vmax $vmin |awk '{printf"%.1f",($1-$2)/4}'`
    psscale -D1.1i/-0.5/1.8i/0.3h -Cdvs1.cpt -Ba${dvstick1}f1/:"%": -E -O -K -N >> $PS
    psscale -D-1.35i/-0.5/1.8i/0.3h -Cdvs2.cpt -Ba${dvstick2}f1/:"%": -E -O -K -N >> $PS
    psscale -D-3.8i/-0.5/1.8i/0.3h -Cvs.cpt -Ba${vstick}f0.1/:"km/s": -E -O -K -N >> $PS

#    if [ $i -eq 4 ];then
#    ### plot slice
#    psxy slice.dat -J -R -W1p,- -m -O -K >> $PS
#    cat slice.dat  |grep "^[^>]" |awk '{printf"%f %f 10 0 0 RT %s\n",$1,$2,$3}'  |pstext -J -R -W -O -K -N >> $PS
#    cat slice.dat  |grep "^[^>]" |awk '{printf"%f %f\n",$1,$2}'  |psxy -J -R -St0.1i -Wblack -Ggreen -O -K >> $PS
#    fi

   
    let i=i+1
done # end loop of depth 
cat /dev/null |psxy -J -R -Sc0.5i -O -N >>$PS
exit

