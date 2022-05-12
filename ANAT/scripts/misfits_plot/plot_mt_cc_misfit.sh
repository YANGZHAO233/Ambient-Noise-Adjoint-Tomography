#!/bin/bash

# ****************************************************
# $13: MT-TT meas		$17: MT-TT error
# $15: XC-TT meas		$19: XC-TT error
# ****************************************************

indir=${1}
mod=${2}
dataout=${3}
imageout=${4}
bands=${5}
R=${6}
dataout0=${7} #M00
##################################
gmt gmtset FONT_ANNOT_PRIMARY +10p
gmt gmtset FONT_LABEL +12p
gmt gmtset FONT_TITLE +16p
J="-JX2.2i/2i"
X="-K -O"

##################################
# Generate the misfit data for each band
##################################
mkdir -p $dataout $imageout
for modpath in `ls $indir/${mod}* -d`
do
	echo "$mod is processing ..."
	if [ $dataout0 != $dataout ] # plot M00
	then
	  mod0="M00"
		flag=true
	else
		flag=false
	fi

	PS=$imageout/${mod}.ps

	##################################
	gmt psxy $R $J -T -K > $PS
	##################################
	i=0
	for band in $bands
	do
	  let i=i+1
		echo "  $band is processing ..."
		for item in `ls $indir/${mod}/${mod}_${band}_*`
		do
			evtnm=`echo $item | gawk -F'/' '{print $NF}' | gawk -F'_' '{print $(NF-2)}'`
			gawk '{printf "%10s %10s %12.4f %12.4f\n",tmp,$3"."$2,$13,$17}' tmp=$evtnm $item >> $dataout/${mod}_${band}_mt_all.dat
			gawk '{if($13!=0) printf "%10s %10s %12.4f %12.4f\n",tmp,$3"."$2,$13,$17}' tmp=$evtnm $item >> $dataout/${mod}_${band}_mt_cc.dat
			gawk '{if($13==0&&$15!=0) printf "%10s %10s %12.4f %12.4f\n",tmp,$3"."$2,$15,$19}' tmp=$evtnm $item >> $dataout/${mod}_${band}_mt_cc.dat
		done

		if $flag
		then
		  if [ $i == 1 ]
		  then
		    gmt pshistogram $R $J $X $dataout0/${mod0}_${band}_mt_cc.dat -i2 -Bxa2+l"Time (s)" -Bya+l"Number of measurements"\
			 -BWS+t"${band}" -L0.5p -W0.5 -Gblue -F >> $PS
		  else
        gmt pshistogram $R $J $X $dataout0/${mod0}_${band}_mt_cc.dat -i2 -Bxa2+l"Time (s)" -BS+t"${band}" -L0.5p -W0.5 \
        -Gblue -F -X2.5i >> $PS
      fi
      gmt pshistogram $R $J $X $dataout/${mod}_${band}_mt_cc.dat -i2 -Bxa2+l"Time (s)" -BS+t"${band}" -L0.7p,red -W0.5 \
      -F >> $PS
		else
		  if [ $i == 1 ]
		  then
		    gmt pshistogram $R $J $X $dataout/${mod}_${band}_mt_cc.dat -i2 -Bxa2+l"Time (s)" -Bya+l"Number of measurements"\
			 -BWS+t"${band}" -L0.5p -W0.5 -Gblue -F >> $PS
		  else
        gmt pshistogram $R $J $X $dataout/${mod}_${band}_mt_cc.dat -i2 -Bxa2+l"Time (s)" -BS+t"${band}" -L0.5p -W0.5 \
        -Gblue -F -X2.5i >> $PS
      fi
		fi

		gmt psbasemap $R $J $X -Bwsen -B0 >> $PS

		if $flag
		then
		  nwin=`cat $dataout0/${mod0}_${band}_mt_all.dat | wc -l `
      nmea=`cat $dataout0/${mod0}_${band}_mt_cc.dat | wc -l`
      echo "nwin: $nwin" | gmt pstext $R $J $X -F+cTL+f7p,1,black -D0.14i/-0.1i >> $PS
      echo "nmea: $nmea" | gmt pstext $R $J $X -F+cTL+f7p,1,black -D0.09i/-0.25i >> $PS

      nwin=`cat $dataout/${mod}_${band}_mt_all.dat | wc -l `
			nmea=`cat $dataout/${mod}_${band}_mt_cc.dat | wc -l`
			echo "nwin: $nwin" | gmt pstext $R $J $X -F+cTL+f7p,1,red -D0.14i/-0.4i >> $PS
			echo "nmea: $nmea" | gmt pstext $R $J $X -F+cTL+f7p,1,red -D0.09i/-0.55i >> $PS

      ave=`gmt gmtmath $dataout0/${mod0}_${band}_mt_all.dat -S -i2 MEAN =`
      std=`gmt gmtmath $dataout0/${mod0}_${band}_mt_cc.dat -S -i2 STD =`
      printf "avg: %.2f s" $ave | gmt pstext $R $J $X -F+cTL+f7p,1,black -D1.53i/-0.1i >> $PS
      printf "std: %.2f s" $std | gmt pstext $R $J $X -F+cTL+f7p,1,black -D1.56i/-0.25i >> $PS

      ave=`gmt gmtmath $dataout/${mod}_${band}_mt_all.dat -S -i2 MEAN =`
      std=`gmt gmtmath $dataout/${mod}_${band}_mt_cc.dat -S -i2 STD =`
      printf "avg: %.2f s" $ave | gmt pstext $R $J $X -F+cTL+f7p,1,red -D1.53i/-0.4i >> $PS
      printf "std: %.2f s" $std | gmt pstext $R $J $X -F+cTL+f7p,1,red -D1.56i/-0.55i >> $PS

		else
		  nwin=`cat $dataout/${mod}_${band}_mt_all.dat | wc -l `
      nmea=`cat $dataout/${mod}_${band}_mt_cc.dat | wc -l`
      echo "nwin: $nwin" | gmt pstext $R $J $X -F+cTL+f7p,1,black -D0.14i/-0.1i >> $PS
      echo "nmea: $nmea" | gmt pstext $R $J $X -F+cTL+f7p,1,black -D0.09i/-0.25i >> $PS

      ave=`gmt gmtmath $dataout/${mod}_${band}_mt_all.dat -S -i2 MEAN =`
      std=`gmt gmtmath $dataout/${mod}_${band}_mt_cc.dat -S -i2 STD =`
      printf "avg: %.2f s" $ave | gmt pstext $R $J $X -F+cTL+f7p,1,black -D1.53i/-0.1i >> $PS
      printf "std: %.2f s" $std | gmt pstext $R $J $X -F+cTL+f7p,1,black -D1.56i/-0.25i >> $PS
    fi
	done

	##################################
	gmt psxy $R $J -T -O >> $PS

	##################################
	gmt psconvert $PS -Tj -E300 -A -P
	rm -rf $PS

done

##################################
rm -rf gmt.*
