#!/bin/bash

##################################
indir=${1}
iset=${2}
isrc=${3}
chidir=${4}
outdir=${5}
bands=${6}
mod=M00
##################################
mkdir -p $outdir/$isrc
cd $outdir/$isrc

##################################
echo "$isrc is processing ..."

##################################
nsrc="4"		# number of stations for each figure
nlen1="20"		# time shift for band1
nlen2="25"		# time shift for band2
nlen3="35"		# time shift for band3
maxlen="400"	# maximum time length

##################################
J="-JX3.2i/1i"
X="-K -O"
gmt gmtset FONT_ANNOT_PRIMARY +10p
gmt gmtset FONT_LABEL +12p
gmt gmtset FONT_TITLE +16p

##################################
basedir="${indir}/${iset}/${isrc}"
band1=`echo $bands | gawk '{print $1}'`
band2=`echo $bands | gawk '{print $2}'`
band3=`echo $bands | gawk '{print $3}'`
winfile1=${basedir}/MEASUREMENT.WINDOWS.${band1}
winfile2=${basedir}/MEASUREMENT.WINDOWS.${band2}
winfile3=${basedir}/MEASUREMENT.WINDOWS.${band3}
nmea=`sed -n '1p' $winfile1`

dataline="2"
synline="3"
winline="5"
for ((i=0;i<$nmea;i++))
do
	ifig=`echo "$i/$nsrc + 1" | bc`
	ifig2=`echo "($i+1)/$nsrc + 1" | bc`
	if [ "$ifig2" != "$ifig" ]
	then
		flag=true
	else
		flag=false
	fi
	if [ `echo "$i + 1" | bc`  == $nmea ]
	then
		flag=true
	fi
	ifig=`printf "%02d" $ifig`
	iline=`echo $i%$nsrc | bc`
	PS=$ifig.ps


	# ***********************************************************
	# plot band1
	# ***********************************************************
	datafile=${basedir}/`sed -n ${dataline}p $winfile1`
	synfile=${basedir}/`sed -n ${synline}p $winfile1`
	#######
	synfile0=`sed -n ${synline}p $winfile1`
  synfile_M00=${basedir}/SYN_${mod}/`echo ${synfile0#*/}`
	#######
	t1=`sed -n ${winline}p $winfile1 | gawk '{print $1}'`
	t2=`sed -n ${winline}p $winfile1 | gawk '{print $2}'`
	ista=`basename $datafile | gawk -F"." '{print $1"."$2}'`

	ts=`echo $t1-$nlen1 | bc`
	te=`echo $t2+$nlen1 | bc`
	ts=`echo $ts | gawk '{if($1<0) print "0"; else print $1}'`
	te=`echo $te $maxlen | gawk '{if($1>$2) print $2; else print $1}'`
	R="-R$ts/$te/-1/1"

	if [ $iline == "0" ]
	then
		gmt psxy $R $J -T -K -X0.3i -Y6.4i > $PS
	else
		gmt psxy $R $J $X -T -X-7.8i -Y-2i >> $PS
	fi

	printf "%s -1\n%s 1\n%s 1\n%s -1\n" $t1 $t1 $t2 $t2 | gmt psxy $R $J $X -L -Glightblue >> $PS
	gmt pssac $R $J $X $datafile -M0.8i -BS -Bxa -W0.7p,black >> $PS
  gmt pssac $R $J $X $synfile_M00 -M0.8i -W0.7p,blue >> $PS
	gmt pssac $R $J $X $synfile -M0.8i -W0.7p,red >> $PS
	echo "${isrc}_${ista}" | gmt pstext $R $J $X -F+cTC+f10p,1,black -D0i/0.2i -N >> $PS
	if [ $iline == "0" ]
	then
		echo "$band1" | gmt pstext $R $J $X -F+cTC+f16p,1,black -D0i/0.7i -N >> $PS
	fi

	mt=`gawk '{if($3"."$2==tmp) printf "%6.2f\n",$13}' tmp=$ista $chidir/*_${band1}_${isrc}_window_chi`
	xc=`gawk '{if($3"."$2==tmp) printf "%6.2f\n",$15}' tmp=$ista $chidir/*_${band1}_${isrc}_window_chi`
	echo "MT:${mt}s" | gmt pstext $R $J $X -F+cTL+f9p,4,black -D2.6i/0.4i -N >> $PS
	echo "XC:${xc}s" | gmt pstext $R $J $X -F+cTL+f9p,4,black -D2.608i/0.2i -N >> $PS



	# ***********************************************************
	# plot band2
	# ***********************************************************
	datafile=${basedir}/`sed -n ${dataline}p $winfile2`
	synfile=${basedir}/`sed -n ${synline}p $winfile2`
  #######
  synfile0=`sed -n ${synline}p $winfile2`
  synfile_M00=${basedir}/SYN_${mod}/`echo ${synfile0#*/}`
  #######
	t1=`sed -n ${winline}p $winfile2 | gawk '{print $1}'`
	t2=`sed -n ${winline}p $winfile2 | gawk '{print $2}'`
	ista=`basename $datafile | gawk -F"." '{print $1"."$2}'`

	ts=`echo $t1-$nlen2 | bc`
	te=`echo $t2+$nlen2 | bc`
	ts=`echo $ts | gawk '{if($1<0) print "0"; else print $1}'`
	te=`echo $te $maxlen | gawk '{if($1>$2) print $2; else print $1}'`
	R="-R$ts/$te/-1/1"

	printf "%s -1\n%s 1\n%s 1\n%s -1\n" $t1 $t1 $t2 $t2 | gmt psxy $R $J $X -L -Glightblue -X3.9i >> $PS
	gmt pssac $R $J $X $datafile -M0.8i -BS -Bxa -W0.7p,black >> $PS
  gmt pssac $R $J $X $synfile_M00 -M0.8i -W0.7p,blue >> $PS
	gmt pssac $R $J $X $synfile -M0.8i -W0.7p,red >> $PS
	echo "${isrc}_${ista}" | gmt pstext $R $J $X -F+cTC+f10p,1,black -D0i/0.2i -N >> $PS
	if [ $iline == "0" ]
	then
		echo "$band2" | gmt pstext $R $J $X -F+cTC+f16p,1,black -D0i/0.7i -N >> $PS
	fi

	mt=`gawk '{if($3"."$2==tmp) printf "%6.2f\n",$13}' tmp=$ista $chidir/*_${band2}_${isrc}_window_chi`
	xc=`gawk '{if($3"."$2==tmp) printf "%6.2f\n",$15}' tmp=$ista $chidir/*_${band2}_${isrc}_window_chi`
	echo "MT:${mt}s" | gmt pstext $R $J $X -F+cTL+f9p,4,black -D2.6i/0.4i -N >> $PS
	echo "XC:${xc}s" | gmt pstext $R $J $X -F+cTL+f9p,4,black -D2.608i/0.2i -N >> $PS

	# ***********************************************************
	# plot band3
	# ***********************************************************
	datafile=${basedir}/`sed -n ${dataline}p $winfile3`
	synfile=${basedir}/`sed -n ${synline}p $winfile3`
  #######
  synfile0=`sed -n ${synline}p $winfile3`
  synfile_M00=${basedir}/SYN_${mod}/`echo ${synfile0#*/}`
  #######
	t1=`sed -n ${winline}p $winfile3 | gawk '{print $1}'`
	t2=`sed -n ${winline}p $winfile3 | gawk '{print $2}'`
	ista=`basename $datafile | gawk -F"." '{print $1"."$2}'`

	ts=`echo $t1-$nlen3 | bc`
	te=`echo $t2+$nlen3 | bc`
	ts=`echo $ts | gawk '{if($1<0) print "0"; else print $1}'`
	te=`echo $te $maxlen | gawk '{if($1>$2) print $2; else print $1}'`
	R="-R$ts/$te/-1/1"

	printf "%s -1\n%s 1\n%s 1\n%s -1\n" $t1 $t1 $t2 $t2 | gmt psxy $R $J $X -L -Glightblue -X3.9i >> $PS
	gmt pssac $R $J $X $datafile -M0.8i -BS -Bxa -W0.7p,black >> $PS
  gmt pssac $R $J $X $synfile_M00 -M0.8i -W0.7p,blue >> $PS
	gmt pssac $R $J $X $synfile -M0.8i -W0.7p,red >> $PS
	echo "${isrc}_${ista}" | gmt pstext $R $J $X -F+cTC+f10p,1,black -D0i/0.2i -N >> $PS
	if [ $iline == "0" ]
	then
		echo "$band3" | gmt pstext $R $J $X -F+cTC+f16p,1,black -D0i/0.7i -N >> $PS
	fi

	mt=`gawk '{if($3"."$2==tmp) printf "%6.2f\n",$13}' tmp=$ista $chidir/*_${band3}_${isrc}_window_chi`
	xc=`gawk '{if($3"."$2==tmp) printf "%6.2f\n",$15}' tmp=$ista $chidir/*_${band3}_${isrc}_window_chi`
	echo "MT:${mt}s" | gmt pstext $R $J $X -F+cTL+f9p,4,black -D2.6i/0.4i -N >> $PS
	echo "XC:${xc}s" | gmt pstext $R $J $X -F+cTL+f9p,4,black -D2.608i/0.2i -N >> $PS

	if [ $flag == "true" ]
	then
		gmt psxy $R $J -T -O >> $PS
		gmt psconvert $PS -Tj -E300 -A -P
		rm -rf $PS
	fi

	# increase the line numbers
	dataline=`echo "$dataline+4" | bc`
	synline=`echo "$synline+4" | bc`
	winline=`echo "$winline+4" | bc`
done

##################################
rm -rf gmt.*
