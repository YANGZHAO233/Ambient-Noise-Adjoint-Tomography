#!/bin/bash

indir=${1}
bands=${2}
mod=${3}
outfile=${4}

##################################
echo "TABLE ${bands} Average" > $outfile

##################################
for dir in `ls $indir/${mod}* -d`
do
	modname=`basename $dir`
	echo "Model: $modname is processing ..."

	step=`echo $modname | gawk -F"_" '{print $2}'`
	if [ "$step" == "" ]
	then
		step="slen0.00"
	fi

	##################################
	myline=$step

	##################################
	cat /dev/null > tmp2.dat
	for band in $bands
	do
		echo "  $band is processing ..."
		cat /dev/null > tmp.dat
		for item in `ls $dir/${modname}_${band}_*`
		do
			gawk '{if($29!=0) printf "%f\n",$29}' $item >> tmp.dat
		done
		ave=`gmt gmtmath tmp.dat -S -i0 MEAN = | gawk '{printf "%10.3f\n",$1}'`
		echo $ave >> tmp2.dat
		myline=`echo "$myline $ave"`
	done

	##################################
	ave2=`gmt gmtmath tmp2.dat -S -i0 MEAN = | gawk '{printf "%10.3f\n",$1}'`
	myline=`echo "$myline $ave2"`
	echo $myline >> $outfile

done

##################################
rm -rf tmp*.dat
