#!/bin/bash

###########################################
workdir=/work/ose-zhaoy/fullWaveform_python
old_dir=/work/ose-zhaoy/ELIP/adjoint
datadir=$old_dir/data
rm -rf $workdir/data
mkdir $workdir/data

# Randomly extracted rows from sources.dat
num_lines=30
rm -f $workdir/src_rec/*
srfile=${workdir}/src_rec/sources.dat
shuf -n${num_lines} ${old_dir}/src_rec/sources.dat > ${srfile}

###########################################
cat $srfile | while read evtfile;do
   stnm=`echo $evtfile |awk '{print $1}'`
   echo $stnm 
   mkdir -p $workdir/data/X1.${stnm}
   

   cat $srfile | while read evtfile2;do
       stnm2=`echo $evtfile2 |awk '{print $1}'`
       #echo $stnm2

       if [ "$stnm"x != "$stnm2"x ];then
           cp $datadir/X1.${stnm}/*${stnm2}* $workdir/data/X1.${stnm}/
       fi
   done
done

