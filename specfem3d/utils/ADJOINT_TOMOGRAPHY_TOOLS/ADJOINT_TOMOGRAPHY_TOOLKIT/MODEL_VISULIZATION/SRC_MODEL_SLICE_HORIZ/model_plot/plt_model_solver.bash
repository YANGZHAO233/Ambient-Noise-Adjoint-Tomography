#!/bin/bash

mod=M00_slen0.10
prog=/home/kai/SEM_adjoint/specfem3d_v3.0

head -1 ../../src_rec/sources.dat |
while read evtfile; do # only need the model file in one event (model are the same for all events)
eid=`echo $evtfile |awk '{printf"%s.%s",$2,$1}'`
fwd_dir=solver/$mod/$eid
echo $eid

slicefile=slice.lst
seq 0 1 167 >$slicefile

in_path=../../$fwd_dir/OUTPUT_FILES/DATABASES_MPI/
#in_path=../../$fwd_dir/DATA/tomo_files/
out_path=model_$mod/
ln -s $prog/bin . 
ln -s ../../$fwd_dir/DATA .
ln -s ../../$fwd_dir/OUTPUT_FILES .
mkdir -p $out_path
for tag in vp vs rho
do
   ./bin/xcombine_vol_data $slicefile $tag $in_path $out_path 0
done

done
