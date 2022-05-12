#!/bin/bash

# copy synthetic data from solver/M00 as real data in checkboard computation
mod=M00
savefile=/data/ose-zhaoy/ELIP/checkboard
save=data
cdir=`pwd`

cd $savefile/solver/${mod}
for evt in X1.*;do
  cd $evt/OUTPUT_FILES
  mkdir -p $cdir/$save/$evt
  echo $evt
  cp *.sac $cdir/$save/$evt/
  cd $cdir/$save/$evt
  rename fwd.semd.sac sac X1.*
  cd $savefile/solver/${mod}
done

cd $savefile/solver
mv ${mod} ${mod}_checkboard
