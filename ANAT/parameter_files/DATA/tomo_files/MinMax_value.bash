#!/bin/bash
file=$1
#min xyz
awk 'BEGIN {min = 6553611111111} {if ($1+0 < min+0) min=$1} END {print "MinX=", min}' $file
awk 'BEGIN {min = 6553611111111} {if ($2+0 < min+0) min=$2} END {print "MinY=", min}' $file
awk 'BEGIN {min = 6553611111111} {if ($3+0 < min+0) min=$3} END {print "MinZ=", min}' $file
#max xyz
awk 'BEGIN {max = 0} {if ($1+0 > max+0) max=$1} END {print "MaxX=", max}' $file
awk 'BEGIN {max = 0} {if ($2+0 > max+0) max=$2} END {print "MaxY=", max}' $file
awk 'BEGIN {max = 0} {if ($3+0 > max+0) max=$3} END {print "MaxZ=", max}' $file
#vp
awk 'BEGIN {min = 6553611111111} {if ($4+0 < min+0) min=$4} END {print "MinVP=", min}' $file
awk 'BEGIN {max = 0} {if ($4+0 > max+0) max=$4} END {print "MaxVP=", max}' $file
#vs
awk 'BEGIN {min = 6553611111111} {if ($5+0 < min+0) min=$5} END {print "MinVS=", min}' $file
awk 'BEGIN {max = 0} {if ($5+0 > max+0) max=$5} END {print "Max=VS", max}' $file
#rho
awk 'BEGIN {min = 6553611111111} {if ($6+0 < min+0) min=$6} END {print "MinRHO=", min}' $file
awk 'BEGIN {max = 0} {if ($6+0 > max+0) max=$6} END {print "MaxRHO=", max}' $file

