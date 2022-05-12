#!/bin/bash

# checks if your GEOCUBIT.py is already in your path (which GEOCUBIT.py)
if ! [ -x "$(command -v GEOCUBIT.py)" ]; then
geocubit=../../CUBIT_GEOCUBIT/GEOCUBIT.py
else
geocubit=GEOCUBIT.py
fi

# meshing
echo
echo "$geocubit --build_volume --mesh --cfg=homogeneous_halfspace.cfg"
echo
$geocubit --build_volume --mesh --cfg=homogeneous_halfspace.cfg


echo
echo "$geocubit --meshfiles=MESH_GEOCUBIT/mesh_vol_0.e --export2SPECFEM3D --SEMoutput=MESH"
echo

$geocubit --meshfiles=MESH_GEOCUBIT/mesh_vol_0.e --export2SPECFEM3D --SEMoutput=MESH

echo
echo

cp -v MESH-default/nummaterial_velocity_file MESH/

