#!/bin/sh
../bin/cubedsphere_mesh_generator op_cube.nml
tol=1.0E-30
start=0
end=5
kgo_dir=OP-Cube-KGOs
basename=C6_locals

for i in $(seq ${start} ${end}) ; do
  echo Comparing C6_locals_${i}-${end}.nc
  nccmp -sdfgm --Tolerance=${tol} ${basename}_${i}-${end}.nc ${kgo_dir}/${basename}_${i}-${end}.nc
  i=$(($i+1))
done

rm ${basename}*.nc
