#!/bin/sh
../bin/planar_mesh_generator op_lam.nml
tol=1.0E-30
start=0
end=24
kgo_dir=OP-Lam-KGOs
basename=LAM_locals

for i in $(seq ${start} ${end}) ; do
  echo Comparing LAM_locals_${i}-5.nc
  nccmp -sdfgm --Tolerance=${tol} ${basename}_${i}-${end}.nc ${kgo_dir}/${basename}_${i}-${end}.nc
  i=$(($i+1))
done

rm ${basename}*.nc
