INPUT=lfric_diag.nc
OUTPUT=ls.nc

# Change the file from a time sequence of 3d fields(axes=space),
# to a single 4d field (axes=space+time) by changing the time dimension
# from unlimited to time=NN.
ncks --fix_rec_dmn time -O $INPUT ${INPUT}_tmp

# Change the file so that the time starts at 0, not 5 (plus extra)
ncap2 -O -s 'time-=5.5; time_bounds-=5.5' ${INPUT}_tmp ${OUTPUT}
rm ${INPUT}_tmp



