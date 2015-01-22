#!/bin/ksh

NPES=1

EXE=dynamo

echo "mpirun -np $NPES $EXE 1>$EXE.out 2>$EXE.err"

# Run the code
mpirun -np $NPES $EXE 1>$EXE.out 2>$EXE.err
