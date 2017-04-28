#!/bin/ksh
##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################

rm -f PET*
rm -f *.err
rm -f *.out

export XPROC=1
export YPROC=1

export NPANELS=6

NPES=$(($NPANELS * $XPROC * $YPROC ))

EXE=../bin/dynamo

echo "mpirun -np $NPES $EXE 1>$EXE.out 2>$EXE.err"

# Run the code
mpirun -np $NPES $EXE 1>$EXE.out 2>$EXE.err
