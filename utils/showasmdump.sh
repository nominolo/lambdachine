#!/bin/sh
INP=$1

if test -z $INP
then
    echo "No input file specified"
    exit 1
fi

OBJDUMP=`which objdump`
OTOOL=`which otool`


if test -z $OTOOL && test -z $OBJDUMP
then
    echo "No otool or objdump found."
    exit 1
elif test -z $OBJDUMP
then
    DUMP=$OTOOL
    FLAGS=-tv
else
    DUMP=$OBJDUMP
    FLAGS=-d
fi

gcc -c $INP -o $INP.o && $DUMP $FLAGS $INP.o
