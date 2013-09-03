#!/bin/bash

PROGRAM="/home/louis/prog/haskell/hdata/src/hdata"

valid1="-y 1001 -t titre -k key1 key2 -p 100 200 -v 30 -a a1 a2"

invalid1=""
invalid2="-t -p 100"
invalid3="-t titrebon -u fail"
invalid4="-v volume"
invalid5="-y year"
invalid6="-p rr"
invalid7="-u fail"
invalid8="-p 100 11t"
invalid9="-p dd 100"
invalid10="-f doesnotexist"
invalid11="-t titre -t duplicated"

valid2="-f createMe"


#########################################


$PROGRAM add $valid1
if [  $? != 0  ]; then
    echo "============> test no. v1 FAILED"
fi

touch "createMe"
$PROGRAM add $valid2
if [ $? != 0 ]; then
    echo "============> test no. v2 FAILED"
fi
rm "createMe"

$PROGRAM add $invalid1
if [ $? = 0 ]; then
    echo "============> test no. i1 FAILED"
fi

$PROGRAM add $invalid2
if [ $? = 0 ]; then
    echo "============> test no. i2 FAILED"
fi

$PROGRAM add $invalid3
if [ $? = 0 ]; then
    echo "============> test no. i3 FAILED"
fi

$PROGRAM add $invalid4
if [ $? = 0 ]; then
    echo "============> test no. i4 FAILED"
fi

$PROGRAM add $invalid5
if [ $? = 0 ]; then
    echo "============> test no. i5 FAILED"
fi

$PROGRAM add $invalid6
if [ $? = 0 ]; then
    echo "============> test no. i6 FAILED"
fi

$PROGRAM add $invalid7
if [ $? = 0 ]; then
    echo "============> test no. i7 FAILED"
fi

$PROGRAM add $invalid8
if [ $? = 0 ]; then
    echo "============> test no. i8 FAILED"
fi

$PROGRAM add $invalid9
if [ $? = 0 ]; then
    echo "============> test no. i9 FAILED"
fi

$PROGRAM add $invalid10
if [ $? = 0 ]; then
    echo "============> test no. i10 FAILED"
fi

$PROGRAM add $invalid11
if [ $? = 0 ]; then
    echo "============> test no. i11 FAILED"
fi

