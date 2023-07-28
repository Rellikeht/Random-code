#!/bin/sh

cd $3

for f in $(ls ../$2/*.asm)
do
    ../$1 $f
    of=${f##.*/}
    mv out.bin ${of%.asm}.bin
done

rm debug.txt
