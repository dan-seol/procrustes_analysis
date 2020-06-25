#!/bin/bash

#Based on @ykarikos .sh
if [ "$1" == "" ]; then
  echo Usage: $0 pngfile
  exit 0;
fi

FILE=`basename $1 ".png"`

if [ ! -e "$FILE.png" ]; then
  echo $FILE.png does not exist
  exit 1;
fi

convert $FILE.png -flatten $FILE_flat.png
convert $FILE_flat.png -grayscale average $FILE_gray.png
#convert +negate $FILE_gray.png $FILE_inverted.png 
#convert $FILE_inverted.png $FILE.pnm
convert $FILE_gray.png $FILE.pnm
potrace -s -o $FILE.svg $FILE.pnm
rm -rf $FILE.pnm
