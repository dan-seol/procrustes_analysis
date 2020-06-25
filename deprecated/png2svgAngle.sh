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

convert $FILE.png -flatten "$FILE_flat$2".png
convert "$FILE_flat$2".png -grayscale average "$FILE_gray$2".png
#convert +negate $FILE_gray.png $FILE_inverted.png 
#convert $FILE_inverted.png $FILE.pnm
convert "$FILE_gray$2".png "$FILE$2".pnm
potrace -s -o "$FILE$2".svg "$FILE$2".pnm -A "$2"
rm -rf "$FILE$2".pnm
