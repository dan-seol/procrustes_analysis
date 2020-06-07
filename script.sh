#!/bin/bash

if [ "$#" -eq 2 ]; then
  convert "$1" -flatten "learnerMarking.png"
  convert "$2" -flatten "teacherMarking.png"
  Rscript scriptified.R learnerMarking.png teacherMarking.png 1 
elif [ "$#" -eq 3 ]; then
  convert "$1" -flatten "learnerMarking.png"
  convert "$2" -flatten "teacherMarking.png"
  Rscript scriptified_mod.R learnerMarking.png teacherMarking.png $3
else
  echo "Usage ./script.sh filename1.png filename2.png [mmToPixels]"
fi
