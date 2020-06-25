#!/bin/bash

if [ "$#" -eq 2 ]; then
  convert "$1" -flatten "learnerMarking.png"
  convert "$2" -flatten "teacherMarking.png"
  Rscript getTRSD.R learnerMarking.png teacherMarking.png 1 "result.xlsx" 
elif [ "$#" -eq 3 ]; then
  convert "$1" -flatten "learnerMarking.png"
  convert "$2" -flatten "teacherMarking.png"
  Rscript getTRSD.R learnerMarking.png teacherMarking.png $3 "result.xlsx"
elif [ "$#" -eq 4 ]; then
  convert "$1" -flatten "learnerMarking.png"
  convert "$2" -flatten "teacherMarking.png"
  Rscript getTRSD.R learnerMarking.png teacherMarking.png $3 $4

else
  echo "Usage ./script.sh filename1.png filename2.png [mmToPixels] [outputname.xlsx]"
fi
