# procrustes_analysis

# Convert .png to .svg
## when you want to convert a single `.png` image to `.svg`

1. Bring image in the same folder in `png2svg.sh`
2. Type on your terminal
  `./png2svg.sh <image_name>.png`; it the name of the image name is `hello.png`, type `./png2svg.sh hello.png`

## when you have a folder of images
1. Copy `png2svg.sh` to the folder of images (it should have only the images).
2. Type 
    `for file in <folder_name>/*; do ./png2svg.sh `basename $file`; done`
    If the name of the folder is new_folder, type in your terminal
   `for file in new_folder/*; do ./png2svg.sh `basename $file`; done`

# Use coordinator to get `.csv` s from `.svg` files.
Sample as low number of points as you can (200-500)

# Run the R script
Open `getTRSD.R` script, and type `mainSvg("<reference_image_name>.csv", "<comparable_image_name>.csv", outputname="<whatever_i_want>.xlsx")`
e.g. ) `mainSvg("teacher.csv", "learner.csv", outputname="teacherVsLearner.xlsx")`
 
