#@author: Dan Yunheum Seol<yunheum.seol@mail.mcgill.ca> <yunheum.seol.conseolting@gmail.com>
#Import all the required R packages.
library(compiler)
library(magrittr)
library(EBImage)
library(shapes)
library(kmlShape)
library(Rcpp)
library(RcppArmadillo)
library(vec2dtransf)
#these are helper modules written in RcppArmadillo for faster computation
sourceCpp("imageThinning.cpp")
sourceCpp("downSamplePoints.cpp")

#convert rawImagePathRGBA.png -flatten imagePath.png

MarkingEvaluation <- setClass(
  # Set the name for the class
  "MarkingEvaluation",
  # Define the slots
  slots = c(
    translation = "matrix",
    rotation  = "numeric",
    scale  = "numeric",
    dissimilarity = "numeric"
  ),
  # Set the default values for the slots. (optional)
  prototype=list(
    rotation = 0,
    scale = 1
  ))

procOPAcompiled <- cmpfun(procOPA)

getTRSDInMilimeters <- cmpfun(function(imagePath1, imagePath2, MilimeterToPixel, doThinning=TRUE) {
  #turn the images intro binary, where the marking is white and the background is black
  thinnedImage1 <- readImage(imagePath1) %>% channel(., "gray") %>% `<`(., 0.5) %>% bwlabel(.) 
  thinnedImage2 <- readImage(imagePath2) %>% channel(., "gray") %>% `<`(., 0.5) %>% bwlabel(.)
  
  if (doThinning) {
    thinnedImage1 <- thinnedImage1 %>% thinImage
    thinnedImage2 <- thinnedImage2 %>% thinImage
  }
  
  #take the 2d coordinates of the image pixels, only the part where it is surgical marking
  curvePoints1 <- getCurvePoints(thinnedImage1)
  curvePoints2 <- getCurvePoints(thinnedImage2)
  dim1 <- dim(curvePoints1)[1]
  dim2 <- dim(curvePoints2)[1]
  minDim <- min(dim1, dim2)
  #even out the number of points so that they can be comparable with procGPA
  updatedCurvePoints1 <- DouglasPeuckerNbPoints(curvePoints1$x_coords, curvePoints1$y_coords, minDim)
  updatedCurvePoints2 <- DouglasPeuckerNbPoints(curvePoints2$x_coords, curvePoints2$y_coords, minDim)
  minDim <- min(dim(updatedCurvePoints1)[1], dim(updatedCurvePoints2)[1])
  updatedCurvePoints1 <- DouglasPeuckerNbPoints(curvePoints1$x_coords, curvePoints1$y_coords, minDim)
  updatedCurvePoints2 <- DouglasPeuckerNbPoints(curvePoints2$x_coords, curvePoints2$y_coords, minDim)
  
  learnerMarking <- as.matrix(updatedCurvePoints1) #to feed those in procGPA()
  teacherMarking <- as.matrix(updatedCurvePoints2)
  procOPAResult <- procOPAcompiled(teacherMarking, learnerMarking)
  rotationAngleInDegree <- ( procOPAResult$R[1,1] %>% acos )*180/pi
  scale <- procOPAResult$s
  translationMatrix <- matrix(colMeans(procOPAResult$Bhat - scale*learnerMarking %*% procOPAResult$R), ncol=2)/MilimeterToPixel
  colnames(translationMatrix) <- c("X", "Y")
  rownames(translationMatrix) <- c("milimeters")
  TRSD <- MarkingEvaluation(translation = translationMatrix,
                            rotation = rotationAngleInDegree,
                            scale = scale,
                            dissimilarity = procOPAResult$rmsd)
  
  #access components by key "translation", "rotation", and "scale" finally "dissimilarity"
  #using @, for example: TRSD@rotation
  return(TRSD)
})

#This is something you should run on
main <- function(imageA, imageB, ratio=1, outputname="defaultResult.xlsx", doThinning=T) {
  
  TRSD <- getTRSDInMilimeters(imageA, imageB, ratio, doThinning)
  l <- list(translationX = TRSD@translation[1],translationY = TRSD@translation[2],
            rotation = TRSD@rotation, scale = TRSD@scale, dissimilarity = TRSD@dissimilarity)
  openxlsx::write.xlsx(l, file = outputname)
  
}

#by default we do thinning, 
#set the pixel/mm ratio to 1 
#and set output name as "defaultResult.xlsx"
# main("image1_rgb.png", "image2_rgb.png")

#you can customize all three
#main("image4_rgb.png", "image5_rgb.png", 2, "result4x5.xlsx", F) 
#1mm is 2 pixels, we call output as "result4x5.xlsx", and we won't do thinning


