#Import all the required R packages.
library(compiler)
library(magrittr)
library(EBImage)
library(shapes)
library(kmlShape)
library(Rcpp)
library(RcppArmadillo)
#these are helper modules written in RcppArmadillo for faster computation
sourceCpp("imageThinning.cpp")
sourceCpp("downSamplePoints.cpp")

#convert rawImagePathRGBA.png -flatten imagePath.png

procGPAcompiled <- cmpfun(procGPA)

getRiemannianDissimilarityDistance <- cmpfun(function(imagePath1, imagePath2) {
  #turn the images intro binary, where the marking is white and the background is black
  thinnedImage1 <- readImage(imagePath1) %>% channel(., "gray") %>% `<`(., 0.5) %>% bwlabel(.) %>% thinImage
  thinnedImage2 <- readImage(imagePath2)%>% channel(., "gray") %>% `<`(., 0.5) %>% bwlabel(.) %>% thinImage
  
  #take the 2d coordinates of the image pixels, only the part where it is surgical marking
  curvePoints1 <- getCurvePoints(thinned1)
  curvePoints2 <- getCurvePoints(thinned2)
  dim1 <- dim(curvePoints1)[1]
  dim2 <- dim(curvePoints2)[1]
  minDim <- min(dim1, dim2)
  #even out the number of points so that they can be comparable with procGPA
  updatedCurvePoints1 <- DouglasPeuckerNbPoints(curvePoints1$x_coords, curvePoints1$y_coords, minDim)
  updatedCurvePoints2 <- DouglasPeuckerNbPoints(curvePoints2$x_coords, curvePoints2$y_coords, minDim)
  arr1 <- as.matrix(updatedCurvePoints1)
  arr2 <- as.matrix(updatedCurvePoints2)
  #combine the two images as one batch of 2 samples
  samples_layered <- array(c(arr1, arr2), dim=c(minDim, 2, 2))
  return(procGPAcompiled(samples_layered, reflect=FALSE))
})