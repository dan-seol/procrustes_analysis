#@author: Dan Yunheum Seol<yunheum.seol@mail.mcgill.ca> <yunheum.seol.conseolting@gmail.com>
#Import all the required R packages.
library(compiler)
library(magrittr)
library(EBImage)
library(shapes)
library(kmlShape)
library(Rcpp)
library(RcppArmadillo)
library(XML)
library(stringr)
setwd("C:/Users/didier1708/Documents/GitHub/procrustes_analysis")
#these are helper modules written in RcppArmadillo for faster computation
#use the command below on terminal to get svg from png
#./png2svg.sh IMG_0028.png


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


collectPointsFromSvgPath <- cmpfun(function(svgPath) {
  svg <- htmlParse(svgPath)
  p <- xpathSApply(svg, "//path", xmlGetAttr, "d")
  p <- str_replace_all(p, "z", "")
  p <- str_replace_all(p, "c", "")
  p <- str_replace_all(p, "l", "")
  # Convert them to numbers
  p <- lapply(str_split(p, " "), as.numeric) %>% unlist %>% matrix(., ncol = 2, byrow = T)
  return(p)
  
})

collectPointsFromCoordinator <- cmpfun(function(path) {
  points <- read.csv(path, header=T)
})

getTRSDInMilimetersFromCoordinator <- cmpfun(function(points1, points2, MilimeterToPixel=1) {
  p1 <- collectPointsFromCoordinator(points1)
  p2 <- collectPointsFromCoordinator(points2)
 
  dim1 <- dim(p1)[1]
  dim2 <- dim(p2)[1]
  mindim <- min(dim1, dim2)
  #even out the number of points so that they can be comparable with procGPA
  updatedCurvePoints1 <- DouglasPeuckerNbPoints(p1$x, p1$y, mindim)
  updatedCurvePoints2 <- DouglasPeuckerNbPoints(p2$x, p2$y, mindim)
  mindim <- min(dim(updatedCurvePoints1)[1], dim(updatedCurvePoints2)[1])
  updatedCurvePoints1 <- DouglasPeuckerNbPoints(p1$x, p1$y, mindim)
  updatedCurvePoints2 <- DouglasPeuckerNbPoints(p2$x, p2$y, mindim)
  
  learnerMarking <- as.matrix(updatedCurvePoints1) #to feed those in procGPA()
  teacherMarking <- as.matrix(updatedCurvePoints2)
  procOPAResult <- procOPAcompiled(teacherMarking, learnerMarking, reflect = TRUE)
  rotationAngleInDegree <- ( procOPAResult$R[1,1] %>% acos )*180/pi
  plotshapes(teacherMarking, learnerMarking, joinline=1:13)
  plotshapes(procOPAResult$Ahat, procOPAResult$Bhat, joinline=1:13)
  scale <- procOPAResult$s 
  learnerMarkingCentered <- t(t(learnerMarking) - colMeans(teacherMarking))
  translationMatrix <- colMeans(procOPAResult$Bhat - (scale*learnerMarkingCentered %*% procOPAResult$R))/MilimeterToPixel %>% matrix(., ncol=2)
  plotshapes(procOPAResult$Bhat, scale*learnerMarkingCentered %*% procOPAResult$R)
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
mainSvg <- cmpfun(function(points1, points2, ratio=1, outputname="defaultResultFromCoord.xlsx") {
  
  TRSD <- getTRSDInMilimetersFromCoordinator(points1, points2, ratio)
  l <- list(translationX = TRSD@translation[1],translationY = TRSD@translation[2],
            rotation = TRSD@rotation, scale = TRSD@scale, dissimilarity = TRSD@dissimilarity)
  openxlsx::write.xlsx(l, file = outputname)
  
})


#by default we do thinning, 
#set the pixel/mm ratio to 1 
#and set output name as "defaultResult.xlsx"
#main("image1_rgb.png", "image2_rgb.png")

#you can customize all three
#main("image4_rgb.png", "image5_rgb.png", 2, "result4x5.xlsx", F) 
#1mm is 2 pixels, we call output as "result4x5.xlsx", and we won't do thinning

#filenames <- c("new_example_batch/IMG_0028.csv", "new_example_batch/IMG_0029.csv", "new_example_batch/IMG_0030.csv", "new_example_batch/IMG_0031.csv", "new_example_batch/IMG_0032.csv")
#mainSvg(filenames[1], filenames[1], outputname="28vs28.csv")
#mainSvg(filenames[1], filenames[2], outputname="28vs29.csv")
#mainSvg(filenames[1], filenames[3], outputname="28vs30.csv")
#mainSvg(filenames[1], filenames[4], outputname="28vs31.csv")
#mainSvg(filenames[1], filenames[5], outputname="28vs32.csv")
mainSvg("28_300.csv", "31_300.csv", outputname="28vs31_300.csv")

#mainSvg("image1_rgb.csv", "image2_rgb.csv", outputname="image1Ximage2.csv")
