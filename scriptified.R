#!/usr/bin/env Rsrcript
library(openxlsx)
library(magick)
source("getTRSD.R")

args <- commandArgs(trailingOnly = TRUE)

# test if there is at least one argument: if not, return an error
if (length(args) < 2 || length(args) > 3) {
  stop("At least two argument must be specified,  at most three arguments for learnerImage.png teacherImage.png mmPerPixel", call.=FALSE)
} else if (length(args) == 2) {
  image1 <- image_read(args[1])
  image2 <- image_read(args[2])
  
  image_write(image1,"learner_flattened.png", format = "png")  
  image_write(image2,"teacher_flattened.png", format = "png")
  # default output file
  TRSD <- getTRSDInMilimeters("learner_flattened.png", "teacher_flattened.png", 1)
  l <- list(translationX = TRSD@translation[1],translationY = TRSD@translation[2],
            rotation = TRSD@rotation, scale = TRSD@scale, dissimilarity = TRSD@dissimilarity)
  write.xlsx(l, file = "result.xlsx")
} else {
  image1 <- image_read(args[1])
  image2 <- image_read(args[2])
  
  image_write(image1,"learner_flattened.png", format = "png")  
  image_write(image2,"teacher_flattened.png", format = "png")
  TRSD <- getTRSDInMilimeters("learner_flattened.png", "teacher_flattened.png", args[3])
  l <- list(translationX = TRSD@translation[1],translationY = TRSD@translation[2],
            rotation = TRSD@rotation, scale = TRSD@scale, dissimilarity = TRSD@dissimilarity)
  write.xlsx(l, file = "result.xlsx")
  
  
}