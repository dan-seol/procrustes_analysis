source("getTRSD.R")

main <- function(imageA, imageB, ratio) {
  
  TRSD <- getTRSDInMilimeters(image1, image2, ration)
  l <- list(translationX = TRSD@translation[1],translationY = TRSD@translation[2],
            rotation = TRSD@rotation, scale = TRSD@scale, dissimilarity = TRSD@dissimilarity)
  openxlsx::write.xlsx(l, file = "result.xlsx")
  
}