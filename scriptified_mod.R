#!/usr/bin/env Rsrcript
source("getTRSD.R")

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  TRSD <- getTRSDInMilimeters(args[1], args[2], args[3])
  l <- list(translationX = TRSD@translation[1],translationY = TRSD@translation[2],
            rotation = TRSD@rotation, scale = TRSD@scale, dissimilarity = TRSD@dissimilarity)
  openxlsx::write.xlsx(l, file = "result.xlsx")
  
  
}
main()