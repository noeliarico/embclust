library(bmp)
library(tidyverse)

path <- file.path("imgs", "brains", "noise0", "T2", "T2")
pathReal <- file.path("imgs", "brains", "real", "Segmentación_")

# Get image by its name
get_image_data_brain <- function(img,
                             real = F,
                             in01 = FALSE,
                             tidy = FALSE) {

  imgColor <- paste0(pathReal, img, ".bmp")
  img <- paste0(path, "_Corte_", img, "_0_0.bmp")

  # BW image -------------------------------------------------------------------

  # Load the image

  img <- read.bmp(img)
  # Get the matrix of the image
  imgM <- img[1:(dim(img)[1]),1:(dim(img)[2])]
  rownames(imgM) <- 1:(dim(img)[1])
  colnames(imgM) <- 1:(dim(img)[2])

  # Normalize in range [0,1]
  if(in01) {
    imgM <- imgM/255
  }

  # Create a data frame with the row and column of the pixel and its value
  if(tidy) {
    imgM <- as.data.frame.table(imgM)
    colnames(imgM) <- c("row", "column", "value")
  }

  if(real) {
    imgColor <- read.bmp(imgColor)
    # There are three matrices of dimension 300x300 (and more info in other attr)
    # Each matrix corresponds with R, G and B colors
    # Each element has the value of each pixel in the corresponding color in [0,255]
    color <- imgColor[1:dim(imgColor)[1],1:dim(imgColor)[2]]
    colnames(color) <- 1:dim(imgColor)[2]
    rownames(color) <- 1:dim(imgColor)[1]

    if(tidy) {
      color <- as.data.frame.table(color)
      colnames(color) <- c("row", "column", "class")
      return(inner_join(imgM, color))
    } else {

      return(list(image = imgM, class = color))
    }
  } else {
    return(imgM)
  }

}

# Example of how to get the data
img <- get_image_data_brain(55, real = T, tidy = T)

################################################################################

# Export values to python to perform the pairwise comparison
library(reticulate)
np = import("numpy")
pathNp <- file.path("imgs", "brains", "noise0", "T2", "numpy", "value", "img")
pathRData <- file.path("imgs", "brains", "noise0", "T2", "rdata")
for(i in 1:181) {
  img <- get_image_data_brain(i, real = T, in01 = T, tidy = T)
  # Save R dataframe
  varName <- paste0("brain_noise0T2_", i)
  assign(varName, img)
  save(list = varName, file = paste0(pathRData, "/", i, ".RData"))
  # Save python values
  np$save(paste0(pathNp, i, ".npy"), r_to_py(img$value))
}

library(reticulate)
np = import("numpy")
np$save(paste0("experiments/brain/training55", ".npy"), r_to_py(training$value))
np$save(paste0("experiments/brain/testing55", ".npy"), r_to_py(testing$value))



# How to plot the objects:
# https://stackoverflow.com/questions/16787894/need-help-converting-a-bmp-image-to-a-matrix-in-r
