library(bmp)

# Get image name
get_image_data_bw <- function(img,
                             in01 = FALSE,
                             tidy = FALSE) {

  # Color image ----------------------------------------------------------------

  # Load the image
  img <- file.path("imgs", "bw", "all", "noise0", paste0("PD_Corte_", img, "_0_0.bmp"))
  img <- read.bmp(img)
  imgM <- img[1:(dim(img)[1]),1:(dim(img)[2])]

  if(in01) {
    imgM <- imgM/255
  }

  if(tidy) {
    rownames(imgM) <- 1:(dim(img)[1])
    colnames(imgM) <- 1:(dim(img)[2])
    imgM <- as.data.frame.table(imgM)
    colnames(imgM) <- c("row", "column", "value")
  }

  return(imgM)
}

get_intervals <- function(img, intervals = "area") {

  if(intervals == "area") {
    f <- function(x) {
      return((1-x)/(1+(.5*x)))
    }
    img$rUP <- 1 - f(img$r)
    img$gUP <- 1 - f(img$g)
    img$bUP <- 1 - f(img$b)
    return(img[,c(1,2,3,7,4,8,5,9,6)])
  }

  if(intervals == "agus") {
    f <- function(x) {
      return((1-x)/(1+(.5*x)))
    }
    img$rUP <- 1 - f(img$r)
    img$gUP <- 1 - f(img$g)
    img$bUP <- 1 - f(img$b)
    return(img[,c(1,2,3,7,4,8,5,9,6)])
  }

  stop("Unkown method for creating intervals")
}


str(myImage)




interval_data_bf <- function(data) {

}

library(reticulate)
np = import("numpy")
for(i in 1:181) {
  img <- get_image_data_bw(i, in01 = T, tidy = F)
  np$save(paste0("img", i, ".npy"), r_to_py(img))
}





# How to plot the objects:
# https://stackoverflow.com/questions/16787894/need-help-converting-a-bmp-image-to-a-matrix-in-r
