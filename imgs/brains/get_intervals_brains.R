get_intervals_brain <- function(img, intervals = "area") {

  if(intervals == "area") {

  }

  if(intervals == "sugeno") {
    f <- function(x) {
      return((1-x)/(1+(.5*x)))
    }
    img$right <- 1 - f(img$value)
    colnames(img)[3] <- "left"
    return(img[,c(1,2,3,5,4)])
  }

  stop("Unkown method for creating intervals")
}

get_intervals_brain(img, "sugeno")

# Export to python
library(reticulate)
np = import("numpy")
pathSugeno <- file.path("imgs", "brains", "noise0", "T2", "numpy", "sugeno", "img")
pathSugenoRData <- file.path("imgs", "brains", "noise0", "T2", "rdata", "sugeno")
for(i in 1:181) {
  img <- get_intervals_brain(
    get_image_data_brain(i, real = T, in01 = T, tidy = T),
    "sugeno")
  # Save R dataframe
  varName <- paste0("brain_noise0T2_sugeno_", i)
  assign(varName, img)
  save(list = varName, file = paste0(pathSugenoRData, "/", i, ".RData"))
  # Save python values
  np$save(paste0(pathSugeno, i, ".npy"), r_to_py(img$right))
}


str(myImage)




interval_data_bf <- function(data) {

}
