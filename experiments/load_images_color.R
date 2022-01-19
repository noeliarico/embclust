library(bmp)

# Get image name
get_image_data <- function(img,
                             in01 = FALSE) {

  # Color image ----------------------------------------------------------------

  # Load the image
  imgColor <- file.path("imgs", "color", "miniexample", "training", paste0(img, ".bmp"))
  imgColor <- read.bmp(imgColor)
  # There are three matrices of dimension 300x300 (and more info in other attr)
  # Each matrix corresponds with R, G and B colors
  # Each element has the value of each pixel in the corresponding color in [0,255]
  r <- imgColor[1:300,1:300,1]
  colnames(r) <- 1:300
  rownames(r) <- 1:300
  g <- imgColor[1:300,1:300,2]
  colnames(g) <- 1:300
  rownames(g) <- 1:300
  b <- imgColor[1:300,1:300,3]
  colnames(b) <- 1:300
  rownames(b) <- 1:300
  # From matrices to dataframe of columns: row, column, r, g, b
  r <- as.data.frame.table(r)
  g <- as.data.frame.table(g)
  b <- as.data.frame.table(b)
  r$color <- "r"
  g$color <- "g"
  b$color <- "b"
  imgColor <- bind_rows(r, g, b)
  colnames(imgColor)[1:2] <- c("row", "column")
  imgColor <- imgColor %>% pivot_wider(names_from = "color", values_from = "Freq")

  if(in01) {
    imgColor$r <- imgColor$r/255
    imgColor$g <- imgColor$g/255
    imgColor$b <- imgColor$b/255
  }



  # Black and white test image -------------------------------------------------

  # Load the image
  imgTest <- file.path("imgs", "color", "miniexample", "test", paste0(img, ".png"))
  imgTest <- readPNG(imgTest)
  colnames(imgTest) <- 1:300
  rownames(imgTest) <- 1:300
  imgTest <- as.data.frame.table(imgTest)
  colnames(imgTest) <- c("row", "column", "class")
  imgTest$class <- factor(imgTest$class, labels = 1:3)

  # Merge the data -------------------------------------------------------------

  return(inner_join(imgColor, imgTest))
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

img1 <- get_image_data("001", in01 = T)



# How to plot the objects:
# https://stackoverflow.com/questions/16787894/need-help-converting-a-bmp-image-to-a-matrix-in-r
