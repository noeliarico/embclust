library(bmp)

# Get image name
get_image_matrix <- function(img, in01 = FALSE, intervals = FALSE) {
  # Load the data
  img <- file.path("imgs", "miniexample", "training", paste0(img, ".bmp"))
  img <- read.bmp(img)
  # There are three matrices of dimension 300x300 (and more info in other attr)
  # Each matrix corresponds with R, G and B colors
  # Each element has the value of each pixel in the corresponding color in [0,255]
  r <- img[1:300,1:300,1]
  colnames(r) <- 1:300
  rownames(r) <- 1:300
  g <- img[1:300,1:300,2]
  colnames(g) <- 1:300
  rownames(g) <- 1:300
  b <- img[1:300,1:300,3]
  colnames(b) <- 1:300
  rownames(b) <- 1:300
  # From matrices to dataframe of columns: row, column, r, g, b
  r <- as.data.frame.table(r)
  g <- as.data.frame.table(g)
  b <- as.data.frame.table(b)
  r$color <- "r"
  g$color <- "g"
  b$color <- "b"
  img <- bind_rows(r, g, b)
  colnames(img)[1:2] <- c("row", "column")
  img <- img %>% pivot_wider(names_from = "color", values_from = "Freq")

  if(in01) {
    img$r <- img$r/255
    img$g <- img$g/255
    img$b <- img$b/255
  }

  if(intervals) {
    f <- function(x) {
      return((1-x)/(1+(.5*x)))
    }
    img$rUP <- 1 - f(img$r)
    img$gUP <- 1 - f(img$g)
    img$bUP <- 1 - f(img$b)
  }
  return(img)
}


img1 <- get_image_matrix("001", in01 = T, intervals = T)


# How to plot the objects:
# https://stackoverflow.com/questions/16787894/need-help-converting-a-bmp-image-to-a-matrix-in-r
