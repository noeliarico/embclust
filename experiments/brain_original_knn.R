library(tidyverse)
library(caret)
library(FastKNN)

# Load the image
load("imgs/brains/noise0/T2/rdata/normal/55.RData")
img <- brain_noise0T2_55

# Divide between training and testing sets
set.seed(123)
inTrain <- createDataPartition(
  img$class,
  p = .7,
  list = FALSE
)

# Sin intervalos
training <- img[inTrain,"value", drop = FALSE]
training_class <- img[inTrain,"class"]
testing <- img[-inTrain,"value", drop = FALSE]
testing_class <- img[-inTrain,"class"]

n55k3 <- read_csv("experiments/brain/nn55k3.csv", col_names = FALSE)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
get_class <- function(nn, testing_class) {
  get_mode(training_class[nn])
}
apply(head(n55k3), 1, get_class, testing_class)
confusionMatrix(n55k3, testing_class)

# Compute nn and classify
predicted_classes <- vector(mode = "numeric", length = nrow(testing))
k <- 3
for(i in 1:nrow(testing)) {
  m <- Distance_for_KNN_test(testing[i,drop=FALSE], training)
  # m <- sim_emb_obj_train(testing[i,], training, emb_w, mean)
  # x <- training_class[get_nn(1, m, k = k)]
  # predicted_classes[i] <- as.numeric(names(which.max(table(x))))
  # print(i)
}


# Check the results?confusionMatrix()
cm <- caret::confusionMatrix(
  reference = testing_class,
  data = as.factor(predicted_classes),
  mode = "everything")
cm$overall
