# Load the image
img1 <- get_image_data("001", in01 = T)
img1 <- get_intervals(img1)

set.seed(123)
# Divide between training and testing
inTrain <- createDataPartition(
  img1$class,
  p = .7,
  list = FALSE
)

# Sin intervalos
# training <- img1[inTrain,3:5]
# training_class <- img1[-inTrain,] %>% pull(class)
# testing <- img1[-inTrain,3:5]
# testing_class <- img1[-inTrain,] %>% pull(class)

# Con intervalos
training <- img1[inTrain,3:8]
training_class <- img1[-inTrain,] %>% pull(class)
testing <- img1[-inTrain,3:8]
testing_class <- img1[-inTrain,] %>% pull(class)

# si cargo aquí la matrix entera quedo sin memoria vectorial

# Compute nn and classify
predicted_classes <- vector(mode = "numeric", length = nrow(testing))
k <- 3
for(i in 1:nrow(testing)) {
  #m <- Distance_for_KNN_test(testing[i,], training)
  m <- sim_emb_obj_train(testing[i,], training, emb_w, mean)
  x <- training_class[get_nn(1, m, k = k)]
  predicted_classes[i] <- as.numeric(names(which.max(table(x))))
  print(i)
}


# Check the results?confusionMatrix()
cm <- caret::confusionMatrix(
  reference = testing_class,
  data = as.factor(predicted_classes),
  mode = "everything")
cm$overall
