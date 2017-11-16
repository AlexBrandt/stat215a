devtools::install_github("rstudio/keras")
library(keras)
library(lattice)
library(ggplot2)
library(caret)
install_keras()
library(pROC)
library(kernlab)
library(MASS)
library(ggpubr)

########################
### Helper functions ###
########################

cleanImage <- function(image, best_feats = TRUE){
  image[, -c(1, 2, 3)] <- scale(image[, -c(1, 2, 3)])
  image_dat <- image[, -c(1, 2)]
  image_dat <- image_dat[image_dat$label != 0, ]
  if (best_feats == TRUE){
    x = image_dat[, 2:4]
    x = as.matrix(x)
  }
  else {
    x = as.matrix(image_dat[, 2:9])
  }
  y = as.matrix(image_dat[, 1])
  y[y == -1] = 0
  return(list(x, y))
}

CalculateTPR <- function(threshhold, preds, truth) {
  # a function to calculate the true positive rate
  # (the number of times we correctly predict 1).
  # Args:
  #   threshold: a value above which the prediction is defined to be 1
  #   preds: a vector of predicted probabilities
  #   truth: a 0, 1 vector of true classes
  # Returns: 
  #   a number between 0 and 1 corresponding to the TPR
  as.numeric(sum(preds[truth] > threshhold) / sum(truth))
}

CalculateFPR <- function(threshold, preds, truth) {
  # a function to calculate the false positive rate 
  # (the number of times we incorrectly predict 1).
  # Args:
  #   threshold: a value above which the prediction is defined to be 1
  #   preds: a vector of predicted probabilities
  #   truth: a 0,1 vector of true classes
  # Returns:
  #   a number between 0 and 1 corresponding to the FPR
  as.numeric(sum(preds[!truth] > threshold) / sum(!truth))
}

# Show summary statistics, ROC curve, and AUC for model
EvaluateModel <- function(preds, truth) {
  # Print an ROC curve and return the AUC for a fitted model.
  # Args:
  #  preds: numeric predictions on the held out data
  #  truth: true classifications of the held out data (same length as preds)
  
  # Plot an ROC curve
  
  cat("TPR at threshold of .5: ", CalculateTPR(.5, preds, truth), "\n")
  cat("FPR at threshold of .5: ", CalculateFPR(.5, preds, truth), "\n")
  
  # Get the TPRs and FPRs for all possible thresholds.  
  # (Note that this can be done more efficiently for large data sets.  
  # See "An introduction to ROC analysis" by Fawcett.)
  
  # the following code calculates the true positive rate for 1000 threshold 
  # values thresholds between 0 and 1
  tprs <- sapply(seq(0, 1, length.out = 1000), 
                 FUN = CalculateTPR, 
                 preds, truth)
  
  # the following code calculates the false positive rate for 1000 threshold 
  # values thresholds between 0 and 1
  fprs <- sapply(seq(0, 1, length.out = 1000), 
                 FUN = CalculateFPR, 
                 preds, truth)
  
  # plot an ROC curve for the model 
  plot <- ggplot(data = data.frame(tprs, fprs)) +
    geom_line(aes(x = fprs, y = tprs), color = "cornflowerblue") +
    geom_abline(aes(slope = 1, intercept = 0)) +
    labs(x = "False positive rate", 
         y = "True positive rate",
         title = 'ROC Curve')
  theme_classic()
  
  # Calculate the AUC
  
  # first calculate the TPR using each of the true negative (group 0) predicted 
  # probabilities as a threshold
  roc.jumps <-
    sapply(preds[!truth],
           FUN = function(threshold) { 
             CalculateTPR(threshold, preds, truth) 
           })
  # calculate the average of these false positive rates
  auc <- sum(roc.jumps) / sum(!truth)
  cat("AUC: ", auc, "\n")
  
  # return(roc.jumps)
  return(plot)
}

neuralNetwork <- function(n_hidden = 128,
                          dropout = 0.3){
  model <- keras_model_sequential() 
  model %>% 
    layer_dense(units = 3, activation = 'relu', input_shape = c(3)) %>% 
    layer_dense(units = n_hidden, activation = 'relu') %>%
    layer_dropout(rate = dropout) %>%
    layer_dense(units = 1, activation = 'sigmoid')
  
  model %>% compile(
    loss = 'binary_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )
  return(model)
}




#########################
### Individual Images ###
#########################
# Get the data for three images
path <- "data"
image1 <- read.table(paste0('image_data/', 'image1.txt'), header = F)
image2 <- read.table(paste0('image_data/', 'image2.txt'), header = F)
image3 <- read.table(paste0('image_data/', 'image3.txt'), header = F)

# Add informative column names.
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs

im1 = cleanImage(image1)
im1x = im1[[1]]
im1y = im1[[2]]

im2 = cleanImage(image2)
im2x = im2[[1]]
im2y = im2[[2]]

im3 = cleanImage(image3)
im3x = im3[[1]]
im3y = im3[[2]]

model <- neuralNetwork()
history <- model %>% fit(
  im1x, im1y, 
  epochs = 20, batch_size = 128, 
  validation_split = 0.0001,
) # 94.30% 
 
model %>% evaluate(im2x, im2y) # 88.17%
model %>% evaluate(im3x, im3y) # 75.83%

# Re-run model above!!!
model <- neuralNetwork()
history <- model %>% fit(
  im2x, im2y, 
  epochs = 20, batch_size = 128, 
  validation_split = 0.0001,
) # 96.75%

model %>% evaluate(im1x, im1y) # 75.58%
model %>% evaluate(im3x, im3y) # 82.21%

# Re-run model above!!!
model <- neuralNetwork()
history <- model %>% fit(
  im3x, im3y, 
  epochs = 20, batch_size = 128, 
  validation_split = 0.0001,
) # 87.0%

model %>% evaluate(im1x, im1y) # 78.33%
model %>% evaluate(im2x, im2y) # 92.91%

##################
### All Images ###
##################

# Goal     : create good folds for CV
# Heuristic: get equal representation of 
#            classes and images in each fold

customCV <- function(n_folds = 5, n_hidden = 128){
  test_acc <- c()
  cat("Number of hidden units:", n_hidden, "\n")
  
  # Create n_folds number of folds for each image
  folds1 <- createFolds(im1y, k = n_folds)
  folds2 <- createFolds(im2y, k = n_folds)
  folds3 <- createFolds(im3y, k = n_folds)
  
  for (i in 1:n_folds){
    test1_x <- im1x[folds1[[i]], ]
    test2_x <- im2x[folds2[[i]], ]
    test3_x <- im3x[folds3[[i]], ]
    train1_x <- im1x[-folds1[[i]], ]
    train2_x <- im2x[-folds2[[i]], ]
    train3_x <- im3x[-folds3[[i]], ]
    test1_y <- im1y[folds1[[i]], ]
    test2_y <- im2y[folds2[[i]], ]
    test3_y <- im3y[folds3[[i]], ]
    train1_y <- im1y[-folds1[[i]], ]
    train2_y <- im2y[-folds2[[i]], ]
    train3_y <- im3y[-folds3[[i]], ]
  
    x_train <- rbind(train1_x, train1_x, train3_x)
    x_test <- rbind(test1_x, test2_x, test3_x)
    y_train <- c(train1_y, train1_y, train3_y)
    y_test <- c(test1_y, test2_y, test3_y)
    
    model = neuralNetwork(n_hidden)
    cat("Holding out fold", i, "... \n")
    history <- model %>% fit(
      x_train, y_train, 
      epochs = 10, batch_size = 128, 
      validation_split = 0.00001,
      verbose = 0,
    )
    results <- model %>% evaluate(x_test, y_test, verbose = 0)
    cat(">>> Test accuracy:", results$acc, "\n")
    test_acc <- c(test_acc, results$acc)
  }
  cat("CV score:", mean(test_acc), "-- SD:", sd(test_acc), "\n")
}

customCV(n_folds = 10, n_hidden = 8)    # CV score: 89.33%, SD: 0.87%
customCV(n_folds = 10, n_hidden = 32)   # CV score: 90.40%, SD: 0.27%
customCV(n_folds = 10, n_hidden = 128)  # CV score: 90.50%, SD: 0.25%
customCV(n_folds = 10, n_hidden = 512)  # CV score: 90.61%, SD: 0.28%
customCV(n_folds = 10, n_hidden = 1024) # CV score: 90.45%, SD: 0.21%

### Best model: 512 hidden units

##################
### ROC Curves ###
##################

set.seed(215)
# Prepare class-balanced folds
makeFolds <- function(x, y, cut = 0.8){
  # Separate classes
  clouds <- which(y == 1)
  non_clouds <- which(y == 0)
  clouds_x <- x[clouds, ]
  non_clouds_x <- x[non_clouds, ]
  clouds_y <- y[clouds, ]
  non_clouds_y <- y[non_clouds, ]
  
  # Create seperate splits for clouds and non-clouds
  cloud_indies <- sample(1:length(clouds))
  non_clouds_indies <- sample(1:length(non_clouds))
  cloud_split <- round(length(clouds) * cut)
  non_cloud_split <- round(length(non_clouds) * cut)
  
  # Create training and test folds for each class
  clouds_x_train <- clouds_x[0:cloud_split, ]
  clouds_y_train <- clouds_y[0:cloud_split]
  clouds_x_test <- clouds_x[cloud_split:length(clouds), ]
  clouds_y_test <- clouds_y[cloud_split:length(clouds)]
  non_clouds_x_train <- non_clouds_x[0:non_cloud_split, ]
  non_clouds_y_train <- non_clouds_y[0:non_cloud_split]
  non_clouds_x_test <- non_clouds_x[non_cloud_split:length(non_clouds), ]
  non_clouds_y_test <- non_clouds_y[non_cloud_split:length(non_clouds)]
  
  # Rejoin classes
  x_train <- rbind(clouds_x_train, non_clouds_x_train)
  x_test <- rbind(clouds_x_test, non_clouds_x_test)
  y_train <- c(clouds_y_train, non_clouds_y_train)
  y_test <- c(clouds_y_test, non_clouds_y_test)
  dat <- list(x_train, x_test, y_train, y_test)
  
  return(dat)
}
im1_split <- makeFolds(im1x, im1y, cut = 0.8)
im2_split <- makeFolds(im2x, im2y, cut = 0.8)
im3_split <- makeFolds(im3x, im3y, cut = 0.8)
x_train <- rbind(im1_split[[1]], im2_split[[1]], im3_split[[1]])
x_test <- rbind(im1_split[[2]], im2_split[[2]], im3_split[[2]])
y_train <- c(im1_split[[3]], im2_split[[3]], im3_split[[3]])
y_test <- c(im1_split[[4]], im2_split[[4]], im3_split[[4]])

model <- neuralNetwork(n_hidden = 512,
                       dropout = 0.3)
history <- model %>% fit(
  x_train, y_train, 
  epochs = 20, batch_size = 128,
  validation_data = list(x_test, y_test),
  verbose = 1,
)

model %>% evaluate(x_test, y_test)
preds <- model %>% predict_classes(x_test)
scores <- model %>% predict(x_test)

neural_eval <- EvaluateModel(c(scores), y_test)
confusionMatrix(preds, y_test)

#########################
### Train on 2 images ###
### Predict the third ###
#########################

# Test on image 1
model <- neuralNetwork(n_hidden = 512)
x_train <- rbind(im2x, im3x)
y_train <- rbind(im2y, im3y)
history <- model %>% fit(
  x_train, y_train, 
  epochs = 10, batch_size = 128, 
  validation_split = 0.00001,
  verbose = 0,
)
model %>% evaluate(im1x, im1y, verbose = 0) # 81.51%
im1_preds <- model %>% predict_classes(im1x)

# Test on image 2
model <- neuralNetwork(n_hidden = 512)
x_train <- rbind(im1x, im3x)
y_train <- rbind(im1y, im3y)
history <- model %>% fit(
  x_train, y_train, 
  epochs = 10, batch_size = 128, 
  validation_split = 0.25,
  verbose = 0,
)
model %>% evaluate(im2x, im2y, verbose = 0) # 94.78%
im2_preds <- model %>% predict_classes(im2x) 

# Test on image 3
model <- neuralNetwork(n_hidden = 512)
x_train <- rbind(im1x, im2x)
y_train <- rbind(im1y, im2y)
history <- model %>% fit(
  x_train, y_train, 
  epochs = 10, batch_size = 128, 
  validation_split = 0.00001,
  verbose = 0,
)
model %>% evaluate(im3x, im3y, verbose = 0) # 81.34%
im3_preds <- model %>% predict_classes(im3x)

##################################
### Plot predictions vs. truth ###
##################################

im1 <- image1[image1$label != 0, ]
im1$preds <- im1_preds
im1$correct <- im1_preds == im1y

im2 <- image2[image2$label != 0, ]
im2$preds <- im2_preds
im2$correct <- im2_preds == im2y

im3 <- image1[image3$label != 0, ]
im3$preds <- im3_preds
im3$correct <- im3_preds == im3y

p1 <- ggplot(im1) + geom_point(aes(x = x, y = y, color = factor(correct))) +
  scale_color_manual(name = "Expert label", breaks = c(TRUE, FALSE),
                     labels = c("Correct", 'Incorrect'),
                     values=c("#56B4E9", "#999999")) +
  theme_classic() +
  labs(title = "Image 1")

p2 <- ggplot(im2) + geom_point(aes(x = x, y = y, color = factor(correct))) +
  scale_color_manual(name = "Expert label", breaks = c(TRUE, FALSE),
                     labels = c("Correct", 'Incorrect'),
                     values=c("#56B4E9", "#999999")) +
  theme_classic() +
  labs(title = "Image 2")

p3 <- ggplot(im3) + geom_point(aes(x = x, y = y, color = factor(correct))) +
  scale_color_manual(name = "Expert label", breaks = c(TRUE, FALSE),
                     labels = c("Correct", 'Incorrect'),
                     values=c("#56B4E9", "#999999")) +
  theme_classic() +
  labs(title = "Image 3")

d <- grid_arrange_shared_legend(p1, p2, p3)
ggsave("preds_vs_truth.png", d, dpi = 300, width = 10, height = 4)



##############################
### Question 5             ###
##############################

analysis_plot <- function(data, plot_type = 12){
  p1 <- ggplot(data) + 
    geom_density(aes(x = data[,4], group = factor(data[,plot_type]), fill = factor(data[,plot_type])), 
                 alpha = 0.5) +
    scale_fill_discrete(name = "Classification")
  p1 <- p1 + xlab("NDAI")
  p2 <- ggplot(data) + 
    geom_density(aes(x = data[,5], group = factor(data[,plot_type]), fill = factor(data[,plot_type])), 
                 alpha = 0.5) +
    scale_fill_discrete(name = "Classification")
  p2 <- p2 + xlab("SD")
  p3 <- ggplot(data) + 
    geom_density(aes(x = data[,6], group = factor(data[,plot_type]), fill = factor(data[,plot_type])), 
                 alpha = 0.5) +
    scale_fill_discrete(name = "Classification")
  p3 <- p3 + xlab("CORR")
  p4 <- ggplot(data) + 
    geom_density(aes(x = data[,7], group = factor(data[,plot_type]), fill = factor(data[,plot_type])), 
                 alpha = 0.5) +
    scale_fill_discrete(name = "Classification")
  p4 <- p4 + xlab("DF")
  p5 <- ggplot(data) + 
    geom_density(aes(x = data[,8], group = factor(data[,plot_type]), fill = factor(data[,plot_type])), 
                 alpha = 0.5) +
    scale_fill_discrete(name = "Classification")
  p5 <- p5 + xlab("CF")
  p6 <- ggplot(data) + 
    geom_density(aes(x = data[,9], group = factor(data[,plot_type]), fill = factor(data[,plot_type])), 
                 alpha = 0.5) +
    scale_fill_discrete(name = "Classification")
  p6 <- p6 + xlab("BF")
  p7 <- ggplot(data) + 
    geom_density(aes(x = data[,10], group = factor(data[,plot_type]), fill = factor(data[,plot_type])), 
                 alpha = 0.5) +
    scale_fill_discrete(name = "Classification")
  p7 <- p7 + xlab("AF")
  p8 <- ggplot(data) + 
    geom_density(aes(x = data[,11], group = factor(data[,plot_type]), fill = factor(data[,plot_type])), 
                 alpha = 0.5) +
    scale_fill_discrete(name = "Classification")
  p8 <- p8 + xlab("AN")  
  ggarrange(p1, p2, p3, p4, p5, p6, p7, p8 + rremove("x.text"), 
            labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
            ncol = 2, nrow = 4)
}


im_total <- rbind(im1,im2,im3)
analysis_plot(im_total,13)

#general analysis
im_false <- im_total[im_total$correct == FALSE,]
analysis_plot(im_false,12)

im_true <- im_total[im_total$correct == TRUE,]
analysis_plot(im_true,12)

#ks.test
ks.test(im_false_negative$NDAI,im_false_positive$NDAI)

im_false_negative <- im_false[im_false$preds == 0,]
im_false_positive <- im_false[im_false$preds == 1,]
im_true_positive <- im_true[im_true$preds == 1,]
im_true_negative <- im_true[im_true$preds == 0,]

im1_false <- im1[im1$correct == FALSE,]
im2_false <- im2[im2$correct == FALSE,]
im3_false <- im3[im3$correct == FALSE,]
ggplot(im1) + geom_point(aes(x = x, y = y, color = AN)) + 
  geom_point(data = im1_false[im1_false$preds == 1,], aes(x = x, y = y), color = "red", shape = 15,size = 1, alpha = 0.5) +
  geom_point(data = im1_false[im1_false$preds == 0,], aes(x = x, y = y), color = "blue", shape = 15, size = 1, alpha = 0.5)
ggplot(im2) + geom_point(aes(x = x, y = y, color = AN)) + 
  geom_point(data = im2_false[im2_false$preds == 1,], aes(x = x, y = y), color = "red", shape = 15, size = 1, alpha = 0.5) +
  geom_point(data = im2_false[im2_false$preds == 0,], aes(x = x, y = y), color = "blue", shape = 15, size = 1, alpha = 0.5)
ggplot(im3) + geom_point(aes(x = x, y = y, color = AN)) + 
  geom_point(data = im3_false[im3_false$preds == 1,], aes(x = x, y = y), color = "red", shape = 15, size = 1, alpha = 0.5) +
  geom_point(data = im3_false[im3_false$preds == 0,], aes(x = x, y = y), color = "blue", shape = 15, size = 1, alpha = 0.5)
# number of cells (115229, 115110, 115217)



##############################
### Train your own network ###
##############################

model <- neuralNetwork(512)
history <- model %>% fit(
  x = # TRAINING X, 
  y = # TRAINING Y, 
  epochs = # NUM EPOCHS,
  batch_size = # BATCH SIZE, 
  validation_split = # VAlIDATION SPLIT,
  verbose = 0, # 1 = PRINT STUFF
)

# Evaluate test accuracy
model %>% evaluate(# TEST X, # TEST Y)
  
# Predict test x
predictions <- model %>% predict_classes(# TEST X)
  

