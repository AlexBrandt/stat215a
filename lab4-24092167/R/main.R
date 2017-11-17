source('model.R')
source('model_selection.R')
library(caret)
library(pROC)

########################
### Helper functions ###
########################

"""
Function: cleanImage
Input   : a single image from the lab 4 data
Output  : a cleaned dataframe with the best features

"""
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

"""
Function: acc
Input   : predictions and true values
Output  : accuracy of the predictions
"""
acc <- function(preds, truth){
  score <- sum(preds == truth) / length(truth)
  return(score)
}

"""
Function: scoresToPreds
Input   : scores - probability scores from a 0-1 model's predictions
        : thresh - threshold for class cut-off
Output  : a vector of 0-1 predictions

"""
scoresToPreds <- function(scores, thresh = 0.1){
  preds <- scores > thresh
  return(1*preds)
}

############################
### Feature correlations ###
############################

# Get the data for three images
path <- "data"
image1 <- read.table(paste0('data/', 'image1.txt'), header = F)
image2 <- read.table(paste0('data/', 'image2.txt'), header = F)
image3 <- read.table(paste0('data/', 'image3.txt'), header = F)

# Add informative column names.
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs

# Get rid of unlabled data
im1 <- image1[image1$label != 0, ]
im2 <- image2[image2$label != 0, ]
im3 <- image3[image3$label != 0, ]
feature_cors <- data.frame()

# Calculate correlations for the numeric features for each image
for (i in 4:11){
  feat <- colnames(im1)[i]
  im1_cor <- cor(im1[, i], im1[, 3])
  im2_cor <- cor(im2[, i], im2[, 3])
  im3_cor <- cor(im3[, i], im3[, 3])
  feat_cor <- c(im1_cor, im2_cor, im3_cor)
  feature_cors <- rbind(feature_cors, feat_cor)
}
feature_cors <- t(feature_cors)
colnames(feature_cors) <- colnames(im1[4:11])
rownames(feature_cors) <- c("Image 1", "Image 2", "Image 3")
feature_cors


#########################
### Individual Images ###
#########################
### TRAIN ON 1 IMAGE, TEST ON OTHER 2
### DO THIS FOR FOUR DIFFERENT MODELS

im1 = cleanImage(image1)
im1x = im1[[1]] # extract features
im1y = im1[[2]] # extract labels

im2 = cleanImage(image2)
im2x = im2[[1]] # extract features
im2y = im2[[2]] # extract labels

im3 = cleanImage(image3)
im3x = im3[[1]] # extract features
im3y = im3[[2]] # extract labels

##### Image 1 ##### 

# LOGISTIC REGRESSION

# Train a logistic regression model
model <- logReg(im1x, im1y)
# Training accuracy
train_preds <- as.numeric(predict(model, newx = im1x, 
                       s = 0.01, type = 'class'))
acc(train_preds, im1y) # 0.9157
# Accuracy on image 2
im2_preds <- as.numeric(predict(model, newx = im2x,
                                s = 0.01, type = 'class'))
acc(im2_preds, im2y) # 83.41%
# Accuracy on image 3
im3_preds <- as.numeric(predict(model, newx = im3x,
                                s = 0.01, type = 'class'))
acc(im3_preds, im3y) # 73.20%

# QDA

# Train a QDA model
model <- QDA(im1x, im1y)
# Training accuracy
train_preds <- predict(model, im1x)$class 
acc(train_preds, im1y) # 92.19%
# Accuracy on image 2
im2_preds <- predict(model, im2x)$class
acc(im2_preds, im2y) # 84.00%
# Accuracy on image 3
im3_preds <- predict(model, im3x)$class
acc(im3_preds, im3y) # 74.50%

# RANDOM FOREST

# Train a rf model
model <- rf(im1x, as.factor(im1y), ntree = 200)
# Training accuracy
train_preds <- predict(model, im1x)
acc(train_preds, im1y) # 100%
# Accuracy on image 2
im2_preds <- predict(model, im2x)
acc(im2_preds, im2y) # 89.20%
# Accuracy on image 3
im3_preds <- predict(model, im3x)
acc(im3_preds, im3y) # 77.19%

# NEURAL NETWORK

model <- neuralNetwork()
history <- model %>% fit(
  im1x, im1y, 
  epochs = 20, batch_size = 128, 
  validation_split = 0.0001,
) # 94.30% 

model %>% evaluate(im2x, im2y) # 88.17%
model %>% evaluate(im3x, im3y) # 75.83%

##### Image 2 #####

# LOGISTIC REGRESSION

# Train a logistic regression model
model <- logReg(im2x, im2y)
# Training accuracy
train_preds <- as.numeric(predict(model, newx = im2x, 
                                  s = 0.01, type = 'class'))
acc(train_preds, im2y) # 0.95.98%
# Accuracy on image 1
im1_preds <- as.numeric(predict(model, newx = im1x,
                                s = 0.01, type = 'class'))
acc(im1_preds, im1y) # 82.85%
# Accuracy on image 3
im3_preds <- as.numeric(predict(model, newx = im3x,
                                s = 0.01, type = 'class'))
acc(im3_preds, im3y) # 81.59%

# QDA

# Train a QDA model
model <- QDA(im2x, im2y)
# Training accuracy
train_preds <- predict(model, im2x)$class 
acc(train_preds, im2y) # 95.37%
# Accuracy on image 1
im1_preds <- predict(model, im1x)$class
acc(im1_preds, im1y) # 90.09%
# Accuracy on image 3
im3_preds <- predict(model, im3x)$class
acc(im3_preds, im3y) # 82.46%

# RANDOM FOREST

# Train a rf model
model <- rf(im2x, as.factor(im2y), ntree = 300)
# Training accuracy
train_preds <- predict(model, im2x)
acc(train_preds, im2y) # 100%
# Accuracy on image 1
im1_preds <- predict(model, im1x)
acc(im1_preds, im1y) # 75.35%
# Accuracy on image 3
im3_preds <- predict(model, im3x)
acc(im3_preds, im3y) # 82.19%

# NEURAL NETWORK

model <- neuralNetwork(n_hidden = 32)
history <- model %>% fit(
  im2x, im2y, 
  epochs = 10, batch_size = 128, 
  validation_split = 0.0001,
) # 96.52%

model %>% evaluate(im1x, im1y) # 82.65%
model %>% evaluate(im3x, im3y) # 82.00%

##### Image 3 #####

# LOGISTIC REGRESSION

# Train a logistic regression model
model <- logReg(im3x, im3y)
# Training accuracy
train_preds <- as.numeric(predict(model, newx = im3x, 
                                  s = 0.01, type = 'class'))
acc(train_preds, im3y) # 0.8211
# Accuracy on image 2
im2_preds <- as.numeric(predict(model, newx = im2x,
                                s = 0.01, type = 'class'))
acc(im2_preds, im2y) # 94.51%
# Accuracy on image 1
im1_preds <- as.numeric(predict(model, newx = im1x,
                                s = 0.01, type = 'class'))
acc(im1_preds, im1y) # 87.29%

# QDA

# Train a QDA model
model <- QDA(im3x, im3y) 
# Training accuracy
train_preds <- predict(model, im3x)$class 
acc(train_preds, im3y) # 82.00%
# Accuracy on image 2
im2_preds <- predict(model, im2x)$class
acc(im2_preds, im2y) # 94.53%
# Accuracy on image 1
im1_preds <- predict(model, im1x)$class
acc(im1_preds, im1y) # 90.17%

# RANDOM FOREST

# Train a rf model
model <- rf(im3x, as.factor(im3y), ntree = 200)
# Training accuracy
train_preds <- predict(model, im3x)
acc(train_preds, im3y) # 100%
# Accuracy on image 2
im2_preds <- predict(model, im2x)
acc(im2_preds, im2y) # 92.55%
# Accuracy on image 1
im1_preds <- predict(model, im1x)
acc(im1_preds, im1y) # 76.82%

# NEURAL NETWORK

model <- neuralNetwork(n_hidden = 512)
history <- model %>% fit(
  im3x, im3y, 
  epochs = 10, batch_size = 128, 
  validation_split = 0.0001,
) # 86.35%

model %>% evaluate(im1x, im1y) # 82.23%
model %>% evaluate(im2x, im2y) # 93.93%


##############################
### Hyper-parameter tuning ###
##############################
"""
function: tuneHyper
input   : n_folds - number of CV folds
        : type - type of 0-1 classifier (3 options)
        : params - vector of hyper-parameter values to test
output  : data frame with CV scores for each param value
"""
tuneHyper <- function(n_folds = 10, type = 'logistic',
                      params = None){
  results <- c()
  for (param in params){
    if (type == 'logistic'){
      cat("************ lambda = ", param, "\n")
      res <- customCV(n_folds = n_folds, type = 'logistic',
               s = param)
    }
    else if (type == 'neural'){
      cat("************ n_hidden = ", param, "\n")
      res <- customCV(n_folds = n_folds, type = 'neural',
               n_hidden = param)
    }
    else{
      cat("************ n_trees = ", param, "\n")
      res <- customCV(n_folds = n_folds, type = 'rf',
               n_tree = param)
    }
    results <- c(results, res)
  }
  dat <- data.frame(params, results)
  best_param <- dat[which.max(dat$results), 1]
  cat("The best hyper-parameter value is:", best_param, "\n")
  return(dat)
} 

### Tune logistic
lr_hyper <- tuneHyper(n_folds = 5, type = 'logistic',
          params = c(0.1, 0.01, 0.001, 0.0001, 0.00001)) # best: 0.00001

### Tune neural network
nn_hyper <- tuneHyper(n_folds = 5, type = 'neural',
          params = c(8, 16, 64, 128, 512)) # best: 128

### Tune random forest
rf_hyper <- tuneHyper(n_folds = 5, type = 'rf',
          params = c(50, 100, 150, 200, 250))

########################
### Cross-validation ###
########################

### Logistic
customCV(n_folds = 10, type = 'logistic')
# CV score: 0.8606 +/- 0.002

### QDA
customCV(n_folds = 10, type = 'qda')
# CV score: 0.8581 +/- 0.002

### Random forest
customCV(n_folds = 10, type = 'rf', n_tree = 150)
# CV score: 0.9040 +/- 0.002

### Neural network
customCV(n_folds = 10, type = 'neural', n_hidden = 128)
# CV score: 0.9047 +/- 0.003

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
  clouds_x <- clouds_x[cloud_indies, ]
  clouds_y <- clouds_y[cloud_indies]
  non_clouds_x <- non_clouds_x[non_clouds_indies, ]
  non_clouds_y <- non_clouds_y[non_clouds_indies]
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

### NEURAL NETWORK
model <- neuralNetwork(n_hidden = 16,
                       dropout = 0.2)
history <- model %>% fit(
  x_train, y_train, 
  epochs = 25, batch_size = 128,
  validation_data = list(x_test, y_test),
  verbose = 1,
)
scores <- model %>% predict(x_test)
roc_obj <- roc(y_test, c(scores))
auc(roc_obj) # AUC: 0.9693
preds <- scoresToPreds(scores)
confusionMatrix(preds, y_test) # Kappa: 0.7608

### LOGISTIC REGRESSION
model <- logReg(x_test, y_test)
scores <- predict(model, x_test, s = 0.00001)
roc_obj <- roc(y_test, c(scores))
auc(roc_obj) # AUC: 0.9478
preds <- scoresToPreds(scores)
confusionMatrix(preds, y_test) # Kappa: 0.7602

### QDA
model <- QDA(x_test, y_test)
scores <- predict(model, x_test)$posterior[, 2]
roc_obj <- roc(y_test, c(scores))
auc(roc_obj) # AUC: 0.9467
preds <- scoresToPreds(scores) 
confusionMatrix(preds, y_test) # Kappa: 0.7752

### RANDOM FOREST
model <- rf(x_test, as.factor(y_test), ntree = 150)
scores <- predict(model, x_test, type = 'prob')[, 2]
roc_obj <- roc(y_test, c(scores))
auc(roc_obj) # AUC: 1.000
preds <- scoresToPreds(scores)
confusionMatrix(preds, y_test) # Kappa: 0.8347


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
model %>% evaluate(im1x, im1y, verbose = 0) # 82.64%
im1_preds <- model %>% predict_classes(im1x)
im1_scores <- model %>% predict(im1x)

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
model %>% evaluate(im3x, im3y, verbose = 0) # 81.80%
im3_preds <- model %>% predict_classes(im3x)
im3_scores <- model %>% predict(im3x)


