devtools::install_github("rstudio/keras")
library(keras)
install_keras()
library(glmnet)
library(MASS)
library(randomForest)

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


logReg <- function(x, y){
  fit <- glmnet(x, y, family = 'binomial')
  return(fit)
}

QDA <- function(x, y){
  return(qda(x, y))
}

rf <- function(x, y, ntree = 100){
  return(randomForest(x, y, ntree = ntree))
}
