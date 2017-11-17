# Model selection
source("model.R")

# Goal     : create good folds for CV
# Heuristic: get equal representation of 
#            classes and images in each fold

customCV <- function(n_folds = 5,
                     type = 'neural',
                     n_hidden = 512,
                     n_tree = 100,
                     s = 0.01){
  test_acc <- c()
  
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
    
    cat("Holding out fold", i, "... \n")
    
    if (type == 'neural'){
      model = neuralNetwork(n_hidden = n_hidden)
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
    
    else {
      if (type == 'logistic') {
        model <- logReg(x_train, y_train)
        test_preds <- as.numeric(predict(model, newx = x_test, 
                                          s = s, type = 'class'))
        accuracy <- acc(test_preds, y_test) 
      }
      if (type == 'qda'){
        model <- qda(x_train, y_train)
        test_preds <- predict(model, x_test)$class
        accuracy <- acc(test_preds, y_test)
      }
      if (type == 'rf'){
        model <- rf(x_train, as.factor(y_train), ntree = n_tree)
        test_preds <- predict(model, x_test)
        accuracy <- acc(test_preds, y_test)
      }
      cat(">>> Test accuracy:", accuracy, "\n")
      test_acc <- c(test_acc, accuracy)
    }
  }
  
  cat("CV score:", mean(test_acc), "-- SD:", sd(test_acc), "\n")
  return(mean(test_acc))
}
