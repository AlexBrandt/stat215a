# Alexander Brandt
# STAT 215 Final Project

# Load packages

library(caret)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(knitr)
library(RColorBrewer)
library(superheat)
library(tidyr)
library(reshape2)
library(plot3D)
library(glmnet)
library(outliers)

# Set relevant work directory here
# setwd("~/Dropbox/STAT_215A/stat215a/lab_final/")
# Load all relevant data
load("data/fMRIdata.RData")
fit_stim <- read.csv("data/fit_stim.csv", header=TRUE)
real_wav <- read.csv("data/real_wav.csv", header=TRUE)

# A practice call to make sure we correctly visualize
# the images
# png("../figures/example_image1.png")
# superheat(matrix(rev(unlist(fit_stim[1,])),
#                           nrow = 128, ncol = 128),
#                  heat.pal = c("black", "white"),
#                   legend = FALSE, title = "Image 1", 
#           left.label.size = 1, bottom.label.size = 1,
#           )
# dev.off()

# Set our random seed for reproducibility
set.seed(1)
# Randomize the data before partitioning
r_index <- sample(1:1750)

# Set our training, validation, and testing ratios to 2:1:1
# 1/2 to 1/4 to 1/4

training_i   = r_index[1:875]
validation_i = r_index[876:1313]
testing_i    = r_index[1314:1750]

# Convert the response data for the data frame
resp_dat_df <- as.data.frame(resp_dat)
names(resp_dat_df) <- unlist(lapply(1:20, function(x) paste('V', toString(x), sep ="")))

# Histogram of all voxel response
g = ggplot(melt(resp_dat_df), aes(x=value, color=variable)) +
  geom_density(alpha=0.2, position="identity") + 
  ggtitle("Histogram of All Voxel Responses")
ggsave(filename = "ResponseHistogram.png", plot = g)

# Correlation of the response data so we can understand
# the voxels individually
resp_dat_cor = cor(resp_dat_df)
diag(resp_dat_cor) = rep(NA, 20)

# Generate the heatmap of voxel response correlation
png("../figures/heatmap.png", height = 900, width = 800)
shm = superheat(resp_dat_cor,
          # pretty.order.cols = TRUE,
          n.clusters.rows = 2,
          n.clusters.cols = 2,
          heat.pal = c("red", "orange", "yellow", "green", "blue", "violet"),
          left.label = 'variable',
          bottom.label = 'variable',
          bottom.label.text.angle = 270,
          left.label.text.size = 5,
          bottom.label.text.size = 5,
          title="Voxel Response Correlations",
          title.size = 10
          )
dev.off()

# The location data for the voxel regions
loc_df <- as.data.frame(loc_dat)
names(loc_df) <- c("x", "y", "z")
voxel_names_ordered = unlist(lapply(1:20, function(i) paste("V", i, sep="")))
loc_df$cluster = shm$membership.cols[voxel_names_ordered]

# Make a plot based on the voxel location with clustering
# based on the heatmap
p <- plot_ly(loc_df, x = ~x, 
             y = ~y, 
             z = ~z, color = ~cluster, colors = c('blue', 'red')) %>%
  add_markers(alpha=.8) %>%
  layout(scene = list(xaxis = list(title = 'x'),
                      yaxis = list(title = 'y'),
                      zaxis = list(title = 'z')))

# Partition the response data in the same way as the feature data
train_resp <- resp_dat_df[training_i,]
valid_resp <- resp_dat_df[validation_i,]
test_resp <- resp_dat_df[testing_i,]

train_feat <- fit_feat_df[training_i,]
valid_feat <- fit_feat_df[validation_i,]
test_feat <- fit_feat_df[testing_i,]

fit_feat_df <- as.data.frame(fit_feat)

# Ensure all Gabor features are labeled correctly
names(fit_feat_df) <- unlist(lapply(1:10921, function(x) paste('G', toString(x), sep ="")))

# Set up the correlation between Gabor features
all_correlations <- 
  lapply(train_resp,
         FUN = function(resp_row) { 
           apply(train_feat, MARGIN = 2, 
                 FUN = function(feat_row) {
                   cor(resp_row, feat_row)
                   }) 
           })

all_correlations <- as.data.frame(all_correlations)

top_features <- t(apply(all_correlations, 2, function(cor) {
  order(cor, decreasing = TRUE)[1:10921]
}))

mini_all_correlations = all_correlations
all_correlations_long = melt(all_correlations)
all_correlations_long$index = row.names(all_correlations_long)
nrow(all_correlations_long)

ordered_v1 = as.data.frame(cbind(1:length(sort(all_correlations$V1)),
                   sort(all_correlations$V1, decreasing=TRUE)))

# Figure 1
g = ggplot(ordered_v1, aes(x=V1, y=V2)) +
  geom_point() + xlab("Ordered Index") +
  ylab(expression(rho)) + 
  ggtitle("Sorted Value of Correlation Coefficient for\nGabor Features")

ggsave(filename = "GaborFeatureOrdered.png", plot = g)

# Figure 2
g = ggplot(ordered_v1, aes(V2)) +
  geom_histogram(binwidth=.01) + xlab(expression(rho)) + 
  ggtitle("Histogram of Correlation Coefficient for V1") +
  geom_vline(xintercept=all_correlations$V1[1000], col='red')

ggsave(filename = "ResponseHistogram.png", plot = g)

# Clear all the data frames that will be used as results
nfolds = 10
model_data_frame = c()
voxel_correlations = c()
lambda_values = c()
best_model = c()
best_model_correlation = c()
set.seed(1)

# Loop through all voxels
for (voxel_number in 1:20) {
  
  # Select the training, validation, and testing matrices
  # for the voxels
  training_matrix =
    train_feat
  training_response = 
    train_resp[,voxel_number]
  
  validation_matrix = 
    valid_feat
  validation_response = 
    valid_resp[,voxel_number]
  
  testing_matrix=
    test_feat
  testing_response = 
    test_resp[,voxel_number]
  
  # Generate a consistent set of lambda values
  my_lambda_seq = 10^seq(0, -2, length = 100)
  
  # This fit will be used lated for AIC, AICc, BIC
  initial_glm_model = glmnet(x=as.matrix(training_matrix),
                             y=as.vector(training_response),
                             alpha=1,
                             lambda=my_lambda_seq)
  # Generate predictions from previous models
  prediction_initial_glm_model = predict.glmnet(initial_glm_model, 
                                 newx=as.matrix(training_matrix),
                                 s=my_lambda_seq)
  # This was the old code, in an attempt to fit with Bin's
  # slides.  The deviance code worked better, but it felt
  # relevant to include
  #
  # RSS = apply(prediction_initial_glm_model,
  #             2,
  #             function(x) return(sum((training_response - x)^2)))
  
  # Set n and k
  n = initial_glm_model$nobs
  k = initial_glm_model$df

  # Generate AIC, AICc, and BIC values for the lambda range
  AIC  = deviance(initial_glm_model) + 2 * k
  AICc = AIC + ((2 * k * (k + 1)) / (n - k - 1))
  BIC  = deviance(initial_glm_model) + k * log(n)
  
  # Minimize to get the lambda chosen by each criterea.
  AIC_lambda  = my_lambda_seq[which.min(AIC)]
  AICc_lambda = my_lambda_seq[which.min(AICc)]
  BIC_lambda  = my_lambda_seq[which.min(BIC)]
  
  ### CUSTOM CV STARTS HERE ###
  
  # Now we build our own version of CV conditioning on 
  # correlation between responses and predictions
  
  # Shuffle the training set, cut it into n-folds, and
  # build an empty data frame
  shuffled = training_matrix[sample(1:nrow(training_matrix)),]
  fold_ids <- cut(1:nrow(shuffled), breaks=nfolds, labels=FALSE)
  my_lambda_df <- data.frame("correlations"=double(),
                             "lambda"=double())

  # Run the CV code for each fold
  for (i in 1:nfolds) {
    print(i)
    # Partition our test data into sub-test and sub-training data
    cv_testing_data  = shuffled[i==fold_ids,]
    cv_training_data = shuffled[i!=fold_ids,]
    cv_testing_response = train_resp[row.names(cv_testing_data),voxel_number]
    cv_training_response = train_resp[row.names(cv_training_data),voxel_number]
    
    # Train the model
    cv_lassos = glmnet(x=as.matrix(cv_training_data),
                          y=as.vector(cv_training_response),
                          alpha=1,
                          lambda=my_lambda_seq)
    # Predict the response from the previous model
    cv_prediction = predict.glmnet(cv_lassos, 
                                    newx=as.matrix(cv_testing_data),
                                    s=my_lambda_seq)
    # Maximize correlation w.r.t lambda
    biggest_cv_lambda_index = which.max(apply(cv_prediction, 
                    2,
                    function(c) cor(c, cv_testing_response)))
    # Add to data frame for later
    to_add = c(my_lambda_seq[biggest_cv_lambda_index],
               max(apply(cv_prediction, 
                         2,
                         function(c) cor(c, cv_testing_response)),
               na.rm=TRUE))
    # Bookkeeping
    to_add = as.data.frame(t(to_add))
    names(to_add) = c("lambda", "correlations")
    my_lambda_df = rbind(my_lambda_df, to_add)
  }
  # Choose the best lambda from the n-folds
  best_lambda = my_lambda_df$lambda[which.max(my_lambda_df$correlations)]
  # Train our ESCV model
  escv_model <- escv.glmnet(x=as.matrix(training_matrix),
                           y=as.vector(training_response),
                           alpha=1,
                           lambda=my_lambda_seq)
  # Use our lambda from the n-folds code to predict
  final_cv_model <- glmnet(x=as.matrix(training_matrix),
                        y=as.vector(training_response),
                        alpha=1,
                        lambda=best_lambda)
  # Use the standard MSE CV model for good measure
  final_package_cv_model <- cv.glmnet(x=as.matrix(training_matrix),
                           y=as.vector(training_response),
                           alpha=1,
                           lambda=my_lambda_seq)
  
  final_escv_model <- glmnet(x=as.matrix(training_matrix),
                           y=as.vector(training_response),
                           alpha=1,
                           lambda=escv_model$lambda.escv)
  # Use the AIC lambda with lasso
  final_aic_model <- glmnet(x=as.matrix(training_matrix),
                           y=as.vector(training_response),
                           alpha=1,
                           lambda=AIC_lambda)
  # Use the AICc lambda with lasso
  final_aicc_model <- glmnet(x=as.matrix(training_matrix),
                           y=as.vector(training_response),
                           alpha=1,
                           lambda=AICc_lambda)
  # Use the BIC lambda with lasso
  final_bic_model <- glmnet(x=as.matrix(training_matrix),
                           y=as.vector(training_response),
                           alpha=1,
                           lambda=BIC_lambda)
  # Create a CV-based ridge model (alpha = 0)
  final_ridge_model <- cv.glmnet(x=as.matrix(training_matrix),
                              y=as.vector(training_response),
                              alpha=0)
  # Create a linear fit for good measure
  lm_fit <- lm(training_response ~ ., data=training_matrix)
  # If it's the first voxel, generate our diagnostic plots
  # for the final writeup
  if (voxel_number == 1) {
    ab_df = as.data.frame(cbind(my_lambda_seq, AIC, AICc, BIC,
      apply(cv_prediction, 2, function(c) cor(c, cv_testing_response)),
      final_package_cv_model$cvm,
      escv_model$es
    ))
    names(ab_df) = c("lambda", "AIC", "AICc", "BIC", "Cor",
                     "CVMSE", "ESCVMSE")
    a = ggplot(ab_df, aes(x=lambda, y=AIC), log="x") +
      geom_line() + scale_x_continuous(trans="log") +
      xlab(expression(lambda)) + ggtitle("AIC Plot")
    b = ggplot(ab_df, aes(x=lambda, y=AICc), log="x") +
      geom_line() + scale_x_continuous(trans="log") +
      xlab(expression(lambda)) + ggtitle("AICc Plot")
    c = ggplot(ab_df, aes(x=lambda, y=BIC), log="x") +
      geom_line() + scale_x_continuous(trans="log") +
      xlab(expression(lambda)) + ggtitle("BIC Plot")
    d = ggplot(ab_df, aes(x=lambda, y=Cor, log="x")) +
      geom_line() + scale_x_continuous(trans="log") +
      xlab(expression(lambda)) +
      ylab(expression(rho)) + ggtitle("CV Plot")
    e = ggplot(ab_df, aes(x=lambda, y=CVMSE, log="x")) +
      geom_line() + scale_x_continuous(trans="log") +
      xlab(expression(lambda)) +
      ylab("MSE") + ggtitle("CV Plot (MSE)")
    f = ggplot(ab_df, aes(x=lambda, y=ESCVMSE, log="x")) +
      geom_line() + scale_x_continuous(trans="log") +
      xlab(expression(lambda)) +
      ylab("MSE") + ggtitle("ES-CV Plot (MSE)")
    png("../figures/AIC.png", width = 8, height = 8, units = 'in', res = 600)
    grid.arrange(a, b, c, d, e, f, ncol = 2)
    dev.off()
  }  
  # Get our predictions from our OLS model
  prediction_lm = predict.lm(lm_fit, 
             newdata=validation_matrix,
             type = "response")
  # Generate our relevant predictions
  all_prediction = predict.glmnet(final_cv_model, 
                                 newx=as.matrix(validation_matrix))[,1]
  
  prediction_cv = predict.glmnet(final_cv_model, 
                                 newx=as.matrix(validation_matrix))[,1]
  
  prediction_package_cv = predict.cv.glmnet(final_package_cv_model, 
                                 newx=as.matrix(validation_matrix))[,1]
  
  prediction_escv = predict.glmnet(final_escv_model,
                                 newx=as.matrix(validation_matrix))[,1]
  
  prediction_aic = predict.glmnet(final_aic_model, 
                                   newx=as.matrix(validation_matrix))[,1]
  
  prediction_aicc = predict.glmnet(final_aicc_model, 
                                 newx=as.matrix(validation_matrix))[,1]
  
  prediction_bic = predict.glmnet(final_bic_model, 
                                   newx=as.matrix(validation_matrix))[,1]
  
  prediction_ridge = predict.cv.glmnet(final_ridge_model,
                                       newx=as.matrix(validation_matrix))[,1]
  # Generate correlations so we can choose the best model
  lm_correlation         = cor(prediction_lm,         validation_response)
  cv_correlation         = cor(prediction_cv,         validation_response)
  package_cv_correlation = cor(prediction_package_cv, validation_response)
  escv_correlation       = cor(prediction_escv,       validation_response)
  aic_correlation        = cor(prediction_aic,        validation_response)
  aicc_correlation       = cor(prediction_aicc,       validation_response)
  bic_correlation        = cor(prediction_bic,        validation_response)
  ridge_correlation      = cor(prediction_ridge,      validation_response)
  # rbind the voxel correlations for later plotting
  voxel_correlations = rbind(voxel_correlations,
        c(lm_correlation, cv_correlation, package_cv_correlation,
    escv_correlation, aic_correlation, aicc_correlation, bic_correlation,
    ridge_correlation))
  # Determine which model was the best based on validation data
  best_model_index = which.max(c(lm_correlation, cv_correlation, package_cv_correlation,
              escv_correlation, aic_correlation, aicc_correlation, bic_correlation,
              ridge_correlation))
  # Append the best model
  best_model = c(best_model_index, best_model)
  # Save all models for later investigation in a list
  all_models = list(lm_fit, final_cv_model, final_package_cv_model,
                 final_escv_model, final_aic_model, final_aicc_model,
                 final_bic_model, final_ridge_model)
  # Append the aforementioned list
  model_data_frame = rbind(model_data_frame, all_models)
  # Get the testing correlation from the whole data set
  testing_cor = cor(testing_response, 
                    as.data.frame(predict(all_models[[best_model_index]],
          newx=as.matrix(testing_matrix)))[,1])
  # Append the testing correlation
  best_model_correlation = c(best_model_correlation, testing_cor)
  # Save the lambda values from each model selection type
  lambda_values = rbind(lambda_values, c(best_lambda, 
                        final_package_cv_model$lambda.min,
                        escv_model$lambda.escv,
                        AIC_lambda, AICc_lambda, BIC_lambda, 
                        final_ridge_model$lambda.min))
  # Testing print code
  #print("VOXEL:")
  # print(voxel_number)
}
# Reframe the object as a data frame, label the columns
voxel_correlations <- as.data.frame(voxel_correlations)
names(voxel_correlations) <- c("LM", "CV", "Package CV", "ES-CV",
                               "AIC", "AICc", "BIC", "Ridge")
# Create a ggplot
to_box = melt(t(voxel_correlations))
colMeans(voxel_correlations, na.rm = TRUE)
# Model Selection Figure
g = ggplot(to_box, aes(Var1, value)) +
  geom_boxplot() + xlab("Method") +
  ylab("Validation Set Correlation") +
  ggtitle("Comparing Model Selection Methods\n") + geom_jitter(width=.01)

ggsave("../figures/ModelSelection.png", g)

# Create final correlations from the best model and test set
final_testset_correlations = rep(NA, 20)
for (i in 1:20) {
  final_testset_correlations[i] = cor(test_resp[,i], 
    as.data.frame(predict(model_data_frame[i,best_model[i]],
    newx=as.matrix(test_feat)))[,1])
}
final_testset_correlations = as.data.frame(final_testset_correlations)
final_testset_correlations$voxel_name = factor(voxel_names_ordered, 
                                               levels=voxel_names_ordered)
final_testset_correlations$best_method =
  unlist(lapply(best_model, function(i) names(voxel_correlations)[i]))

# Get the best test set correatlion by voxel
g = ggplot(final_testset_correlations,
           aes(voxel_name, y=final_testset_correlations)) +
  geom_boxplot() + 
  ggtitle("Test Set Correlation by Voxel") +
  xlab("Voxel Name") +
  ylab("Final Test Set Correlations")
ggsave(filename = "TestSetCorrelationByVoxel.png", plot = g)
  
ggsave("../figures/AllTrainingCorrelations.png")

# Bootstrap the test set, get correatlion coefficients
# and histogram them from the bootstrap
bootstraps_correlations = rep(NA, 1000)
for (i in 1:1000) {
  bootstrap_indices = sample(1:437, replace = TRUE)
  bootstrap_matrix   = test_feat[bootstrap_indices,]
  bootstrap_response = test_resp[bootstrap_indices,1]
  bootstraps_correlations[i] = cor(bootstrap_response, 
                             as.data.frame(predict(model_data_frame[1,best_model[1]],
                             newx=as.matrix(bootstrap_matrix)))[,1])
}

# Graph the bootstrap histogram
g = ggplot(as.data.frame(bootstraps_correlations),
           aes(x=bootstraps_correlations)) +
  geom_histogram() + 
  ggtitle("Bootstrapped Test Set Correlations") +
  xlab(expression(rho))
ggsave(filename = "../figures/BootstrapHistogram.png", plot = g)

# Find out what features are in common
all_G_features = c()

for (i in 1:20) {
  my_betas = model_data_frame[i,best_model[i]]$all_models$beta
  G_i = names(my_betas[,1])[my_betas[,1] != 0]
  all_G_features = c(all_G_features, G_i)
}

# Histogram the best features that have over 20% occurance
top_Gs = names(sort(table(all_G_features), decreasing=TRUE))[1:13]
top_Gs_hist = all_G_features[all_G_features %in% top_Gs]
table(top_Gs_hist)

g = ggplot(as.data.frame(top_Gs_hist), aes(top_Gs_hist)) +
  geom_bar() +
  ggtitle("Features Found in More Than 4 Voxels") +
  xlab("Feature") + ylab("Frequency")
ggsave("../figures/barplot.png", g)

# Outlier analysis

aq = aq.plot(resp_dat)
out_images = which(aq$outliers)

# We select V2 and look at what images our model is most responsive to

i = 2

model_response_predictions = as.data.frame(predict(model_data_frame[i,best_model[i]],
                                                   newx=as.matrix(test_feat)))
image_response_ordering = rev(order(model_response_predictions$s0))

a = superheat(matrix(rev(unlist(fit_stim[image_response_ordering[1],])),
                 nrow = 128, ncol = 128),
          heat.pal = c("black", "white"),
          legend = FALSE, title = "Image 1", 
          left.label.size = 1, bottom.label.size = 1)
b = superheat(matrix(rev(unlist(fit_stim[image_response_ordering[2],])),
                     nrow = 128, ncol = 128),
              heat.pal = c("black", "white"),
              legend = FALSE, title = "Image 2", 
              left.label.size = 1, bottom.label.size = 1)
c = superheat(matrix(rev(unlist(fit_stim[image_response_ordering[3],])),
                     nrow = 128, ncol = 128),
              heat.pal = c("black", "white"),
              legend = FALSE, title = "Image 3", 
              left.label.size = 1, bottom.label.size = 1)
d = superheat(matrix(rev(unlist(fit_stim[image_response_ordering[4],])),
                     nrow = 128, ncol = 128),
              heat.pal = c("black", "white"),
              legend = FALSE, title = "Image 4", 
              left.label.size = 1, bottom.label.size = 1)

# Save our figure

png("../figures/Voxel2Figures.png", width = 8, height = 8, units = 'in', res = 600)
grid.arrange(a$plot, b$plot, c$plot, d$plot, ncol = 2)
dev.off()

# Predict the prompt test set response from question 6

prediction = predict(model_data_frame[1,best_model[1]],
                     newx=val_feat)$all_models

write.table(x = prediction[,1], file = "output/predv1_AlexanderBrandt.txt",
            quote=FALSE, row.names = FALSE, col.names = FALSE)