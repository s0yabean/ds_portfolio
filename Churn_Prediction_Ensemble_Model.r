################################################################################################
# Churn-Prediction Machine Learning Project 
# Description: A paid project for a venture-backed startup, helping to predict the likeliest customers 
# about to churn for a B2C subscription-based app. Here, I am going through a couple of popular 
# ML algorithms for this binary classification problem to find baseline performance, and also 
# trying out an ensemble method to see if performance improves.
# Date: 2018 Spring
# Author: Tony Tong
################################################################################################

library(caret)
library(dplyr)
library(pROC)
library(vtreat)
library(corrplot)

################################################################################################
# Parallel Processing (To speed up machine learning speeds)
################################################################################################

# Parallel Processing (Added by Tony. My Mac has an i5 processor, which has 4 cores)
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# run when rounding up machine learning project to stop the parallel processing
# stopCluster(cl)

################################################################################################
# Obtaining and Pre-processing Data

# Essentially here, I am running the data through a dummy variable conversion (converting text to numerics/ factors)
# and vtreat, which is to standardise, normalise and deal with discrepancies like missing values in the data.

# Workflow: Data -> dummyVar() -> vtreat() -> scale() for both TRAINING and TEST data.
################################################################################################

# Connect to db
connection <- XXXXXXX::pgsql_connect("XXXXXXXXXXXXXXXXXXXXXXXXXXXX") 

# Training data. Positive class is set as default to be 0, when there is no renewal.
train_data <- dbGetQuery(connection, "XXXXXXXXXXXXXXXXXXXXXXXXXXXX")

# Test data. Key difference is that there is an extra date filter, to those before 21st Feb as there was an engineering change before.
test_data <- dbGetQuery(connection,"XXXXXXXXXXXXXXXXXXXXXXXXXXXX")

# Dummy variable transformation
transform = dummyVars(" ~ .", data = train_data)
train_data_dummy = data.frame(predict(transform, newdata = train_data))
str(train_data_dummy)

# create a cross frame experiment (TRAINING data)
cross_frame_experiment <- mkCrossFrameCExperiment(train_data_dummy, colnames(train_data_dummy), 'renewed', 1)
plan <- cross_frame_experiment$treatments
performance_stats <- plan$scoreFrame
treated_train <- cross_frame_experiment$crossFrame

# treat TEST data based on the same treatment plan on training data 
test_data_dummy = data.frame(predict(transform, newdata = test_data))
treated_test <- vtreat::prepare(plan, test_data_dummy, pruneSig = NULL)

# Scaling
treated_train[, 1:24] = scale(treated_train[, 1:24])
treated_test[, 1:24] = scale(treated_test[, 1:24])

# Converting to character as a downstream function does not work with numerics/ factors.
treated_train$renewed_char <- ifelse(treated_train$renewed == 1, "yes", "no")
treated_test$renewed_char <- ifelse(treated_test$renewed == 1, "yes", "no")

################################################################################################
# Measuring Correlation and picking top 20 variables
################################################################################################

correlation <- cor(treated_train) 
corrplot(correlation, method = "circle", tl.cex = .5, tl.col = 'black')
vars = as.data.frame(correlation) %>% mutate(variable = colnames(correlation)) %>% 
  select(variable, renewed) %>% arrange(desc(abs(renewed))) %>% dplyr::slice(2:21) 

x_vars <- paste(unlist(vars[1:20,1]), collapse=' + ')
equation <- as.formula(paste("renewed_char", x_vars, sep = " ~ " ))

# str(treated_train)
# str(treated_test)

################################################################################################
# Creating a standard set of control parameters in train function

# Number and repeats are changable parameters
################################################################################################

myControl = trainControl(method = "cv", number = 3, repeats = 2, search = 'grid', 
                         classProbs = TRUE, summaryFunction=twoClassSummary, verboseIter = TRUE)

################################################################################################
# Machine Learning

# Methods tried:
# Random Forest (rf)
# Logistic Regression (glm)
# Xgboost (xgb)
# Ada (Boosted Classification Tree)
# Naive Bayes (nb)
# Support Vector Machine (svm)

################################################################################################

#################################
# Random Forest
#################################

rf_fit = train(equation, data = treated_train, method="rf", metric = 'ROC', trControl = myControl)
rf_fit

# testing data
rf_fit_prob <- predict(rf_fit, newdata = treated_test, type='prob')[,2]

roc(treated_test$renewed, rf_fit_prob) %>% auc 

confusionMatrix(ifelse(rf_fit_prob>= 0.50, 1, 0), treated_test$renewed)

# Training Data
# ROC        Sens       Spec     
# 0.8192454  0.8405315  0.6713205

# Testing Data
# ROC        Sens       Spec     
# 0.72       0.6272     0.6926

#################################
# GLMNET (Logistic Regression)
#################################

glmnet_grid <- expand.grid(.alpha = c(0, 0.5, 1), .lambda=seq(0, 1, by=.01))
glmnet_fit <- train(equation, data = treated_train, 
                    method='glmnet', tuneGrid=glmnet_grid, metric='ROC',
                    trControl= myControl)
glmnet_fit$results

# testing data

glmnet_fit_prob <- predict(glmnet_fit, newdata = treated_test, type='prob')[,2]

roc(treated_test$renewed, glmnet_fit_prob) %>% auc 

confusionMatrix(ifelse(glmnet_fit_prob >= 0.50, 1, 0), treated_test$renewed)

# Training Data
# ROC        Sens       Spec     
# 0.8186528  0.8968968  0.6185666

# Testing Data
# ROC        Sens       Spec     
# 0.7778     0.7629     0.6667

####################################################
# XGBOOST - extreme gradient boosting (tree method)
####################################################

# xgboost tree (0.9605899  0.8795028  0.9524339)
xgb_grid <- expand.grid(nrounds = 50, #the maximum number of iterations
                        eta = c(0.01,0.1), # shrinkage
                        max_depth = c(2,6,10),
                        gamma = 0,               #default=0
                        colsample_bytree = 1,    #default=1
                        min_child_weight = 1,   #default=1
                        subsample = c(.05, 1))

xgb_fit <- train(equation, data = treated_train, method='xgbTree', tuneGrid=xgb_grid, metric='ROC',
                 trControl=myControl)

xgb_fit

xgb_fit_prob <- predict(xgb_fit, newdata = treated_test, type='prob')[,2]

roc(treated_test$renewed, xgb_fit_prob ) %>% auc 

confusionMatrix(ifelse(xgb_fit_prob  >= 0.50, 1, 0), treated_test$renewed)

# Training Data
# ROC        Sens       Spec 
# 0.8192677  0.8593222  0.6636708

# Testing Data
# ROC        Sens       Spec 
# 0.7864     0.6681     0.7305

################################################################################################
# SVM Modelling
################################################################################################

svm_fit <- train(equation, data = treated_train, 
                    method='svmLinear2', metric='ROC',
                    trControl=myControl)
svm_fit

svm_fit_prob <- predict(svm_fit, newdata = treated_test, type='prob')[,2]

roc(treated_test$renewed, svm_fit_prob ) %>% auc 

confusionMatrix(ifelse(svm_fit_prob >= 0.50, 1, 0), treated_test$renewed) 

# Training Data
# ROC        Sens       Spec 
# 0.8079858  0.9309330  0.5743828

# Testing Data
# ROC        Sens       Spec 
# 0.7191     0.7694     0.6527

################################################################################################
# NB Modelling
################################################################################################

nb_fit <- train(equation, data = treated_train, 
                 method='nb', metric='ROC',
                 trControl=myControl)
nb_fit

nb_fit_prob <- predict(nb_fit, newdata = treated_test, type='prob')[,2]

roc(treated_test$renewed, nb_fit_prob ) %>% auc 

confusionMatrix(ifelse(nb_fit_prob >= 0.50, 1, 0), treated_test$renewed) 

# Training Data
# ROC        Sens       Spec 
# 0.7977393  0.9090973  0.5955845

# Testing Data
# ROC        Sens       Spec 
# 0.755      0.7543     0.6527

################################################################################################
# Boosted Classification Trees
################################################################################################

ada_fit <- train(equation, data = treated_train, 
                method='ada', metric='ROC', 
                trControl=myControl)
ada_fit

ada_fit_prob <- predict(ada_fit, newdata = treated_test, type='prob')[,2]

roc(treated_test$renewed, ada_fit_prob) %>% auc 

confusionMatrix(ifelse(ada_fit_prob >= 0.50, 1, 0), treated_test$renewed) 

# Training Data
# ROC        Sens       Spec 
# 0.8219956  0.8313704  0.6812450

# Testing Data
# ROC        Sens       Spec 
# 0.7841     0.7651     0.6567

################################################################################################
# Seeing results on training data (during resampling) to approximate performace of alogrithms
################################################################################################

# Based on a subset training data (sample bags)
results = resamples(list(ada = ada_fit, nb = nb_fit, svm = svm_fit,
                         xgb = xgb_fit, rf = rf_fit, glmnet = glmnet_fit))

summary(results)
bwplot(results)
dotplot(results)
# dev.off() #if plot does not show

################################################################################################
# Ensemble Modelling
################################################################################################

# Adding all 6 to build an ensemble model

treated_train$ada_fit_prob = predict(ada_fit, newdata = treated_train, type='prob')[2] %>% unlist()
treated_train$nb_fit_prob = predict(nb_fit, newdata = treated_train, type='prob')[2] %>% unlist()
treated_train$svm_fit_prob = predict(svm_fit, newdata = treated_train, type='prob')[2] %>% unlist()
treated_train$rf_fit_prob = predict(rf_fit, newdata = treated_train, type='prob')[2] %>% unlist()
treated_train$glmnet_fit_prob = predict(glmnet_fit, newdata = treated_train, type='prob')[2] %>% unlist()
treated_train$xgb_fit_prob = predict(xgb_fit, newdata = treated_train, type='prob')[2] %>% unlist()

treated_test$ada_fit_prob = predict(ada_fit, newdata = treated_test, type='prob')[2] %>% unlist()
treated_test$nb_fit_prob = predict(nb_fit, newdata = treated_test, type='prob')[2] %>% unlist()
treated_test$svm_fit_prob = predict(svm_fit, newdata = treated_test, type='prob')[2] %>% unlist()
treated_test$rf_fit_prob = predict(rf_fit, newdata = treated_test, type='prob')[2] %>% unlist()
treated_test$glmnet_fit_prob = predict(glmnet_fit, newdata = treated_test, type='prob')[2] %>% unlist()
treated_test$xgb_fit_prob = predict(xgb_fit, newdata = treated_test, type='prob')[2] %>% unlist()

# Scaling the probabilities just like all the other data
treated_train[, 27:32] = scale(treated_train[, 27:32])
treated_test[, 27:32] = scale(treated_test[, 27:32])

equation_ensemble <- as.formula("XXXXXXXXXXXXXXXXXXXXXXXXXXXX")

#################################
# GLMNET (Logistic Regression)

# Reruning the machine learning
# using glmnet (arbitrary)
#################################

glmnet_fit_ensemble <- train(equation_ensemble, data = treated_train, method='glmnet', 
                             tuneGrid=glmnet_grid, metric = 'ROC', trControl = myControl)

glmnet_fit_ensemble_prob <- predict(glmnet_fit_ensemble, newdata = treated_test , type='prob')[,2]

# KEEP POSITIVE CLASS AT 0! (Did not renew, which is the default in the confusion matrix function)
roc(treated_test$renewed, glmnet_fit_ensemble_prob) %>% auc 
confusionMatrix(ifelse(glmnet_fit_ensemble_prob >= 0.50, 1, 0), treated_test$renewed) 

####################################################
# XGBOOST - extreme gradient boosting (tree method)
####################################################

xgb_grid <- expand.grid(nrounds = 50, #the maximum number of iterations
                        eta = c(0.01,0.1), # shrinkage
                        max_depth = c(2,6,10),
                        gamma = 0,               #default=0
                        colsample_bytree = 1,    #default=1
                        min_child_weight = 1,   #default=1
                        subsample = c(.05, 1))

xgb_fit_ensemble <- train(equation_ensemble, data = treated_train, method='xgbTree', 
                             tuneGrid=xgb_grid, metric = 'ROC', trControl = myControl)

xgb_fit_ensemble_prob <- predict(xgb_fit_ensemble, newdata = treated_test , type='prob')[,2]

# KEEP POSITIVE CLASS AT 0! (Did not renew, which is the default in the confusion matrix function)
roc(treated_test$renewed, xgb_fit_ensemble_prob) %>% auc 

confusionMatrix(ifelse(xgb_fit_ensemble_prob >= 0.50, 1, 0), treated_test$renewed) 

####################################################
# Conclusion
####################################################
# For single algorithm, having boosted classification trees gave the best performane ROC wise, so we can tell that tree-based
# algorithms seem to work well for this problem type.

# During the ensembling, there was severe overfitting on the training data, as the training accuracy went up to 99% but testing accuracy remained similar to baseline. 

# This suggests that the main bottleneck is not from algorithms, perhaps we need more/ better data or fine-tuning hyperparameters.
