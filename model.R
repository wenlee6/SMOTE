

print("Running model.R")



##becomes of an unbalanced sample will need to do this step in setting up a dataset to run a model


ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "smote")


print("Running smote -- might take some time");
model_rf_smote <- caret::train(y ~ .,
                               data = train_data,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)


##trying the model now on test data:
final_smote <- data.frame(actual = test_data$y,
                          predict(model_rf_smote, newdata = test_data, type = "prob"))





print("Finished smote -- now validating the model")

final_smote$predict <- ifelse(final_smote$Y > 0.5, "Y", "N")
cm_smote <- confusionMatrix(final_smote$predict, test_data$y)

pred<-prediction(final_smote$Y,final_smote$actual)

gain <- performance(pred,"tpr","fpr")
plot(gain, col="orange", lwd=2)



plot(x=c(0, 1), y=c(0, 1), type="l", col="red", lwd=2,
     ylab="True Positive Rate", 
     xlab="Rate of Positive Predictions")
lines(x=c(0, 0.05, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)

gain.x = unlist(slot(gain, 'x.values'))
gain.y = unlist(slot(gain, 'y.values'))

lines(x=gain.x, y=gain.y, col="orange", lwd=2)



##Messing around here to try new parameters and playing around with the datasets - i.e trying random boosting etc

## Here trying out glm to see the + and - of each predictor

## Looking at the variables as a whole to determine the coeffecient

yy<-glm(y ~ .  , family=binomial,data=train_data)
summary(yy)


##Trying gradient boosting here without smote and taking now whole dataset

train_indices <- createDataPartition(y, times =1, p=(1-validation_size), list=TRUE);
X_train <- X[train_indices$Resample1, ];
y_train <- y[train_indices$Resample1];
X_valid <- X[-train_indices$Resample1, ];
y_valid <- y[-train_indices$Resample1];




dtrain_temp <- model.matrix(~., data=X_train)[,-1];
dvalid_temp <- model.matrix(~., data=X_valid  )[,-1];

y_trains<-as.numeric(y_train)-1
y_valids<-as.numeric(y_valid)-1


dtrain <- xgb.DMatrix(data.matrix(model.matrix(~., data=X_train)[,-1]), label=as.integer(y_trains));
dvalid <- xgb.DMatrix(data.matrix(model.matrix(~., data=X_valid)[,-1]), label=as.integer(y_valids));



# Define parameter space, and do a grid search for best parameters in xgboost
xgb_grid <- expand.grid(
  max_depth = c(3, 5, 7),
  eta = c(0.1, 0.01, 0.001),
  subsample = c(0.75, 0.9),
  colsample_bytree = c(0.6, 0.8)
);

rmseErrorsHyperparameters <- apply(xgb_grid, 1, function(parameterList){
  # Extract parameters to use
  max_depth_val = parameterList[["max_depth"]];
  eta_val = parameterList[["eta"]];
  subsample_val = parameterList[["subsample"]];
  colsample_bytree_val =  parameterList[["colsample_bytree"]];
  
  bstcv <- xgb.cv(data = dtrain, nrounds = 500, nfold=10, showsd= TRUE,
                  objective="binary:logistic", 
                  metrics=list("auc", "error"),
                  verbose=1,
                  nthread=4,
                  max_depth = max_depth_val,
                  eta = eta_val,
                  subsample = subsample_val,
                  colsample_bytree = colsample_bytree_val,
                  early.stop.round=20,
                  maximize = FALSE,
                  seed=12345
  );
  
  
  xvalidationscores <- as.data.frame(bstcv$evaluation_log);
  errval <- tail(xvalidationscores$test_error_mean, 1);
  aucval <- tail(xvalidationscores$test_auc_mean, 1);
  best_iter <- which.max(xvalidationscores[, "test_auc_mean"]);
  
  return(c(max_depth_val, eta_val, subsample_val, colsample_bytree_val, best_iter, aucval, errval))
});


write.csv(rmseErrorsHyperparameters, "./output/xgb_hyperparameters_tuning.csv");


# getting best values for model (lowest MSE) 
opt_max_depth_val <- rmseErrorsHyperparameters[1 , which(rmseErrorsHyperparameters[6, ] == max(rmseErrorsHyperparameters[6, ]))];
opt_eta_val <- rmseErrorsHyperparameters[2 , which(rmseErrorsHyperparameters[6, ] == max(rmseErrorsHyperparameters[6, ]))];
opt_subsample_val <- rmseErrorsHyperparameters[3 , which(rmseErrorsHyperparameters[6, ] == max(rmseErrorsHyperparameters[6, ]))];
opt_colsample_val <- rmseErrorsHyperparameters[4 , which(rmseErrorsHyperparameters[6, ] == max(rmseErrorsHyperparameters[6, ]))];
opt_num_rounds <- rmseErrorsHyperparameters[5 , which(rmseErrorsHyperparameters[6, ] == max(rmseErrorsHyperparameters[6, ]))];


####################################2. building model using best parameters ############################################################
# Use the best model output from the hyperpaparmeter tuning using the max AUC and min error.
opt_param <- list(max_depth=opt_max_depth_val, 
                  eta=opt_eta_val, 
                  n_thread=4, silent=1, booster="gbtree",
                  subsample=opt_subsample_val,
                  colsample_bytree=opt_colsample_val
);
num_round <- opt_num_rounds;
opt_model <- xgb.train(opt_param, data = dtrain, nrounds = num_round,
                       objective="binary:logistic", 
                       eval_metric="auc",
                       verbose=1,
                       maximize = FALSE
);

# Dump the model to text file and plot the feature importance and first tree.
model <- xgb.dump(opt_model, with_stats=T);
importance_matrix <- xgb.importance(colnames(dtrain_temp), model = opt_model);

importance_matrix_dtl <- xgb.importance(colnames(dtrain_temp), model = opt_model, data = dtrain_temp, label =y_trains);


xgb.plot.importance(importance_matrix, cex=0.7);
xgb.plot.tree(feature_names=colnames(dtrain_temp), model=opt_model, n_first_tree = 1);


# Based on the variable importance from the model, plot the feature-gain graph to determine ideal number of features.
write.csv(xgb.model.dt.tree(colnames(dtrain_temp), model = opt_model, n_first_tree = 1), "./output/xgbmodel_first_tree.csv");
write.csv(importance_matrix, "./output/xgbmodel_importance_matrix.csv");
write.csv(importance_matrix_dtl, "./output/xgbmodel_importance_matrix_detailed.csv");

print("Ran model.R")
