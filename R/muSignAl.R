options(warn=-1)

library(glmnet)
library(pROC)
getFea <- function(data, feature, outcome, n_run=100, thres=0.9) {
  df <- data[c(feature, outcome)]
  run <- list()
  for(i in 1:n_run) {
    trainIndex = createDataPartition(df[[outcome]], p = .8, list = FALSE)
    Train <- df[trainIndex,]
    Test  <- df[-trainIndex,]
    x_train <- data.matrix(Train[,!names(Train) %in% outcome])
    y_train <- as.matrix(Train[,outcome])
    x_test <- data.matrix(Test[,!names(Train) %in% outcome])
    y_test <- as.matrix(Test[,outcome])
    if(nlevels(as.factor(y_test))==1) next
    if(nrow(data)-sum(y_train) < 8) next
    if(length(y_test)-sum(y_test) == 0) next
    cvfit <- cv.glmnet(x_train, y_train, type.measure = "class", alpha=0.9, nfolds = 10, family="binomial")
    y_test_pred <- predict(cvfit, newx = x_test, s = "lambda.min")
    roc_test <- suppressMessages(roc(y_test, y_test_pred))
    if(pROC::auc(roc_test)>0.5){
      beta <- as.matrix(coef(cvfit, s = "lambda.min"))
      prot <- rownames(beta)[beta[,1]!=0]
      run <- append(run, prot)
    }
  }
  freq <- as.list(table(as.factor(apply(cbind(run), 2, unlist))))
  freq <- freq[order(unlist(freq), decreasing = T)]
  imp <- freq[freq>=thres*n_run]
  core <- names(imp)[-1]
  
  return(core)
}

acc <- function (conf_matrix) {
  TP <- conf_matrix$table[1,1]
  TN <- conf_matrix$table[2,2]
  FP <- conf_matrix$table[1,2]
  FN <- conf_matrix$table[2,1]

  acc_final <- (TP+TN)/(TN+TP+FN+FP)
  return(acc_final)
}

mcc <- function (conf_matrix) {
  TP <- conf_matrix$table[1,1]
  TN <- conf_matrix$table[2,2]
  FP <- conf_matrix$table[1,2]
  FN <- conf_matrix$table[2,1]

  mcc_num <- (TP*TN - FP*FN)
  mcc_den <- as.double((TP+FP))*as.double((TP+FN))*as.double((TN+FP))*as.double((TN+FN))

  mcc_final <- mcc_num/sqrt(mcc_den)
  return(mcc_final)
}

library(stringr)
getPerf <- function(data, feature, outcome, core) {
  c <- str_c(core, collapse = ",")
  set.seed(200)
  flds <- createFolds(data[[outcome]], k = 5, list = T, returnTrain = F)
  new <- data[c(core, outcome)]
  
  #Fold1
  Train <- new[-flds$Fold1,]
  Test  <- new[flds$Fold1,]
  x_train <- data.matrix(Train[,!names(Train) %in% outcome])
  y_train <- as.matrix(Train[,outcome])
  x_test <- data.matrix(Test[,!names(Train) %in% outcome])
  y_test <- as.matrix(Test[,outcome])
  if(nlevels(as.factor(y_test))==1) return(data.frame())
  cvfit <- cv.glmnet(x_train, y_train, type.measure = "class", alpha=0.9, nfolds = 10, family="binomial")
  y_train_pred <- predict(cvfit, newx = x_train, s = "lambda.min")
  y_test_pred <- predict(cvfit, newx = x_test, s = "lambda.min")
  roc_train <- suppressMessages(roc(as.numeric(y_train),as.numeric(y_train_pred)))
  roc_test <- suppressMessages(roc(as.numeric(y_test),as.numeric(y_test_pred)))
  auc_train1 <- pROC::auc(roc_train)
  auc_test1 <- pROC::auc(roc_test)
  th_train <- coords(roc_train, "b", ret = "t", best.method = "youden", transpose = T)
  y_train_pred_b <- as.factor(ifelse(y_train_pred > th_train,1,0))
  y_test_pred_b <- as.factor(ifelse(y_test_pred > th_train,1,0))
  cm_train <- caret::confusionMatrix(data = y_train_pred_b, reference = as.factor(y_train))
  cm_test <- caret::confusionMatrix(data = y_test_pred_b, reference = as.factor(y_test))
  acc_train1 <- acc(cm_train)
  acc_test1 <- acc(cm_test)
  se_train1 <- cm_train$byClass['Sensitivity']
  sp_train1 <- cm_train$byClass['Specificity']
  se_test1 <- cm_test$byClass['Sensitivity']
  sp_test1 <- cm_test$byClass['Specificity']
  mcc_train1 <- mcc(cm_train)
  mcc_test1 <- mcc(cm_test)
  
  #Fold2
  Train <- new[-flds$Fold2,]
  Test  <- new[flds$Fold2,]
  x_train <- data.matrix(Train[,!names(Train) %in% outcome])
  y_train <- as.matrix(Train[,outcome])
  x_test <- data.matrix(Test[,!names(Train) %in% outcome])
  y_test <- as.matrix(Test[,outcome])
  if(nlevels(as.factor(y_test))==1) return(data.frame())
  cvfit <- cv.glmnet(x_train, y_train, type.measure = "class", alpha=0.9, nfolds = 10, family="binomial")
  y_train_pred <- predict(cvfit, newx = x_train, s = "lambda.min")
  y_test_pred <- predict(cvfit, newx = x_test, s = "lambda.min")
  roc_train <- suppressMessages(roc(as.numeric(y_train),as.numeric(y_train_pred)))
  roc_test <- suppressMessages(roc(as.numeric(y_test),as.numeric(y_test_pred)))
  auc_train2 <- pROC::auc(roc_train)
  auc_test2 <- pROC::auc(roc_test)
  th_train <- coords(roc_train, "b", ret = "t", best.method = "youden", transpose = T)
  y_train_pred_b <- as.factor(ifelse(y_train_pred > th_train,1,0))
  y_test_pred_b <- as.factor(ifelse(y_test_pred > th_train,1,0))
  cm_train <- caret::confusionMatrix(data = y_train_pred_b, reference = as.factor(y_train))
  cm_test <- caret::confusionMatrix(data = y_test_pred_b, reference = as.factor(y_test))
  acc_train2 <- acc(cm_train)
  acc_test2 <- acc(cm_test)
  se_train2 <- cm_train$byClass['Sensitivity']
  sp_train2 <- cm_train$byClass['Specificity']
  se_test2 <- cm_test$byClass['Sensitivity']
  sp_test2 <- cm_test$byClass['Specificity']
  mcc_train2 <- mcc(cm_train)
  mcc_test2 <- mcc(cm_test)
  
  #Fold3
  Train = new[-flds$Fold3,]
  Test  = new[flds$Fold3,]
  x_train = data.matrix(Train[,!names(Train) %in% outcome])
  y_train = as.matrix(Train[,outcome])
  x_test = data.matrix(Test[,!names(Train) %in% outcome])
  y_test = as.matrix(Test[,outcome])
  if(nlevels(as.factor(y_test))==1) return(data.frame())
  cvfit = cv.glmnet(x_train, y_train, type.measure = "class", alpha=0.9, nfolds = 10, family="binomial")
  y_train_pred = predict(cvfit, newx = x_train, s = "lambda.min")
  y_test_pred = predict(cvfit, newx = x_test, s = "lambda.min")
  roc_train = suppressMessages(roc(as.numeric(y_train),as.numeric(y_train_pred)))
  roc_test = suppressMessages(roc(as.numeric(y_test),as.numeric(y_test_pred)))
  auc_train3 = pROC::auc(roc_train)
  auc_test3 = pROC::auc(roc_test)
  th_train = coords(roc_train, "b", ret = "t", best.method = "youden", transpose = T)
  y_train_pred_b = as.factor(ifelse(y_train_pred > th_train,1,0))
  y_test_pred_b = as.factor(ifelse(y_test_pred > th_train,1,0))
  cm_train = caret::confusionMatrix(data = y_train_pred_b, reference = as.factor(y_train))
  cm_test = caret::confusionMatrix(data = y_test_pred_b, reference = as.factor(y_test))
  acc_train3 = acc(cm_train)
  acc_test3 = acc(cm_test)
  se_train3 = cm_train$byClass['Sensitivity']
  sp_train3 = cm_train$byClass['Specificity']
  se_test3 = cm_test$byClass['Sensitivity']
  sp_test3 = cm_test$byClass['Specificity']
  mcc_train3 = mcc(cm_train)
  mcc_test3 = mcc(cm_test)
  
  #Fold4
  Train = new[-flds$Fold4,]
  Test  = new[flds$Fold4,]
  x_train = data.matrix(Train[,!names(Train) %in% outcome])
  y_train = as.matrix(Train[,outcome])
  x_test = data.matrix(Test[,!names(Train) %in% outcome])
  y_test = as.matrix(Test[,outcome])
  if(nlevels(as.factor(y_test))==1) return(data.frame())
  cvfit = cv.glmnet(x_train, y_train, type.measure = "class", alpha=0.9, nfolds = 10, family="binomial")
  y_train_pred = predict(cvfit, newx = x_train, s = "lambda.min")
  y_test_pred = predict(cvfit, newx = x_test, s = "lambda.min")
  roc_train = suppressMessages(roc(as.numeric(y_train),as.numeric(y_train_pred)))
  roc_test = suppressMessages(roc(as.numeric(y_test),as.numeric(y_test_pred)))
  auc_train4 = pROC::auc(roc_train)
  auc_test4 = pROC::auc(roc_test)
  th_train = coords(roc_train, "b", ret = "t", best.method = "youden", transpose = T)
  y_train_pred_b = as.factor(ifelse(y_train_pred > th_train,1,0))
  y_test_pred_b = as.factor(ifelse(y_test_pred > th_train,1,0))
  cm_train = caret::confusionMatrix(data = y_train_pred_b, reference = as.factor(y_train))
  cm_test = caret::confusionMatrix(data = y_test_pred_b, reference = as.factor(y_test))
  acc_train4 = acc(cm_train)
  acc_test4 = acc(cm_test)
  se_train4 = cm_train$byClass['Sensitivity']
  sp_train4 = cm_train$byClass['Specificity']
  se_test4 = cm_test$byClass['Sensitivity']
  sp_test4 = cm_test$byClass['Specificity']
  mcc_train4 = mcc(cm_train)
  mcc_test4 = mcc(cm_test)
  
  #Fold5
  Train = new[-flds$Fold5,]
  Test  = new[flds$Fold5,]
  x_train = data.matrix(Train[,!names(Train) %in% outcome])
  y_train = as.matrix(Train[,outcome])
  x_test = data.matrix(Test[,!names(Train) %in% outcome])
  y_test = as.matrix(Test[,outcome])
  if(nlevels(as.factor(y_test))==1) return(data.frame())
  cvfit = cv.glmnet(x_train, y_train, type.measure = "class", alpha=0.9, nfolds = 10, family="binomial")
  y_train_pred = predict(cvfit, newx = x_train, s = "lambda.min")
  y_test_pred = predict(cvfit, newx = x_test, s = "lambda.min")
  roc_train = suppressMessages(roc(as.numeric(y_train),as.numeric(y_train_pred)))
  roc_test = suppressMessages(roc(as.numeric(y_test),as.numeric(y_test_pred)))
  auc_train5 = pROC::auc(roc_train)
  auc_test5 = pROC::auc(roc_test)
  th_train = coords(roc_train, "b", ret = "t", best.method = "youden", transpose = T)
  y_train_pred_b = as.factor(ifelse(y_train_pred > th_train,1,0))
  y_test_pred_b = as.factor(ifelse(y_test_pred > th_train,1,0))
  cm_train = caret::confusionMatrix(data = y_train_pred_b, reference = as.factor(y_train))
  cm_test = caret::confusionMatrix(data = y_test_pred_b, reference = as.factor(y_test))
  acc_train5 = acc(cm_train)
  acc_test5 = acc(cm_test)
  se_train5 = cm_train$byClass['Sensitivity']
  sp_train5 = cm_train$byClass['Specificity']
  se_test5 = cm_test$byClass['Sensitivity']
  sp_test5 = cm_test$byClass['Specificity']
  mcc_train5 = mcc(cm_train)
  mcc_test5 = mcc(cm_test)
  
  auc_train <- (auc_train1+auc_train2+auc_train3+auc_train4+auc_train5)/5
  auc_test <- (auc_test1+auc_test2+auc_test3+auc_test4+auc_test5)/5
  acc_train <- (acc_train1+acc_train2+acc_train3+acc_train4+acc_train5)/5
  acc_test <- (acc_test1+acc_test2+acc_test3+acc_test4+acc_test5)/5
  se_train <- (se_train1+se_train2+se_train3+se_train4+se_train5)/5
  sp_train <- (sp_train1+sp_train2+sp_train3+sp_train4+sp_train5)/5
  se_test <- (se_test1+se_test2+se_test3+se_test4+se_test5)/5
  sp_test <- (sp_test1+sp_test2+sp_test3+sp_test4+sp_test5)/5
  mcc_train <- (mcc_train1+mcc_train2+mcc_train3+mcc_train4+mcc_train5)/5
  mcc_test <- (mcc_test1+mcc_test2+mcc_test3+mcc_test4+mcc_test5)/5

  if(auc_test <= 0.5) return(data.frame())
  
  print(c)
  print("AUC	Accuracy	Sensitivity	Specificity	MCC")
  print(c(auc_test, acc_test, se_test, sp_test, mcc_test))
  
  return(data.frame(c, auc_train, auc_test, acc_train, acc_test, se_train, se_test, sp_train, sp_test, mcc_train, mcc_test))
}

#' muSignAl: Multiple Signature Algorithm.
#'
#' This function evaluates all the possible signatures
#' It requires an excel input for features as well as outcome variable
#' Any values not present will be imputed
#'
#' @export
muSignAl <- function(data, feature, outcome, nrun, thres) {
  core <- getFea(data, feature, outcome, nrun, thres)
  c <- str_c(str_sort(core), collapse = ",")
  if(length(core)<2) return(data.frame())
  else {
    signatures <- getPerf(data, feature, outcome, core)
    for(prot in core) {
      n_feature <- feature[!feature %in% prot]
      if(length(n_feature)<2) return(data.frame())
      n_sign <- muSignAl(data, n_feature, outcome, nrun, thres)
      signatures <- rbind(signatures, n_sign)
    }
  }
  return(signatures)
}
