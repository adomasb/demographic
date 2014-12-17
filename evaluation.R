library(xtable)
library(ggplot2)

# load models, which are saved as files
path <- "/home/adomas/Dropbox/models/"
allModels <- dir(path, full.names=TRUE,ignore.case = T)
for (i in 1:length(allModels)){
  load(allModels[i])
}

# gives reported scores
# from given model
modelEvaluation <- function(modelList){
  model <- modelList$model
  M_train <- modelList$M_train
  M_test <- modelList$M_test
  y_train <- modelList$y_train
  y_test <- modelList$y_test
  
  y_hat <- predict(model, M_test, type="response")
  # accuracy
    
  predTest <- prediction(y_hat, y_test)
  perfTest <- performance(predTest, measure="tpr", x.measure="fpr")
  perfTestPR <- performance(predTest, measure="prec", x.measure="rec")
  perfTestLift <- performance(predTest, measure="lift", x.measure="rpp")
  perfTestFPR <- performance(predTest, measure = "fpr")
  perfTestRecall <- performance(predTest, measure = "rec")
  perfAUC <- performance(predTest, measure="auc")
  
  #plot(perfTest)
  # precision recall curve      
  #plot(perfTestPR)
  # lift
  #plot(perf)
  
  evalList <- list()
  # cv auc
  evalList$auc_cv <- round(max(model$cvm), 3) 
  # test auc
  evalList$auc_test <- round(perfAUC@y.values[[1]], 3)
  # accuracy
  evalList$accuracy_test <- round(mean(1*(y_hat > 0.5) == y_test), 3)
  # fpr
  evalList$fpr <- round(perfTestFPR@y.values[[1]][which(round(perfTestFPR@x.values[[1]],3)==0.500)[1]],3)
  # recall
  evalList$recall <- round(perfTestRecall@y.values[[1]][which(round(perfTestRecall@x.values[[1]],3)==0.500)[1]], 3)
  return(evalList)
}

# AUC with different number of URL limits
auc_cv_by_url <- function(){
  models_list1 <- list(gender_25, gender_50, gender_100, gender_150, gender_250,
                       income_low_25, income_low_50, income_low_100, income_low_150, income_low_250,
                       income_medium_25, income_medium_50, income_medium_100, income_medium_150, income_medium_250,
                       income_high_25, income_high_50, income_high_100, income_high_150, income_high_250)
  auc_cv <- c()
  for (m in 1:length(models_list1)){
    auc_cv <- c(auc_cv, modelEvaluation(models_list1[[m]])$auc_cv)
  }
  table <- data.table(Model=c(rep("Gender Female", 5), rep("Income Low", 5), rep("Income Medium", 5), rep("Income High", 5)),
             Limit = rep(c(25, 50, 100, 150, 250), 4),
             AUC_CV = auc_cv)
  return(table)
}

# Plottin auc with diff url limits
ggplot(auc_cv_by_url(), aes(Limit, AUC_CV, color=Model))+
  geom_line()+geom_point()+
  xlab("Minimal number of domains clicked")+
  ylab("AUC CV")
ggsave("/home/adomas/Dropbox/browsing\ behavior/desc/URL_limit.png", width = 7, height = 4)

# first table, where limits are 25 & 25

gender_25_eval <- modelEvaluation(gender_25)
income_high_25_eval <- modelEvaluation(income_high_25)
income_medium_25_eval <- modelEvaluation(income_medium_25)
income_low_25_eval <- modelEvaluation(income_low_25)

firstTable <- data.table(Model=c("Gender Female", "Income Low", "Income Medium", "Income High"),
                         AUC_CV = c(gender_25_eval$auc_cv, income_low_25_eval$auc_cv, income_medium_25_eval$auc_cv, income_high_25_eval$auc_cv),
                         AUC_LIMIT_CV = c(gender_250_eval$auc_cv, income_low_100_eval$auc_cv, income_medium_150_eval$auc_cv, income_high_250_eval$auc_cv),
                         Limit = c(250, 100, 150, 250))
xtable(firstTable, digits=3)

# final table
gender_250_eval <- modelEvaluation(gender_250)
income_high_250_eval <- modelEvaluation(income_high_250)
income_medium_150_eval <- modelEvaluation(income_medium_150)
income_low_100_eval <- modelEvaluation(income_low_100)

finalTable <- data.table(Model=c("Gender Femal", "Income Low", "Income Medium", "Income High"),
                            AUC_CV = c(gender_250_eval$auc_cv, income_low_100_eval$auc_cv, income_medium_150_eval$auc_cv, income_high_250_eval$auc_cv),
                            AUC_test = c(gender_250_eval$auc_test, income_low_100_eval$auc_test, income_medium_150_eval$auc_test, income_high_250_eval$auc_test),
                            Accuracy = c(gender_250_eval$accuracy_test, income_low_100_eval$accuracy_test, income_medium_150_eval$accuracy_test, income_high_250_eval$accuracy_test),
                            FPR = c(gender_250_eval$fpr, income_low_100_eval$fpr, income_medium_150_eval$fpr, income_high_250_eval$fpr),
                            Recall = c(gender_250_eval$recall, income_low_100_eval$recall, income_medium_150_eval$recall, income_high_250_eval$recall))

xtable(finalTable)
