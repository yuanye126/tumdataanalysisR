library(ggplot2) 
library(data.table) 
library(magrittr) 
library(tidyr) 
library(ggrepel)
library(plotROC) 
library(caret) 
library(rpart) 
install.packages(c("caret","randomForest"))
library(randomForest)
#Models trained using cross validation do not over-fit. Wrong. Does not lead to overfitting for sure.
diabetes_dt <- fread("/Users/yeyuan/Downloads/extdata-4/pima-indians-diabetes.csv") 
diabetes_dt[, Outcome := as.factor(Outcome)]
# Store feature variables that we will need for later
feature_vars <- colnames(diabetes_dt[,-c("Outcome")])
diabetes_dt
full_formula <- as.formula(paste(c("Outcome ~ ",
                                   paste(feature_vars, collapse = " + ")),
                                 collapse = ""))
full_formula
# We do not use this command full_formula <- Outcome~.
# Since we are adding some columns later and we do not want the formula to be altered
#decision tree classifier
dt_classifier <- rpart(full_formula,
                       data =diabetes_dt,
                       control = rpart.control(minsplit = 3, cp = 0.001))
predicteddata<- predict(dt_classifier, type = "prob")[,2]
diabetes_dt[,predict_Dt := predicteddata]#minimum observations in one leaf, 
#cp determines when you stop improving
ggroc <- ggplot(diabetes_dt, aes(d = as.numeric(Outcome), m = predict_Dt)) + geom_roc() + geom_abline()
ggroc
set.seed(13)
#Observations, floor function rounding down the decimal number
smp_size <- floor(0.7 * nrow(diabetes_dt))
train_ind <- sample(seq_len(nrow(diabetes_dt)), size = smp_size)
#build dt_classifier
dt_classifer <- rpart(full_formula,data = diabetes_dt[train_ind],control = rpart.control(minsplit = 3, cp = 0.001))
#mark test & training data
diabetes_dt[, predictedd := predict(dt_classifer,type ="prob", 
                                              newdata = diabetes_dt)[,2]]
diabetes_dt[train_ind, dataset := "train"]
diabetes_dt[-train_ind, dataset := "test"]
ggroc <- ggplot(diabetes_dt, aes(d=as.numeric(Outcome), m = predictedd, color = dataset)) + geom_roc() + geom_abline()
ggroc
rf_classifier <- randomForest(formula= full_formula,data = diabetes_dt[train_ind],
                              ntree = 200,#number of trees
                              nodesize =20, #minimum size of a leaf node, 
                              maxnodes = 7,#maximum number of leaf node
                              mtry =5) #number of featured variables considered for the trees)
rf_classifier
diabetes_dt[,rfpredict := predict(rf_classifier, type = "prob", newdata = diabetes_dt)[,2]]
ggrf <- ggplot(diabetes_dt, aes(d= as.numeric(Outcome), m = rfpredict, color = dataset)) + geom_roc() + geom_abline()
ggrf

#Section 5

