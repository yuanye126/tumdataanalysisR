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
#Observations
smp_size <- floor(0.7 * nrow(diabetes_dt))
train_ind <- sample(seq_len(nrow(diabetes_dt)), size = smp_size)
