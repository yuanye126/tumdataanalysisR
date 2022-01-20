library(ggplot2) 
library(data.table) 
library(magrittr) 
library(tidyr) 
library(ggrepel)

library(plotROC)
diabetes_dt <- fread("/Users/yeyuan/Downloads/extdata-4/pima-indians-diabetes.csv") 
diabetes_dt[, Outcome := as.factor(Outcome)]
diabetes_dt
feature_vars <- colnames(diabetes_dt[,-c("Outcome")])
#1.check how balanced the classes are
#Keep in mind wenn man Logistic Regression nutzt. From the outcome, we could see that there is 65% chance that does not have a diabete
# Unbalanced sample should be considered.
diabetes_dt[,.N/nrow(diabetes_dt),by= "Outcome"]
feature_vars
summary(diabetes_dt)
#a categorical variable and a numerical variable: Boxplot
diabetes_dt_melted <- melt(diabetes_dt[,.(Outcome,Glucose,Insulin,BloodPressure)], id.vars = "Outcome")
ggplot(data = diabetes_dt_melted, aes(value, Outcome) ) + geom_violin() + facet_wrap(~value)

#section 3
logistic_fit <- glm(formula = Outcome ~ Glucose, family = "binomial",data=  diabetes_dt)
summary(logistic_fit)

#what does the co efficient mean?
#log-odds(Outcome) = beta_0(Intercept) + glucose * beta_1
#delta log-oads = beta_1 * 1 = 0.038\
#odds = exp(o.o038) = 1.04
# suppose glucose = 130 => odds = 1:1
#         glucose = 131 =  odds = 1*1.04 :1
#         glucose = 140 =  odds = 1.04 ^10 :1
#interpretation : an increase of glucose in 1 mg/dl would change increasing your odds of having diabetes by 4%

#Section 4
diabetes_predicted <- predict(logistic_fit, data = diabetes_dt, type = "response")
summary(diabetes_predicted)
diabetes_dt[,predicted := diabetes_predicted]
ggplot(diabetes_dt, aes(predicted, fill = Outcome)) + geom_histogram(position = "dodge")
#It does not perform that well if we just simply use glucose as variable
ggplot(diabetes_dt,aes(Glucose, predicted)) + geom_point() 


confusion_matrix <- function(dt, score_column, labels_column, threshold){
  confusion_matrix(dt[,get(score_column)], dt[,get(labels_column)])
  
  
}
confusion_matrix(diabetes_dt,"predicted","Outcome",0)
