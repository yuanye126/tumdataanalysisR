library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
library(patchwork) # optional, makes plots nicer library(cowplot)
heights <- fread("/Users/yeyuan/Downloads/extdata-4/height.csv") %>% na.omit() %>% .[, sex:=as.factor(toupper(sex))]
head(heights)
lm1<- lm(height ~ sex + mother + father,data = heights)
heights[,.N,by =sex]
summary(lm1)
m <- heights[sex %in% c("M","F"),lm(height~sex+mother+father)]
summary(m)
#section 1.2
res <-residuals(m)
plot(residuals(m))
pre <-predict(m)
table <-as.data.table(res,pre)
ggplot(table, aes(x=pre,y=res)) + geom_point() +geom_hline(yintercept = 0)
ggplot(table,aes(sample = res)) + geom_qq() + geom_qq_line() #to see the distribution(gaussian distribution), we can then be confident that we are using linear regression because it requires normal distribution

