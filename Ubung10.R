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
mheight <- heights[sex %in% c("M")]
fheight <- heights[sex %in% c("F")]
mlm <- lm(height ~ father, data = mheight)
flm <- lm(height ~ father, data = fheight)
ggplot(mheight, aes(x= father, height)) + geom_point() + geom_smooth(method = "lm")
ggplot(fheight, aes(x = father,height)) + geom_point()  + geom_smooth(method = "lm")+ geom_line(aes(father,height,color = "predicted father"))

pca_pbj <- princomp(mheight[,.(height,father)])
pca_pbj$loadings
slope <- pca_pbj$loadings["height","Comp.1"] / pca_pbj$loadings["father","Comp.1"]
intercept <- pca_pbj$center["height"] -slope* pca_pbj$center["father"]
geom_abline(aes(intercept = intercept, slope = slope ,color = "pc_1"))
#PCA basically minize along the axis y and x.
#in the PCA, Sum of squares along all the axises are minimized.

#section 2
growth <- genotype_growth %>% melt(id.vars="strain", variable.name='media', value.name='growth_rate')
growth <- growth[media=="YPMalt"]
genotype <- genotype[, c(strain, "mrk_5211", "mrk_5091")]
full <- lm(growth_rate ~ mrk_5211 + mrk_5091)



