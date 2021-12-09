library(ggplot2) 
library(data.table) 
library(magrittr) 
library(tidyr)
library(dplyr)
library(datasets)
#t test works for the normal distribution
#Wilcoxon test does not need normal distribution
#pearson no linear relationship
#spearman no monotonic relationship
gene <- fread("/Users/yeyuan/Downloads/extdata-4/eqtl/gene.txt")
genotype <- fread("/Users/yeyuan/Downloads/extdata-4/eqtl/genotype.txt")
genotype <- melt(genotype, id.vars = 'strain', variable.name = 'marker',
                 value.name = 'genotype') 
growth <- fread("/Users/yeyuan/Downloads/extdata-4/eqtl/growth.txt")
growth <- melt(growth, id.vars = "strain", variable.name = 'media', value.name = 'growth_rate')
marker <- fread("/Users/yeyuan/Downloads/extdata-4/eqtl/marker.txt")
#section 1
#1 Spearman test, no normal distribution, two continuous variables
#2 t test, binary and continuous with gaussian distribution
#3 binomial test, null hypothesis: There is 99% chance that it can identify whether 
#4 Fisher's F test

#section 2
getMaltoseDt <- function(mrk){ 
  growth_mrk <- merge(growth, genotype[marker == mrk, .(strain, genotype)],by = 'strain') 
  growth_mrk[media == "YPMalt"]}

plot_growth_one_mk <- function(mk){ ggplot(getMaltoseDt(mk), aes(genotype, growth_rate)) + geom_boxplot() +
    labs(title = mk) + theme_bw(base_size = 16) + theme(plot.title = element_text(hjust = 0.5))
} 
plot_growth_one_mk("mrk_5211")
#two binary variable, we will choose F test to check the correlation

# H0: there is no difference in means
# H1: there is difference in means
ttest <- t.test(growth_rate ~ genotype, getMaltoseDt("mrk_5211"), alternative = "two.sided", var.equal = TRUE)
ttest
#t test with Welch correction
# t test without Welch correction
# Wilcoxon rank sum test
wilc <- wilcox.test(growth_rate ~ genotype, getMaltoseDt("mrk_5211"), alternative = "two.sided")
wilc
#section 2.3
test_growth <- function(mk, test){
  m_dt <- getMaltoseDt(mk)
  if(test == 'wilcoxon') {
    wtest <- wilcox.test(growth_rate ~ genotype, m_dt, alternative = "two.sided")
    pval <-wtest$p.value
  } else {
    tt <- t.test(growth_rate ~ genotype, m_dt, alternative = "two.sided")
    pval <- tt$p.value
  }
  return(pval)
}
#data frane used in the function will not be documented as global variables, only returned value will be documented

test_growth("mrk_1653",'wilcoxon')

#Section 3
cor.test(iris$Sepal.Width , iris$Sepal.Length,method = "spearman")
cor(iris$Sepal.Length,iris$Sepal.Width, method = "pearson") # normality
#section 3.2
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point() + geom_smooth(method = "lm")+ facet_wrap(~Species)
iris_dt <- data.table(iris)
iris_setosa <- iris_dt[Species == "setosa"]
cor(iris_setosa$Sepal.Length, iris_setosa$Sepal.Width, method = "spearman")
cor.test(iris_setosa$Sepal.Length, iris_setosa$Sepal.Width, method = "spearman")
corr_dt <- iris_dt[, cor.test(Sepal.Length, Sepal.Width, method = "pearson"), by = Species]
corr_dt
