library(ggplot2) 
library(data.table) 
library(magrittr)
library(tidyr) 
library(dplyr) 
install.packages("patchwork")
library(patchwork)
genotype <-fread("/Users/yeyuan/Downloads/extdata-4/eqtl/genotype.txt")
growth_rate <-fread("/Users/yeyuan/Downloads/extdata-4/eqtl/growth.txt")
marker <-fread("/Users/yeyuan/Downloads/extdata-4/eqtl/marker.txt")
setnames(marker, "id", "marker") 
genotype <- genotype %>%
  melt(id.vars = "strain", variable.name = "marker", value.name = "genotype")

#1. generate normal distribution
set.seed(10)
n <- 100
draws <- rnorm(n)
dt <- data.table(observed = draws)
dt
hist <- ggplot(dt, aes(observed)) + geom_histogram(bins =20) + xlim(-6,6) + labs( x = "observed_quantiles")
hist
#In that way, we still could not see clearly that this follows a normal distribution
qq <- ggplot(dt, aes(sample = observed)) + geom_qq(distribution =  stats::qnorm) + geom_abline(slope =  1, intercept = 0) 
qq

#plot_qq <- function(dt, observedquantiles){}

#Section 1.2
dt[, rshift := sort(rnorm(n, mean= 4))]
plot_qq(dt, "rshift")
# we could see that the dots moving upwards
 #increase the standard deviation

#section 2
genotype_growth <- merge(genotype, growth_rate, by = "strain")
test_res <- genotype_growth[, .(pval =wilcox.test(YPMalt ~ genotype)$p.value), by = "marker"]
ggplot(test_res, aes(pval)) + geom_histogram(boundary = TRUE, bins = 50)

ggplot(test_res[order(pval)], aes(-log10(ppoints(pval)), -log10(pval))) + geom_point() + geom_abline()

#Section 2.3, do we need to correct for multiple testing?
#Yes, we have done 1000 tests. Two methods: Bonferroni

test_res[, padj := p.adjust(pval, method = "BH")]
test_res[padj < 0.05][order(padj)]

newtest <- merge(marker, test_res, by = "marker")
ggplot(newtest,aes(start, -log10(pval)) ) + geom_point() +facet_grid(~chrom, scales = "free_x") + theme(axis.text.x = element_blank())
newtest[padj < 0.05,.N]
