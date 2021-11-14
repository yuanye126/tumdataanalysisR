install.packages("ggrepel")
install.packages("GGally")
library(ggplot2)
library(data.table)
library(magrittr) # Needed for %>% operator library(tidyr)
library(ggrepel)
library(GGally)

#section 1
##1. shows distribution and quantiles, especially useful when comparing uni-modal distributions. Boxplot
#2. highlights individual values, supports comparison and can show rankings or deviations categories and
#totals                                                        Bar Chart
#3. shows overall changes and patterns, usually over intervals of time Bar chart
#4. shows relationship between two continuous variables.         scatter plot
#Options: bar chart, line chart, scatterplot, boxplot

#Section 2
mpg <- as.data.table(mpg)
mpg_new <- mpg[year%in% c(1999,2008)]
mpg_new$year <- as.factor(mpg_new$year)
ggplot(data = mpg_new, aes(cty, hwy)) + geom_point(aes(color =year)) + geom_smooth(aes(color= year), method = "lm")
#ggplot(mpg_new, aes(cty,hwy, color = factor(year)))  geom_smooth(method = "lm") 

iris_table <- as.data.table(iris)
head(iris_table)
iris_table <- melt(iris_table, measure.vars = c("Sepal.Width", "Petal.Width" ), variable.name = "Widthtype",value = "Widthvalue")
iris_table <- melt(iris_table, measure.vars = c("Sepal.Length", "Petal.Length" ), variable.name = "Lengthtype",value = "Lengthvalue")
head(iris_table)

ggplot(iris_table, aes(Widthvalue)) + geom_histogram() + facet_wrap(~Widthtype)

iris_table <- melt(iris_table, id.vars = "Species")
ggplot(iris_table, aes(value)) + facet_wrap(~variable) + geom_histogram(bins = ...)
head(iris_table)
ggpairs(iris_table,columns = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
ggplot(iris_table, aes(Petal.Length, Sepal.Length, group = Species)) + geom_boxplot() + geom_jitter() + geom_violin()
ggplot(iris_table, aes(variable, value)) + geom_boxplot() + geom_jitter() + geom_violin()

#boxplot not good, because in the petal.width and petal.length, most of the points are distributed two sidely, which means there are two modes(two centre) here

#7.   The species plays a role of why we have two modes.



