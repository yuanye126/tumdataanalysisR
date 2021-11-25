#Section 1
# A typical data analysis starts with an exploration of the data.
#This is illustrated with descriptive plots showing how the data is distributed. This allows to get familiar with the dataset in an unbiased fashion.
#2 Afterwards, one typically highlights some interesting relationships between variables. This is often in order to suggest a testable, causal relationship.
#We call the latter types of plots associative plots. 
library(ggplot2)
library(data.table)
library(magrittr) # Needed for %>% operator library(tidyr)

# If A and B correlate and A happens before B, then A causes B. anti Example: Rooster crowds abd sun rises

# If A causes B and A causes C, then B also causes C. Common cause
# If A and B correlate and A happens before B, then A causes B. 
# Causation implies linear association.(quadratic, exponential, does not have to be linear)
# Reverse Causality: A and B correlate, you think A causes B, but in reality B causes A

#Section 2
coffee_dt <- fread("/Users/yeyuan/Downloads/extdata-3/coffee_sim.csv") 
summary(coffee_dt)
ggplot(coffee_dt, aes(cups_per_day, risk)) + geom_boxplot()  
#boxplot used for a categorical data!
#From the box plot we could see that, there may be  a correlation, but we are not sure about the causality.
ggplot(coffee_dt,aes(packs_per_day, risk)) + geom_boxplot()
#Seems that there is a stronger correlation between packs and risk
ggplot(coffee_dt, aes(cups_per_day, risk)) + geom_boxplot() + facet_grid(~packs_per_day)
ggplot(coffee_dt, aes(packs_per_day, risk)) + geom_boxplot() + facet_grid(~cups_per_day)
#from the two plots we could see that there is a causaulity between packs and risk and cups do not play a role in it.

coff_melt <- melt(coffee_dt, id.vars = "risk")
ggplot(coff_melt, aes(value, risk, color = variable)) + geom_point()
ggplot(coffee_dt, aes(packs_per_day, risk, color= cups_per_day))+ geom_boxplot()
#add the color inside the aes function to let us draw multiple boxlplots.
ggplot(coffee_dt, aes(cups_per_day, packs_per_day)) + geom_point()
#common cause and indirect cause
table(coffee_dt[,c("cups_per_day", "packs_per_day")])
#Section 4
#good: simple design, clear labels, no color, no chart junk
#bad: scale of the y axis could be around where the data lies (8000-13000)
#suggestions: plot single points(+boxplot) instead of bars
#sort mutants by median 
#give color for above and below WT
genome_dt <- data.table(pro_uptake = c(rnorm(3, 10100, 300), rnorm(4, 12100, 300), rnorm(3, 9850, 300), rnorm(4, 11100, 300),    rnorm(4,8300, 300), rnorm(3,10050, 300), rnorm(3, 12000, 300), rnorm(3, 10020, 300), rnorm(3, 10080, 300), rnorm(3, 10070, 300) ), mutants = c(rep("WT",3), rep("T49A",4), rep("K227N",3), rep("A400V",4), rep("L421P",4), rep("I500T",3), rep("N591D",3), rep("A601T",3), rep("E684D",3), rep("G710R",3) ) )
summary(genome_dt)
ggplot(genome_dt, aes(mutants, pro_uptake)) + geom_boxplot()
#1. calculate the median
genome_dt[,medians := median(pro_uptake), by = mutants]
genome_dt
wt_med <- unique(genome_dt[mutants == "WT", medians])
genome_dt[,mutants := factor(mutants, level = unique(genome_dt[order(medians),mutants]))]
genome_dt[, rel_to_wt := ifelse(medians < wt_med,"SmallerthanWT","LargerthanWT")]
genome_dt[mutants == "WT", rel_to_wt := "WT"]
genome_dt
ggplot(genome_dt, aes(mutants, pro_uptake, fill = rel_to_wt)) + geom_boxplot()
