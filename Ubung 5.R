#Heap Map, used to show the distribution of the points.
#From the Heatmaps, we have seen some outliers and we removed them.

install.packages("pheatmap")
install.packages("mclust")
library(ggplot2) 
library(data.table) 
library(magrittr) 
library(tidyr) 
library(GGally)
library(pheatmap)
library(mclust)
expr <-readRDS("/Users/yeyuan/Downloads/extdata-2/cancer_data.rds") %>% as.data.table(keep.rownames="tumor_type") 
head(expr[, 1:6])
ggcorr(expr[, - "turbo_type"])

mat <- as.matrix(expr[,-"tumor_type"])
rownames(mat) <- expr[, tumor_type]

head(mat)
pheatmap(mat, cluser_cols = FALSE, cluster_rows = FALSE)
expr_melt <- melt(expr, id.vars =  "tumor_type")
expr_melt[order(-value)]
expr[tumor_type == "DOHH2", FUK:= NA]
expr[tumor_type == "DOHH2", FUK := NA]

#And we create the heapmap again.

#Section 2
iris_table <- as.data.table(iris)
head(iris_table)
iris_newtable <- iris_table[,1:4]
head(iris_newtable)
irismat <- as.matrix(iris_newtable)
rownames(irismat) <- iris_table[,setosa]
pheatmap(irismat, cluster_cols =  FALSE, cluster_rows =  FALSE, scale =  "column")
#2
pheatmap(irismat, clustering_method =  "complete", scale = "column")
#3 Dendrogram 
x <- scale(irismat)
d <- dist(x)
hc <- hclust(d)
plot(d,hang = -1)

hc_complete <- pheatmap(irismat, clustering_method =  "complete", scale =  "column", silent = T)

#cut the hierarchical tree, which means take three clusters out.
complete <- cutree(hc_complete$tree_row,k =3)
#set row names for iris_mat
rownames(irismat) <- 1:nrow(irismat)
rownames(irismat)
row.ann <- data.table(Species = iris_table$Species)
row.ann[,complete := factor(complete)]
# create a data table to compare the clustering result & true species of each row
#create heapmaps with annotation
pheatmap(irismat, clustering_method = "complete", scale =  "column", annotation_row = row.ann, show_rownames = FALSE)
#at last compare the result of the clusters and the real data.

