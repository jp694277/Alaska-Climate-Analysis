###########################################
#climate
###########################################
library(readxl)
CL <- "climate.xls"
excel_sheets(CL) # List the sheet names
CL <- read_excel(CL,sheet = "Climate", na = "NA")
View(CL)
head(CL, 3)
str(CL)
###########################################
#PCA
###########################################
CL.pca <-princomp(CL[,2:18], cor=TRUE, scores=TRUE)
# 2D plot for first two components
pca.dim1 <- CL.pca$scores[,1]
pca.dim2 <- CL.pca$scores[,2]
plot(pca.dim1, pca.dim2, main="PCA for Climate", xlab="1st PCA
     Component", ylab="2nd PCAComponent")
# shows a screeplot.
install.packages("scales")
library(factoextra)
library(ggplot2)
eig.val <- get_eigenvalue(CL.pca)
fviz_eig(CL.pca, addlabels =TRUE, ylim = c(0, 50))
plot(CL.pca)
biplot(CL.pca)
summary(CL.pca)

# color individuals by their cos2 values
fviz_pca_ind(CL.pca, col.ind= "cos2", gradient.cols= c("blue", "black", "red"),repel = TRUE)

library(pcaMethods)
var <- get_pca_var(CL.pca)
fviz_pca_var(CL.pca,col.var = "black")
# variables with low/mid/high cos2 values will becolored in blue/yellow/red
fviz_pca_var(CL.pca,col.var = "cos2",gradient.cols =c("blue", "yellow", "red"),repel = TRUE)# Avoid text overlapping

windows()
par(mfrow=c(1,2))
# Contributions of variables to PC1
fviz_contrib(CL.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(CL.pca, choice = "var", axes = 2, top = 10)
# The total contribution to PC1 and PC2:
fviz_contrib(CL.pca, choice = "var", axes = 1:2, top = 10)

fviz_pca_var(CL.pca,col.var = "contrib",gradient.cols =c("blue", "yellow", "red"))
set.seed(123)
var.kms <- kmeans(var$coord, centers = 3, nstart = 25)
kms.grp <- as.factor(var.kms$cluster)
# Color variables by kmeans' result
fviz_pca_var(CL.pca, col.var= kms.grp, palette =c("blue", "green", "red"),legend.title = "Cluster")

###########################################
#FA
###########################################
data <- Iris[,1:4]
class <- Iris[,5]
CL.fa <- factanal(CL[,2:18], factors=1)
fa.dim1 <- as.matrix(CL[,2:18])%*%CL.fa$loadings[,1]
CL.fa
#########################################
hw7_9 <- read.table("SCORE.DAT", header=TRUE, row.names=1)
SC <- as.matrix(hw7_9)

## create heatmap using pheatmap
library(pheatmap)
library("DESeq")

pheatmap(CL[,2:18])
pheatmap(CL[,2:18], clustering_method = "average", main = "average")

cal_z_score <- function(x){
  (x - mean(x)) / sd(x)
}

data_subset_norm <- t(apply(CL[,2:18], 1, cal_z_score))

pheatmap(data_subset_norm, clustering_method = "complete", 
         main = "complete")
pheatmap(data_subset_norm, clustering_method = "average", 
         main = "average")


my_hclust_gene <- hclust(dist(data_subset_norm), method = "complete")
my_hclust_gene <- hclust(dist(data_subset_norm), method = "average")

# load package
library(dendextend)

as.dendrogram(my_hclust_gene) %>%
  plot(horiz = F)



library("cluster")
hw79.pam <- pam(data_subset_norm, k=4)
plot(hw79.pam)
hw79.pam <- pam(data_subset_norm, 4)$clustering
clusplot(data_subset_norm, hw79.pam, color = TRUE)

library(factoextra)
CL.scaled <- scale(CL[,2:18])
CL.km <- kmeans(CL.scaled,  centers=4)
fviz_cluster(CL.km, CL[,2:18])
fviz_cluster(CL.km, CL[,2:18], ellipse.type = "norm")

