## Let X be the iris data set without the Species column and only for the species setosa. Perform PCA on X
iris_dt <- as.data.table(iris)
X <- iris_dt[Species == "setosa", -"Species"]
pca <- prcomp(X, center=T, scale=T)
pca

## GOAL of PCA: Reduce dimensions but we want to keep as many as information
## from the original dataset as possible(e.g if two data points are far away in 3D
## they should be far in 2D)


summary(pca)
## proportion of variance in the 2nd row of summary
## for projection 1. compute matrix of PCs 2. multiply original data with PCs

## Plot the projection on the first two principle components
proj <- as.data.table(predict(pca)) ## and keep only first two columns for projected 2d space
ggplot(proj, aes(PC1, PC2))+
  geom_point()
biplot(pca)
## PC1 related to more about size of flower (sepal, petal = pc1 negative = larger flower)

## Plot the first principal component against the other variables in the dataset
# 1st method
X[, PC1 := proj$PC1]
melt <- melt(X, id.vars = "PC1")
ggplot(melt, aes(value, PC1))+
  geom_point()+
  facet_wrap(~variable, scales = "free")
# all linear relationships

# 2nd method
pc_iris <- cbind(iris_dt[Species=="setosa"], proj)
pc_iris_melted <- melt(pc_iris, id.vars = c("Species", "PC1", "PC2", "PC3", "PC4"))
ggplot(pc_iris_melted, aes(value, PC1))+
  geom_point()+
  facet_wrap(~variable, scales = "free")
## projection of PC1 is strongly correlated with all original variables
## -->
## large projected PC1 value <--> small flower (small values for original variables)

## Perform PCA on all the species of iris dataset
pca_data <- iris_dt[, -"Species"]
pca <- prcomp(pca_data, center=T, scale=T)

#1st method
biplot(pca)
predict <- as.data.table(predict(pca))
predict[,Species := iris_dt$Species]
ggplot(predict, aes(PC1, PC2, color=Species))+
  geom_point()

iris_dt_merge <- cbind(iris_dt, predict[,PC1])
pca_data_melt <- melt(iris_dt_merge, id.vars = c("Species", "V2"))
ggplot(pca_data_melt, aes(value, V2, color=Species))+
  geom_point()+
  facet_wrap(~variable, scales = "free")+
  labs(y="PC1")

#PC1 is negatively correlated with Sepal.Width
#Setosa has higher Sepal.Width

#2nd method
proj <- as.data.table(predict(pca))
pc_iris <- cbind(iris_dt, proj)
ggplot(pc_iris, aes(PC1, PC2, color = Species))+
  geom_point()

##PC1 differentiates Setosa from the rest two species
ggplot(pc_iris, aes(value, PC1, color=Species))+
  geom_point()+
  facet_wrap(~variable, scales = "free")
## Species <--> Flower Size <--> PC1 projection
## PC1 is irrelevant of PC1 in the former case







