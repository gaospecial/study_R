library(mixOmics)

data(multidrug)

## this data set contains missing values, therefore 
## the 'prcomp' function cannot be applied
pca.res <- pca(multidrug$ABC.trans, ncomp = 4, scale = TRUE)
plot(pca.res)
print(pca.res)
biplot(pca.res, xlabs = multidrug$cell.line$Class, cex = 0.7)

# samples representation
plotIndiv(pca.res, ind.names = multidrug$cell.line$Class, cex = 0.5, 
          col = as.numeric(as.factor(multidrug$cell.line$Class)))
plot3dIndiv(pca.res, cex = 0.2,
            col = as.numeric(as.factor(multidrug$cell.line$Class)))

# variables representation
plotVar(pca.res, var.label = TRUE)
plot3dVar(pca.res, rad.in = 0.5, var.label = TRUE, cex = 0.5)
