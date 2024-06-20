#python_machineLearnig_AMAZ
#install.packages("mclust")
library(ggplot2)
library(mclust)
library(readxl)

ciano <- read_excel("/Users/USER/Desktop/novas_analises_meta/graficos.xlsx", sheet = "ciano")
ciano <- t(ciano)
colnames(ciano) <-ciano[1,]
ciano <- ciano[-1,]
ciano <- as.data.frame(ciano)
mcl <- Mclust(ciano)
summary(mcl)
cluster <- factor(predict(mcl)$classification)
ggplot(data=ciano, aes(x=ciano$Phormidium, y=ciano$Nodularia, color=cluster, shape=cluster)) +
  geom_point(alpha=.8)


plot(mcl, what='BIC', ask=FALSE)
#Biblio NCBI
summary(mcl$BIC)

drmod <- MclustDR(mcl, lambda = 1)
summary(drmod)
plot(drmod, what = 'contour')

#ARG (tentar)
args <- read_excel("/Users/USER/Desktop/novas_analises_meta/graficos.xlsx", sheet = "arg")
args <- t(args)
colnames(args) <-args[1,]
args <- args[-1,]
args <- as.data.frame(args)
mcl2 <- Mclust(args)
summary(mcl2)
summary(mcl2$BIC)

cluster_2 <- factor(predict(mcl2)$classification)
plot(mcl2, what='BIC', ask=FALSE) 

table(cluster_2, mcl2$classification)
adjustedRandIndex(cluster_2, mcl2$classification)
drmod2 <- MclustDR(mcl2, lambda = 1)
summary(drmod2)

c <- as.data.frame(drmod2[["dir"]])
#library(dplyr)

args['dir1'] <- c$Dir1
args['dir2'] <- c$Dir2

plot(drmod2, what = 'contour')
plot(drmod2, what = 'boundaries', ngrid = 200)
plot(drmod2, what = 'classification', main = FALSE)

ggplot(data=args, aes(x= args$dir1, y=args$dir2, color=cluster_2, shape=cluster_2)) +
  geom_point(alpha=.8)