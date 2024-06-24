library(factoextra)
#Carregando pacotes exigidos: ggplot2
#Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
library(FactoMineR)
library(readxl)
PCA_ <- read_excel("~/Desktop/PCA .xlsx")
View(PCA_)                                                                                                      
agua<-PCA_
agua<-na.omit(agua)
res_pca <- PCA(agua, quali.sup = 1)
#Warning message:
  #In PCA(agua, quali.sup = 1) :
  #Missing values are imputed by the mean of the variable: you should use the imputePCA function of the missMDA package
summary(res_pca)


 
sweep(res_pca$var$coord,2,sqrt(res_pca$eig[1:ncol(res_pca$var$coord),1]), FUN="/")
 
fviz_contrib(res_pca, choice = "var", axes = c(1,2))
fviz_eig(res_pca, addlabels = T, ylim = c(0,90))
fviz_pca_biplot(res_pca, geom.ind = "point", pointshape = 21, pointsize = 3, fill.ind = agua$Local)+theme_bw()
fviz_pca_biplot(res_pca, geom.ind = "point", pointshape = 21, pointsize = 2, fill.ind = agua$Local)+theme_bw()+labs(title = "Gráfico PCA Qualidade da Água", fill = "Local de Coleta")
fviz_pca_var(res_pca, col.var = "blue")
