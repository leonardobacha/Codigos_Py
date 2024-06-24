library(readxl)
library(reshape2)
library(FactoMineR)
library(factoextra)
library(cluster)
big <- read_excel("/Users/USER/Desktop/pca.xlsx")
row.names(big) <- big$RIO
big <- big[,2:3]
big <- big[,c('PC1', 'PC2')]
km <- kmeans(big, centers=4)
big$cluster <- factor(km$cluster)
head(big)
centers <- data.frame(cluster=factor(1:4), km$centers)
centers

ggplot(data=df, aes(x=PC1, y=PC2, color=cluster, shape=cluster)) +
  geom_point(alpha=1) +
  geom_point(data=centers, aes(x=PC1, y=PC2), size=1, stroke=2)

km <- kmeans(df, centers=4)
df$cluster <- factor(km$cluster)


row.names(df) <- d$RIO
df$RIO <- NULL
library(tidyverse)
df %>% column_to_rownames(., var = "RIO")
#Video que mostra cluster por k-means e dendograma
#Chamar pacotes
library(FactoMineR)
library(factoextra)
library(cluster)
#tem um comando interessante chamado file.choose que dá o caminho do arquivo

#Importar do excel
dados_brutos <- read_excel("/Users/USER/Desktop/BIG_PLANILHA_RIO_historico.xlsx", sheet = "cluster_ana_2022")
f <- dados_brutos[,-1]
d <- dados_brutos
#Tirar a coluna cliente e colocar ela como rotulo de linha
row.names(d) <- d$RIO

#agrupar em cluster no dendograma
cluster <- hclust(dist(d))
plot(cluster)

tiff(filename = 'cl', width = 8, height = 6, res = 1000, units = 'in')
plot(cluster)

dev.off()

rect.hclust(cluster, k=4, border = 2.5)

#numero otimo de clusters
fviz_nbclust(f, kmeans, method = "gap_stat")
fviz_nbclust(f, kmeans, method = "silhouette")
fviz_nbclust(f, kmeans, method = "wss")

#gerar kmeans
dados_kmeans <- kmeans(f, 4)
#visualizar no gráfico
fviz_cluster(dados_kmeans, data = f, ellipse.type = "euclid", palette ="Set2", ggtheme = theme_minimal())

tiff(filename = 'test', width = 8, height = 6, res = 1000, units = 'in')

fviz_cluster(dados_kmeans, data = f, ellipse.type = "euclid", palette ="Set2", ggtheme = theme_minimal())

dev.off()
#se eu quiser adicionar o cluster aos dados
lista <- dados_kmeans$cluster
dados_gerais <- cbind(dados, lista)

#PCA
library(factoextra)
#Carregando pacotes exigidos: ggplot2
#Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
library(FactoMineR)
library(readxl)
PCA_ <- read_excel("/Users/USER/Desktop/BIG_PLANILHA_RIO_historico.xlsx", sheet = "cluster_ana_2022")
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
#fviz_pca_biplot(res_pca, geom.ind = "point", pointshape = 21, pointsize = 3, fill.ind = agua$RIO)+theme_bw()
fviz_pca_biplot(res_pca, geom.ind = "point", pointshape = 21, pointsize = 2, fill.ind = agua$RIO)+theme_bw()+labs(title = "Gráfico PCA Qualidade da Água", fill = "Local de Coleta")
fviz_pca_var(res_pca, col.var = "blue")


library(readxl)
library(vegan)
library(ggplot2)
#especies <- (read.table(file.choose(), header = TRUE))

ANOSIM = read_excel("/Users/USER/Desktop/BIG_RIOS/etc/LEONARDO.xlsx", sheet='ANOSIM')
especies = ANOSIM[,3:39]

m_com = as.matrix(especies)

ano = anosim(m_com, ANOSIM$Grupo, distance='bray', permutations = 9999)
ano
#install.packages("indicspecies")
library(indicspecies)
inv = multipatt(especies, ANOSIM$Grupo, func='r.g', control = how(nperm = 9999))
summary(inv)

#ENVIT
df = read_excel("/Users/USER/Desktop/BIG_RIOS/etc/LEONARDO.xlsx", sheet='ENVIT')
com = df[,19:55]
env = df[,3:18]

m_com = as.matrix(com)

#nmds code
set.seed(123)
nmds = metaMDS(m_com, distance = "bray")
nmds
en = envfit(nmds, env, permutations = 999, na.rm = TRUE)
en
plot(nmds)
plot(en)
#extract NMDS scores (x and y coordinates) for sites from newer versions of vegan package
data.scores = as.data.frame(scores(nmds)$sites)

#add 'season' column as before
data.scores$season = df$Grupo
en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)
en_coord_cat = as.data.frame(scores(en, "factors")) * ordiArrowMul(en)

gg = ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = data.scores, aes(colour = season), size = 3, alpha = 0.5) + 
  scale_colour_manual(values = c("orange", "steelblue","green"))  + 
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont, size =1, alpha = 0.5, colour = "grey30") +
  geom_point(data = en_coord_cat, aes(x = NMDS1, y = NMDS2), 
             shape = "diamond", size = 4, alpha = 0.6, colour = "navy") +
  geom_text(data = en_coord_cat, aes(x = NMDS1, y = NMDS2+0.04), 
            label = row.names(en_coord_cat), colour = "navy", fontface = "bold") + 
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
            fontface = "bold", label = row.names(en_coord_cont)) + 
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) + 
  labs(colour = "Group")

gg