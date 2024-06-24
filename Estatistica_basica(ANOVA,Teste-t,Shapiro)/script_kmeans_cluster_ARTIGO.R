#Video que mostra cluster por k-means e dendograma
#Chamar pacotes
library(FactoMineR)
library(factoextra)
library(cluster)
library(xlsx)
#tem um comando interessante chamado file.choose que dá o caminho do arquivo

#Importar do excel
dados_brutos <- read.xlsx("/Users/leonardobacha/Desktop/Current_Microbial/NOVA\ PLANILHA.xlsx",sheetIndex = 1,header = T )
f <- dados_brutos[,-1]
d <- dados_brutos
#Tirar a coluna cliente e colocar ela como rotulo de linha
row.names(d) <- d$Cities


#retirar os omit


#comparar quali com quali : não se esqueça de trocar o valor de x para funções abaixo
#d <-d[,c(7,8)]    - se quiser comparar apenas o último ano
#f <- f[,c(7,8)]    - se eu quiser comparar apenas o ultimo ano


#agrupar em cluster no dendograma
cluster <- hclust(dist(d))
plot(cluster)

tiff(filename = 'cl', width = 8, height = 6, res = 1000, units = 'in')
plot(cluster)

dev.off()

rect.hclust(cluster, k=2, border = 2.5)

#numero otimo de clusters
fviz_nbclust(f, kmeans, method = "gap_stat")
fviz_nbclust(f, kmeans, method = "silhouette")
fviz_nbclust(f, kmeans, method = "wss")

#gerar kmeans
dados_kmeans <- kmeans(f, 3)
#visualizar no gráfico
fviz_cluster(dados_kmeans, data = f, ellipse.type = "euclid", palette ="Set2", ggtheme = theme_minimal())

tiff(filename = 'test', width = 8, height = 6, res = 1000, units = 'in')

fviz_cluster(dados_kmeans, data = f, ellipse.type = "euclid", palette ="Set2", ggtheme = theme_minimal())

dev.off()
#se eu quiser adicionar o cluster aos dados
lista <- dados_kmeans$cluster
dados_gerais <- cbind(dados, lista)
