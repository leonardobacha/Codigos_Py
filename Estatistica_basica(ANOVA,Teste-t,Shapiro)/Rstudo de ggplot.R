library(ggplot2)
library(tidyr)
library(tidyverse)
library(readxl)


rio <- read_excel("~/Desktop/BIG_Planilha_completa.xlsx",
                                    sheet = "Rio")

mar <- read_excel("~/Desktop/BIG_Planilha_completa.xlsx",
                  sheet = "Mar")

tudo <- read_excel("~/Desktop/BIG_Planilha_completa.xlsx",
                   sheet = "TUDO")

sst <- read_excel("~/Desktop/SST_CHL.xlsx",
                  sheet = "SST")


#primeiro importe a tabela do Excel


attach(rio)
attach(mar)

# estudo sobre ggplot - gráfico de linhas e de barras

#arrumando o banco para long
library(reshape2)
lmar <- melt(mar, id.vars = c("Localidade", "Data da Coleta"))
lrio <- melt(rio, id.vars = c("Localidade", "Data da Coleta"))
ltudo <-melt(tudo, id.vars = c("Localidade", "Data da Coleta"))
lsst <-melt(sst, id.vars = c("Date(yyyy-MM)"))
#melhor, já seleciona nele mesmo o Parâmetro desejado:

lmar %>%
  filter(variable == "Escherichia Coli (NMP/100ml)")%>%
  ggplot(., aes(x = `Data da Coleta`, y = value)) + geom_point(aes(colour = Localidade)) + geom_line(aes(colour = Localidade)) + labs(x = "Data", y = "(NMP/100ml)", fill = "Ponto Analisado", title = "Escherichia Coli ")

lrio %>%
  filter(variable == "Escherichia Coli (NMP/100ml)")%>%
  ggplot(., aes(x = `Data da Coleta`, y = value)) + geom_point(aes(colour = Localidade)) + geom_line(aes(colour = Localidade)) + labs(x = "Data", y = "(NMP/100ml)", fill = "Ponto Analisado", title = "Escherichia Coli ")

lsst %>%
  ggplot(., aes(as.Date(`Date(yyyy-MM)`), value)) + geom_point(aes(colour = variable)) + geom_line(aes(colour = variable)) + labs(x = "Data", y = "SST(ºC)", fill = "Rio Analisado", title = "Temperatura") +
  geom_hline(yintercept = 28) + geom_vline(xintercept = as.numeric(as.Date("2017-09-01")), 
                                           color = "red", 
                                           lwd = 0.5) + geom_vline(xintercept = as.numeric(as.Date("2018-09-01")), 
                                                                   color = "red", 
                                                                   lwd = 0.5) + geom_vline(xintercept = as.numeric(as.Date("2019-09-01")), 
                                                                                         color = "red", 
                                                                                         lwd = 0.5) + geom_vline(xintercept = as.numeric(as.Date("2020-09-01")), 
                                                                                                               color = "red", 
                                                                                                               lwd = 0.5) + geom_vline(xintercept = as.numeric(as.Date("2021-09-01")), 
                                                                                                                                     color = "red", 
                                                                                                                                     lwd = 0.5)

#tem fazer um para o rio e outro para o mar

#agora boxplot


ltudo %>%
  filter(variable == "Coliformes Total (NMP/100ml)")%>%
  ggplot(., aes(x = Localidade, y = value)) + geom_boxplot() + geom_tile() +
  theme(axis.text.x = element_text(angle = 90))


#PCA - está legal : pode fazer com os reads do metagenoma

install.packages("plotly")
install.packages("ggfortify")

library(plotly)
library(ggfortify)

pca <- read_excel("/Users/leonardobacha/Desktop/megan/leozin.xlsx", sheet = "PCA")

rma <- pca[,2:5]
 
pca_res <- prcomp(rma, scale. = TRUE)

p <- autoplot(pca_res, data = pca, colour = 'Data',
              loadings = TRUE, loadings.colour = 'blue',
              loadings.label = TRUE, loadings.label.size = 3)
ggplotly(p)

# NOVO CCA
install.packages("ca")
library(ca)
cc <- read_excel("C:/Users/Leonardo/Desktop/BIG/BIG_Planilha_completa.xlsx",
                 sheet = "CCA")
g <- cc[,1]
rownames(cc) <- g$Código
cc <- cc[,-1] 



ca.fit <- ca(cc)
ca.plot <- plot(ca.fit)
str(ca.plot)
make.ca.plot.df <- function (ca.plot.obj,
                             row.lab = "Rows",
                             col.lab = "Columns") {
  df <- data.frame(Label = c(rownames(ca.plot.obj$rows),
                             rownames(ca.plot.obj$cols)),
                   Dim1 = c(ca.plot.obj$rows[,1], ca.plot.obj$cols[,1]),
                   Dim2 = c(ca.plot.obj$rows[,2], ca.plot.obj$cols[,2]),
                   Variable = c(rep(row.lab, nrow(ca.plot.obj$rows)),
                                rep(col.lab, nrow(ca.plot.obj$cols))))
  rownames(df) <- 1:nrow(df)
  df
}
ca.plot.df <- make.ca.plot.df(ca.plot,
                              row.lab = "Sample_Point",
                              col.lab = "Parameter")
ca.plot.df$Size <- ifelse(ca.plot.df$Variable == "Sample_Point", 2, 1)
ca.plot.df
ca.sum <- summary(ca.fit)
dim.var.percs <- ca.sum$scree[,"values2"]
dim.var.percs
library(ggplot2)
library(ggrepel)

p <- ggplot(ca.plot.df, aes(x = Dim1, y = Dim2,
                            col = Variable, shape = Variable,
                            label = Label, size = Size)) +
  geom_vline(xintercept = 0, lty = "dashed", alpha = .5) +
  geom_hline(yintercept = 0, lty = "dashed", alpha = .5) +
  geom_point()

p <- p +
  scale_x_continuous(limits = range(ca.plot.df$Dim1) + c(diff(range(ca.plot.df$Dim1)) * -0.2,
                                                         diff(range(ca.plot.df$Dim1)) * 0.2)) +
  scale_y_continuous(limits = range(ca.plot.df$Dim2) + c(diff(range(ca.plot.df$Dim2)) * -0.2,
                                                         diff(range(ca.plot.df$Dim2)) * 0.2)) +
  scale_size(range = c(4, 7), guide = F) +
  geom_label_repel(show.legend = F, segment.alpha = .5, point.padding = unit(5, "points")) +
  guides(colour = guide_legend(override.aes = list(size = 4)))

p <- p +
  labs(x = paste0("Dimension 1 (", signif(dim.var.percs[1], 3), "%)"),
       y = paste0("Dimension 2 (", signif(dim.var.percs[2], 3), "%)"),
       col = "", shape = "") +
  theme_minimal()
plot(p)