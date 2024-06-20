 #GRAFICO PIZZA (DOMINIO)
install.packages("reshape2")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("tidyverse")
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(readxl)
library(RColorBrewer)

# Get the positions
pizza <- read_excel("/Users/USER/Desktop/arquivos_megan/dados_alt_bai.xlsx", sheet = "pizza_dom")

pizza$value = round(pizza$value,4)*100

df2 <- pizza %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

colourCounta = length(unique(pizza$Group))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

ggplot(pizza, aes(x = "" , y = value, fill = fct_inorder(Group))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = getPalette(colourCounta)) +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(value, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Group")) +
  theme_void() + labs(title="Domínio", subtitle = "Porcentagem dos Domínios")

#GRÁFICO BAR PLOT (FILO) -MUDAR
library(reshape2)

filo <- read_excel("/Users/USER/Desktop/arquivos_megan/dados_alt_bai.xlsx", sheet = "filo")

colourCount = length(unique(filo$Filo))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))


filo <- melt(filo, idvars = "Filo")

ggplot(filo, aes(Filo, value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") + scale_fill_manual(values = getPalette(colourCount)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Filos" , subtitle = "Banco SEED")

#GRAFICO NORMAL - CLASSE (BARRAS)

classe <- read_excel("/Users/USER/Desktop/arquivos_megan/dados_alt_bai.xlsx", sheet = "classe")

colourCounti = length(unique(classe$Classe))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))


classe <- melt(classe, idvars = "Classe")

ggplot(classe, aes(Classe, value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") + scale_fill_manual(values = getPalette(colourCounti)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Classes" , subtitle = "Banco SEED")


#GRAFICO ORDEM (DE LADO)

ordem <- read_excel("/Users/USER/Desktop/arquivos_megan/dados_alt_bai.xlsx", sheet = "ordem")


ordem <- melt(ordem, idvars = "Ordem")

colourCounte = length(unique(ordem$variable))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

ggplot(ordem, aes(Ordem, value)) +
  geom_bar(aes(fill = variable), position = "fill", stat="identity") + scale_fill_manual(values = getPalette(colourCounte)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Ordem", subtitle = "", x= "Ordem", y="Porcentagem",
                                                                  fill = "Local") + coord_flip()
#GRAFICO EM PÉ (GENEROS)


genero <- read_excel("/Users/USER/Desktop/arquivos_megan/dados_alt_bai.xlsx", sheet = "genero")

genero <- melt(genero, idvars = "Genero")

colourCountu = length(unique(genero$variable))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

ggplot(genero, aes(Genero, value)) +
  geom_bar(aes(fill = variable), position = "fill", stat="identity") + scale_fill_manual(values = getPalette(colourCountu)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Gênero", subtitle = "Gênero")



