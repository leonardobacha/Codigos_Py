library(ggplot2)
library(reshape2)
library(readxl)
library(RColorBrewer)

#LIMPOS
limp <- read_excel("/Users/leonardobacha/Desktop/megan/leozin.xlsx", sheet = "graf_limp")

limp <- melt(limp, idvars = "Data")

#colnames(limp) <- c("Local", "Percentual", "Gênero")
colourCount = length(unique(limp$variable))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

ggplot(limp, aes(Data, value)) +
  geom_bar(aes(fill = variable), position = "fill", stat="identity") + scale_fill_manual(values = getPalette(colourCount)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Predominância de Bactérias em Rios Menos Poluídos", subtitle = "Reads acima de 200",  x= "Local", y="Valores",
                                                                  fill = "Gêneros") + coord_flip()

#SUJOS
suj <- read_excel("/Users/leonardobacha/Desktop/megan/leozin.xlsx", sheet = "graf_suj")

suj <- melt(suj, idvars = "Data")

colourCount = length(unique(suj$variable))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

ggplot(suj, aes(Data, value)) +
  geom_bar(aes(fill = variable), position = "fill", stat="identity") + scale_fill_manual(values = getPalette(colourCount)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Pred. Gênero em Rios mais poluídos", subtitle = "Reads acima de 500", x= "Local", y="Valores",
                                                                  fill = "Gêneros") + coord_flip()


#FECAL

fec <- read_excel("/Users/leonardobacha/Desktop/megan/leozin.xlsx", sheet = "Fecal")

fec <- melt(fec, idvars = "Data")

colourCount = length(unique(fec$variable))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

ggplot(fec, aes(Data, value)) +
  geom_bar(aes(fill = variable), position = "fill", stat="identity") + scale_fill_manual(values = getPalette(colourCount)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Distribuição de bactérias fecais", subtitle = "Locais")

fezes <- read_excel("/Users/leonardobacha/Desktop/megan/leozin.xlsx", sheet = "Fecal_graf")

fezes <- melt(fezes, idvars = "Data")

colourCount = length(unique(fezes$variable))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

ggplot(fezes, aes(Data, value)) +
  geom_bar(aes(fill = variable), position = "fill", stat="identity") + scale_fill_manual(values = getPalette(colourCount)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Pred. de bactérias fecais", subtitle = "Gênero")


#BARPLOT colocar médias e dsvpad dos Pontos
df<-data.frame(Mean=c(1306,84761,12963,3333,431,108676,1166,17290,10575,33273),
               sd=c(1146,21108,2204,1304,128,37768,647,10977,10414,3152),
               Quality=as.factor(c("Menos Poluído","Mais Poluído","Mais Poluído",
                                   "Menos Poluído","Menos Poluído","Mais Poluído","Menos Poluído",
                                   "Mais Poluído","Mais Poluído","Mais Poluído")), 
               Local=c("BRA","CEN","CJA","FOZ","FRA","JAC","MAM", "PAR","TAQ","JAP"),
               Insert= c(0.0, 0.1, 0.2, 0.3, 0.4,0.5,0.6,0.75,0.9,1.0))

ggplot(df, aes(x=Local, y=Mean, fill=Quality)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour='black') +
  geom_errorbar(aes(ymin=Mean-sd, ymax=Mean+sd), width=.2) + 
  labs(title = "BarPlot (Média + SD)", subtitle = "Reads de bactérias fecais")

# Gráfico Pizza (Bacterias Totais + Fecais/Redutoras de Sulfato)
pizza <- read_excel("/Users/leonardobacha/Desktop/megan/leozin.xlsx", sheet = "pizza")

#ATENÇÃO ESTE ESTÁ SUPERADO, VER O MAIS ABAIXO!
graf.bac <- ggplot(pizza, aes(x ="", y=Total, fill=Tipo)) + geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0, direction = -1) + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_blank(),
    legend.title = element_blank()) + 
  geom_text(data = pizza, aes(x ="", y=Total, label = PERCENTUAL),
  position = position_stack(vjust = 0.5)) +

  labs(title = "Gráfico de Pizza",
       subtitle = "Distribuição de bactérias fecais (%)",
       fill = "Gêneros bactérias")
graf.bac

install.packages("ggplot2")
install.packages("ggrepel")
install.packages("tidyverse")
library(ggplot2)
library(ggrepel)
library(tidyverse)

# Get the positions
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
  theme_void() + labs(title="Pie Chart of Fecal Contaminants", subtitle = "Percetange of Genus")


