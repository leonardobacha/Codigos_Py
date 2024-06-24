library(ggplot2)
library(reshape2)
library(readxl)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(dplyr)
library(patchwork)
library(magrittr)
library(readxl)
library(reshape2)
library(ggpubr)
#LIMPOS
limp <- read_excel("/Users/USER/Desktop/BIG_RIOS/graf_rio.xlsx", sheet = "Bom_t")
#limp <- t(limp)]
#colnames(limp) <-limp
limp <- limp[,1:12]
limp <- melt(limp, idvars = "Generos")

#colnames(limp) <- c("Local", "Percentual", "Gênero")
colourCount = length(unique(limp$variable))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

f <- ggplot(limp, aes(Generos, value)) +
  geom_bar(aes(fill = variable), position = "fill", stat="identity") + scale_fill_manual(values = getPalette(colourCount)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Predominância de Bactérias em Rios Menos Poluídos", subtitle = "Reads acima de 200",  x= "Local", y="Valores",
                                                                  fill = "Gêneros") + coord_flip()

med <- read_excel("/Users/USER/Desktop/BIG_RIOS/graf_rio.xlsx", sheet = "Medio_t")
#limp <- t(limp)]
#colnames(limp) <-limp
med <- med[,1:12]
med <- melt(med, idvars = "Generos")

#colnames(limp) <- c("Local", "Percentual", "Gênero")
colourCount = length(unique(med$variable))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

p <- ggplot(med, aes(Gêneros, value)) +
  geom_bar(aes(fill = variable), position = "fill", stat="identity") + scale_fill_manual(values = getPalette(colourCount)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Predominância de Bactérias em Rios Menos Poluídos", subtitle = "Reads acima de 200",  x= "Local", y="Valores",
                                                                  fill = "Gêneros") + coord_flip()

#SUJOS
suj <- read_excel("/Users/USER/Desktop/BIG_RIOS/graf_rio.xlsx", sheet = "Ruim_t")
#limp <- t(limp)]
#colnames(limp) <-limp
suj <- suj[,1:12]
suj <- melt(suj, idvars = "Generos")

#colnames(limp) <- c("Local", "Percentual", "Gênero")
colourCount = length(unique(suj$variable))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

v <- ggplot(suj, aes(Gêneros, value)) +
  geom_bar(aes(fill = variable), position = "fill", stat="identity") + scale_fill_manual(values = getPalette(colourCount)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Predominância de Bactérias em Rios Menos Poluídos", subtitle = "Reads acima de 200",  x= "Local", y="Valores",
                                                                  fill = "Gêneros") + coord_flip()
v
g <- ggarrange (f,p,v, 
                   ncol = 1, nrow = 3,
                   common.legend = FALSE, legend ="bottom", 
                   #labels= c("A", "B"),
                   hjust=-0.3,
                   font.label= list(size=16))
g

library(readxl)
samp <- read_excel("/Users/USER/Desktop/corr_args.xlsx")
samp <- samp[,-1]

data <- as.matrix(samp)

# Default Heatmap
heatmap(data)

library(corrplot)
res <- cor(samp) # Corr matrix
round(res, 2)
corrplot(cor(samp), method = "circle")
