library(ggplot2)
library(ggrepel)
library(tidyverse)
library(readxl)
library(RColorBrewer) 
library(reshape2)
# Get the positions
pizza <- read_excel("/Users/USER/Desktop/novas_analises_meta/graficos.xlsx", sheet = "pizza")

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

#GRAFICO EM PÉ (filo) - variable - local -- #falta filo+ generos fecais


filo <- read_excel("/Users/USER/Desktop/novas_analises_meta/graficos.xlsx", sheet = "fecal")

filo <- melt(filo, idvars = "gen")

colourCountu = length(unique(filo$gen))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

ggplot(filo, aes(variable, value)) +
  geom_bar(aes(fill = gen), position = "fill", stat="identity") + scale_fill_manual(values = getPalette(colourCountu)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Fecal genus")

#fecal

fecal <- read_excel("/Users/USER/Desktop/AMAZ/AMAZ.xlsx", sheet = "gen_fecal")

fecal <- melt(fecal, idvars = "Fecal")

colourCounto = length(unique(fecal$Fecal))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

ggplot(fecal, aes(variable, value)) +
  geom_bar(aes(fill = Fecal), position = "fill", stat="identity") + scale_fill_manual(values = getPalette(colourCounto)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Fecal genus")

#ARGS

arg <- read_excel("/Users/USER/Desktop/novas_analises_meta/graficos.xlsx", sheet = "arg")


arg <- melt(arg, idvars = "arg")

colourCounte = length(unique(arg$arg))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

ggplot(arg, aes(arg, value)) +
  geom_bar(aes(fill = variable), position = "fill", stat="identity") + scale_fill_manual(values = getPalette(colourCounte)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Arg", subtitle = "", x= "Arg", y="Porcentagem",
                                                                  fill = "Local") + coord_flip()

