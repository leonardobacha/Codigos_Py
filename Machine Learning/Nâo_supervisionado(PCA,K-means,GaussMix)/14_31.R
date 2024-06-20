#GRAFICO EM PÉ (filo) - variable - local -- # 14_08_2015


filo <- read_excel("/Users/USER/Desktop/arquivos_megan/dados_14_31.xlsx", sheet = "14_31_filo")

filo <- melt(filo, idvars = "Filo")

colourCountu = length(unique(filo$Filo))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

ggplot(filo, aes(variable, value)) +
  geom_bar(aes(fill = Filo), position = "fill", stat="identity") + scale_fill_manual(values = getPalette(colourCountu)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Filo")

#GRAFICO classe (DE LADO)

classe <- read_excel("/Users/USER/Desktop/arquivos_megan/dados_14_31.xlsx", sheet = "classe")


classe <- melt(classe, idvars = "Classe")

colourCounte = length(unique(classe$variable))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

ggplot(ordem, aes(Ordem, value)) +
  geom_bar(aes(fill = variable), position = "fill", stat="identity") + scale_fill_manual(values = getPalette(colourCounte)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Classe", subtitle = "", x= "Classe", y="Porcentagem",
                                                                  fill = "Local") + coord_flip()

# Get the positions
pizza <- read_excel("/Users/USER/Desktop/arquivos_megan/dados_14_31.xlsx", sheet = "pizza_dom_14")

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