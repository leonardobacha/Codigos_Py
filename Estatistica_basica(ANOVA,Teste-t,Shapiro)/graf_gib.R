

desov <- read_excel("C:/Users/Leonardo/Desktop/BIG/Resumo_desovas.xlsx", sheet = "Planilha1")

idx <- order(desov$order, decreasing = FALSE)
# criar os níveis ordenados
levels <- desov$`Data entra no MAR`[idx]
# criar um factor com níveis ordenados
desov$`Data entra no MAR` <- factor(desov$`Data entra no MAR`, levels=levels, ordered=TRUE)
desov$Temperatura <- round(desov$Temperatura,1)
#rownames(desov) <- desov$Temperatura
desov$`Water seasonality*` <- factor(desov$`Water seasonality*`)

ggplot(desov, aes(x = desov$`Data entra no MAR`)) +
  geom_bar(aes(y = desov$`Total Sementes 2º Manejo`, fill="Sea Seed Production"), stat = "identity", alpha=0.4) +
  geom_point(aes(y = desov$`CHL(entrada)`/1.3, color="Chlorophyll"), size=2, group = 1)  + 
  geom_label(aes(y =desov$`CHL(entrada)`/1.3),
             label=desov$Temperatura, 
             #fill = factor(desov$`Water seasonality*`), color = c("blue","red"), 
             #position = "identity",
             nudge_x = 1, nudge_y = 1,
             #label.size = 0.1,
             #label.padding = unit(0.55, "lines"), # Rectangle size around label
             #label.size = 0.35,
             #color = "black",
             #fill= desov$`Water seasonality*`, color=c("blue","red"),
             check_overlap = T
  ) +
  geom_line(aes(y = desov$`CHL(entrada)`/1.3, color="Chlorophyll"), size=1, group = 1) +
  
  scale_y_continuous(sec.axis = sec_axis(~.*1.3, name = "Chlorophyll")) +
  #scale_x_continuous(sec.axis = sec_axis(~./3, name = "Spats(millions)")) +
  scale_fill_manual("Legend      ", values = "#CD9600") + 
  
  scale_color_manual(NULL, values = "#0CB702") +
  labs(y = "Spats (millions)", x = "Period") + 
  #xlab(bquote(Seed (K)))+
  #ylab(bquote(Period ^superscript))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(order = 1), color = guide_legend(order = 2))
