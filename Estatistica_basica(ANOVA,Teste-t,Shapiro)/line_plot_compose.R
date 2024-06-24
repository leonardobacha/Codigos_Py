library(ggplot2)
library(grid)
library(RColorBrewer)
library(ggpubr)

rm(list=ls()) # remove qualquer objeto criando em sess???es anteriores
setwd("C:/Users/olive/OneDrive/LAB_THOMPSON/BIG")
desov <- resumo_desov

h <- ggplot(desov, aes(desov$Data, desov$log)) +
  geom_bar(aes(fill = "Sea Seed Production"), position = "dodge", stat="identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=13),
        axis.title.y = element_text (size= 17),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 15)) +
  theme(legend.title= element_blank(),
        legend.text= element_text(size = 13)) +
  labs(x = "", y="Spats (log)",fill ="") +
    scale_fill_manual(values = c("#8494FF"))
h

j <- ggplot(desov, aes(x = desov$Data)) +
  geom_point(aes(y = desov$Temperatura, color="Temperature"), size=2, group = 1)  + 
  geom_line(aes(y = desov$Temperatura, color="Temperature"), linewidth=1, group = 1) +
  geom_point(aes(y = desov$`CHL`*4, color="Chlorophyll"), size=2, group = 1)  + 
  geom_line(aes(y = desov$`CHL`*4, color="Chlorophyll"), linewidth=1, group = 1) +
  scale_y_continuous(sec.axis = sec_axis(~./4, name = "Chlorophyll (mg/m³)")) +
  scale_fill_manual("", values = "#8494FF","#0CB702") + 
  scale_color_manual(values = c("#0CB702", "#c77CFF", "#8494FF", "#E68613")) +
  labs(y = "Tempetarure (°C)", x = "") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=13),
        axis.title.y = element_text (size= 17, vjust=2), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 15)) +
  theme(legend.title= element_blank(),
        legend.text= element_text(size = 13)) +
  theme(legend.position = "bottom")
j



comb <- ggarrange (h, j, 
                    ncol = 1, nrow = 2,
                    common.legend = FALSE, legend ="bottom", 
                    labels= c("A", "B"),
                    hjust=-0.3,
                    font.label= list(size=16))
comb
dev.off()
tiff("Plot_fig3.tiff",  height = 12, width = 16, units = 'in', compression = 'lzw', res=300)

