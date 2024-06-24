library(readxl)
big <-  read_excel("C:/Users/Leonardo/Desktop/BIG/BIG_planilha.xlsx", sheet = "mar")
b <- big [,-2]
big <- b[,-1]
cor(big)

install.packages("outliers")
library(outliers)
rm.outlier(big, fill = FALSE, median = FALSE, opposite = FALSE)
oxi <- big$`Oxi (mg/L)`
rm.outlier(oxi, fill = FALSE, median = FALSE, opposite = FALSE)
oxi
outlier(big, opposite = FALSE, logical = FALSE)

bg <- read_excel("C:/Users/Leonardo/Desktop/BIG/SST_CHL.xlsx", sheet = "SST_CHL")
outlier(bg, opposite = FALSE, logical = FALSE)

bg <- na.omit(bg)
anova(bg$sst.j,bg$sst.gw,bg$sst.ge,bg$sst.bg)
cor(n)
#vai leo

leo <- read_excel("C:/Users/Leonardo/Desktop/Pasta1.xlsx", sheet = "Planilha4")
le <- read_excel("C:/Users/Leonardo/Desktop/Pasta1.xlsx", sheet = "Planilha5")

leo$Temperatura <- round(leo$Temperatura,1)
le$Temperatura <- round(le$Temperatura,1)

idx <- order(leo$order, decreasing = FALSE)
# criar os níveis ordenados
levels <- leo$Season[idx]
# criar um factor com níveis ordenados
leo$Season <- factor(leo$Season, levels=levels, ordered=TRUE)

id <- order(le$order, decreasing = FALSE)
# criar os níveis ordenados
level <- le$Season[id]
# criar um factor com níveis ordenados
le$Season <- factor(le$Season, levels=level, ordered=TRUE)

riu <- ggplot(leo, aes(x = leo$Season)) +
      geom_bar(aes(y = leo$`Seed Production`, fill="Sea Seed Production"), stat = "identity", alpha=0.4) +
      geom_point(aes(y = leo$Chl/1.25, color="Chlorophyll"), size=2, group = 1)  + 
      geom_label(aes(y =leo$`Seed Production`),
             label=leo$Temperatura, 
             #fill = factor(leo$`Water seasonality*`), 
             position = "identity",
             #nudge_x = 1, nudge_y = 1,
             #label.size = 0.1,
             #label.padding = unit(0.55, "lines"), # Rectangle size around label
             #label.size = 0.35,
             color = "blue",
             fill= factor(leo$`Water seasonality*`),
             check_overlap = T
  ) +
      geom_line(aes(y = leo$Chl/1.25, color="Chlorophyll"), size=1, group = 1) +
  
      scale_y_continuous(sec.axis = sec_axis(~.*1.25, name = "Chlorophyll")) +
  
      scale_fill_manual("Legend      ", values = "#00BFC4") + 
  
      scale_color_manual(NULL, values = "#0CB702") +
      labs(title = "2010-2016" , y = "Spats (millions)", x = "Season") + 
  #xlab(bquote(Seed (K)))+
  #ylab(bquote(Period ^superscript))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position = "bottom") + 
      guides(fill = guide_legend(order = 1), color = guide_legend(order = 2))

piu <- ggplot(le, aes(x = le$Season)) + 
      geom_bar(aes(y = le$`Seed Production`, fill="Spats Production"), stat = "identity", alpha=0.4) +
      geom_point(aes(y = le$Chl/4, color="Chlorophyll"), size=2, group = 1)  + 
      geom_label(aes(y =le$`Seed Production`),
             label=le$Temperatura, 
             fill = factor(le$`Water seasonality*`),# color = factor(le$`Water seasonality*`),
             position = "identity",
             #nudge_x = 1, nudge_y = 1,
             #label.size = 0.1,
             #label.padding = unit(0.55, "lines"), # Rectangle size around label
             #label.size = 0.35,
             color = "blue",
             #fill= desov$`Water seasonality*`,
             check_overlap = T #scale_fill_manual(values = c("blue","red")), scale_color_manual(values = c("blue","red"))
  )  +
      geom_line(aes(y = le$Chl/4, color="Chlorophyll"), size=1, group = 1) +
  
      scale_y_continuous(sec.axis = sec_axis(~.*4, name = "Chlorophyll")) +
  
      scale_fill_manual(values = "#00BFC4") + 
  
      scale_color_manual(NULL, values = "#0CB702") +
      labs(title = "2017-2022" , y = "Spats (thousands)", x = "Season") + 
  #xlab(bquote(Seed (K)))+
  #ylab(bquote(Period ^superscript))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position = "bottom") + 
      guides(fill = guide_legend(order = 1), color = guide_legend(order = 2))

gridExtra::grid.arrange(riu, piu, nrow=2)

riu
piu

lm100 <- lm(leo$`Seed Production` ~ leo$Chl + leo$Temp, data = leo)

summary(lm100)


ggplot(year, aes(year$Year, year$value)) +
  geom_bar(aes(fill = year$Origin), position = "dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(x = "Year", y="Spats (millions)",fill ="Type") + 
  
  scale_fill_manual(values = c("#8494FF", "#00BE67"))

