library(readxl)
library(reshape2)
library(ggplot2)


bg <- read_excel("C:/Users/USER/Desktop/BIG/BIG_2023/2003_2022_sst.xlsx")
temp <- read_excel("C:/Users/USER/Desktop/BIG/TEMP_GLOBAL.xlsx")


# tente depois (ESSA A BOA) - separado _ como pedido em meio_23

ggplot(bg, aes(x = bg$Date)) +
  #geom_point(aes(y = bg$J, color="J"), size=2, group = 1) +
  geom_line(aes(y = bg$J, color="J"), size=1, group = 1) +
  #geom_point(aes(y = bg$GW, color="GW"), size=2, group = 1) +
  geom_line(aes(y = bg$GW, color="GW"), size=1, group = 1) +
  #geom_point(aes(y = bg$GE, color="GE"), size=2, group = 1) +
  geom_line(aes(y = bg$GE, color="GE"), size=1, group = 1) +
  #geom_point(aes(y = bg$BG, color="BG"), size=2, group = 1) +
  geom_line(aes(y = bg$BG, color="BG"), size=1, group = 1) +
  
  labs(x = "Date", y = "Local Temperature (ºC)", color = "") +
  scale_color_manual(values = c("#0CB702", "#c77CFF", "#8494FF", "#E68613")) +
  theme_bw() +
  
te <- ggplot(temp, aes(x = temp$Date)) +
  geom_line(aes(y = temp$Lowess, color ="LO"), size=1, group = 1) +
  geom_line(aes(y = temp$No_Smoothing, color ="NO"), size=1, group = 1) +
  #scale_x_continuous(bg$Date) +
  #scale_y_continuous(sec.axis = sec_axis(~./15, name = "Global Temperature (°C)")) +
  
  #scale_x_continuous(temp$Year) +
  
  labs(x = "", y = "Global Temperature Anomaly (ºC)", color = "") +
  scale_color_manual(values = c("#ABA300", "#00A9FF", "#0CB702", "#c77CFF", "#8494FF", "#E68613")) +
  theme_bw() 

te
dev.off()
tiff("Supp_Fig_2.tiff",  height = 12, width = 16, units = 'in', compression = 'lzw', res=300)


# analise Vibrios

novo <-  read_excel("C:/Users/USER/Desktop/BIG/BIG_planilha.xlsx", sheet = "dados")
lm1 <- lm(novo$Vibrio ~ novo$Temperature, data = novo)

summary(lm1)

# analise Larvas/Sobrevivencia (barras?)

sobr <- read_excel("C:/Users/USER/Desktop/BIG/TEMP_GLOBAL.xlsx", sheet = "graf")

install.packages("trend")
install.packages("Kendall")
library(trend)
library(Kendall)

mk.test(sobr$`Sobrevivência Véliger/Pediveliger`)
mk.test(sobr$`Sobrevivência Pediveliger/Sementes`)
mk.test(sobr$`Sobrevivência Manejo/Mar`)

plot(sobr$`Sobrevivência Véliger/Pediveliger`, type = "l")



so <- ggplot(sobr, aes(x = sobr$Data)) +
  #geom_point(aes(y = bg$J, color="J"), size=2, group = 1) +
  geom_line(aes(y = sobr$`Sobrevivência Véliger/Pediveliger`, color="Pediveliger/Velinger"), size=1, group = 1) +
  #geom_point(aes(y = bg$GW, color="GW"), size=2, group = 1) +
  geom_line(aes(y = sobr$`Sobrevivência Pediveliger/Sementes`, color="Seed/Pedivelinger"), size=1, group = 1) +
  #geom_point(aes(y = bg$GE, color="GE"), size=2, group = 1) +
  #geom_line(aes(y = sobr$`Sobrevivência Manejo/Mar`, color="Sea Handling"), size=1, group = 1) +

  labs(title = "Production Survival Phases",x = 'Data', y = "%", color = "") +
  scale_color_manual(values = c("#8494FF", "#E68613")) +
  theme_bw() + theme(legend.position = "bottom")

so
dev.off()
tiff("Supp_Fig_1.tiff",  height = 12, width = 16, units = 'in', compression = 'lzw', res=300)


# analise Spats+Scallop
year <- read_excel("C:/Users/USER/Desktop/BIG/Resumo_desovas.xlsx", sheet = "lab_sea")
year_2 <- read_excel("C:/Users/USER/Desktop/BIG/Resumo_desovas.xlsx", sheet = "lab_sea(2)")

year_2$Year <- as.numeric(signif(year_2$Year, digits = 4))
year_2$Year <- format(year_2$Year, format="%Y")

#y <- melt(year, idvars = year$Year)

w <- ggplot(year, aes(year$Year, year$value)) +
  geom_bar(aes(fill = year$Origin), position = "dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(
                                                                  x = "", y="Spats (millions)",fill ="") + 
  #scale_y_continuous(sec.axis = sec_axis(~ . / 10, name = "Scallops"))+
  scale_fill_manual(values = c("#8494FF", "#00BE67","#E68613" )) + theme_classic() + theme(legend.position = "bottom")

w 
dev.off()
tiff("Fig_2A.tiff",  height = 12, width = 16, units = 'in', compression = 'lzw', res=300)

hj <- ggplot(year_2, aes(year_2$Year, year_2$value)) +
  geom_bar(aes(fill = year_2$Origin), position = "dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(
    x = "", y="Scallops dozens (thousands)",fill ="") + 
  #scale_y_continuous(sec.axis = sec_axis(~ . / 10, name = "Scallops"))+
  scale_fill_manual(values = c("#E68613" )) + theme_classic() + theme(legend.position = "bottom")

hj
dev.off()
tiff("Fig_2B.tiff",  height = 12, width = 16, units = 'in', compression = 'lzw', res=300)

gridExtra::grid.arrange(w, hj, nrow=2)


regr <- read_excel("C:/Users/USER/Desktop/BIG/BIG_2023/BIG_artigo/tabelas.xlsx", sheet = "Tab_spats")
lm2 <- lm(regr$`Spats (millions)` ~ regr$Temperature, data = regr)
summary(lm2)

ggplot(regr, aes(x = regr$CHL)) +
  geom_line(aes(y = regr$`Spats (millions)` , color="#ABA300", size=0.5, group = 1)) +
  labs(title = "Regressão", x = "CHL", y = "Spats", color = "") +
  scale_color_manual(values = c("#ABA300", "#00A9FF")) +
  theme_bw() 

p <- ggplot(regr, aes(x=regr$CHL, y=regr$`Spats (millions)`)) + # Informa os dados a serem utilizadps
  geom_point() # Informa que eu quero um gráfico de dispersão.
#p
p1 <- p + geom_smooth(method=lm) # Acrescenta a linha de tendência e o intervalo de confiança de predição
#p1

p2 <- p + geom_smooth(method=lm, se=FALSE) +
  labs(x = "Chlorofill (mg/m³)", y = "Spats (millions)")# Acrescenta a linha de tendência, sem o intervalo de predição
p2

#Chl
chl <- read_excel("C:/Users/USER/Desktop/BIG/BIG_2023/2033_2022_chl.xlsx")

ggplot(chl, aes(x = chl$Date)) +
  #geom_point(aes(y = bg$J, color="J"), size=2, group = 1) +
  geom_line(aes(y = chl$J, color="J"), size=1, group = 2) +
  #geom_point(aes(y = bg$GW, color="GW"), size=2, group = 1) +
  geom_line(aes(y = chl$GW, color="GW"), size=1, group = 2) +
  #geom_point(aes(y = bg$GE, color="GE"), size=2, group = 1) +
  geom_line(aes(y = chl$GE, color="GE"), size=1, group = 2) +
  #geom_point(aes(y = bg$BG, color="BG"), size=2, group = 1) +
  geom_line(aes(y = chl$BG, color="BG"), size=1, group = 2) +
  
  labs(x = "Date", y = "Chlorofill (mg/m³)", color = "") +
  scale_color_manual(values = c("#0CB702", "#c77CFF", "#8494FF", "#E68613")) +
  theme_bw()


desov <- read_excel("C:/Users/USER/Desktop/BIG/Resumo_desovas.xlsx", sheet = "Planilha1")

idx <- order(desov$order, decreasing = FALSE)
# criar os níveis ordenados
levels <- desov$Data[idx]
# criar um factor com níveis ordenados
desov$`Data` <- factor(desov$`Data`, levels=levels, ordered=TRUE)
desov$Temperatura <- round(desov$Temperatura,0)
#rownames(desov) <- desov$Temperatura
desov$`Water seasonality*` <- factor(desov$`Water seasonality*`)


ggplot(desov, aes(x = desov$`Data`)) +
  geom_bar(aes(y = desov$`log`, fill="Sea Seed Production"), stat = "identity", alpha=0.4) +
  geom_point(aes(y = desov$`CHL`, color="Chlorophyll"), size=2, group = 1)  + 
  geom_label(aes(y =desov$`CHL`),
             label=desov$Temperatura, 
             fill = factor(desov$`Water seasonality*`), 
             position = "identity",
             #nudge_x = 1, nudge_y = 1,
             #label.size = 0.1,
             #label.padding = unit(0.55, "lines"), # Rectangle size around label
             #label.size = 0.35,
             color = "blue",
             #fill= desov$`Water seasonality*`, color=c("blue","red"),
             check_overlap = T
  ) +
  geom_line(aes(y = desov$`CHL`, color="Chlorophyll"), size=1, group = 1) +
  
  scale_y_continuous(sec.axis = sec_axis(~., name = "Chlorophyll (mg/m³)")) +
  #scale_x_continuous(sec.axis = sec_axis(~./3, name = "Spats(millions)")) +
  scale_fill_manual("", values = "#8494FF") + 
  
  scale_color_manual(NULL, values = "#0CB702") +
  labs(y = "Spats (Log)", x = "") + 
  #xlab(bquote(Seed (K)))+
  #ylab(bquote(Period ^superscript))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(order = 1), color = guide_legend(order = 2))


#PCA

library(readxl)
novo <-  read_excel("C:/Users/USER/Desktop/BIG/BIG_planilha.xlsx", sheet = "dados")

n <- novo[,-1]



#PCA mais avançado

library(factoextra)
#Carregando pacotes exigidos: ggplot2
#Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
library(FactoMineR)
library(readxl)

res_pca <- PCA(n, quali.sup = 1)
#Warning message:
#In PCA(agua, quali.sup = 1) :
#Missing values are imputed by the mean of the variable: you should use the imputePCA function of the missMDA package
summary(res_pca)



sweep(res_pca$var$coord,2,sqrt(res_pca$eig[1:ncol(res_pca$var$coord),1]), FUN="/")

fviz_contrib(res_pca, choice = "var", axes = c(1,2))
fviz_eig(res_pca, addlabels = T, ylim = c(0,90))
fviz_pca_biplot(res_pca, geom.ind = "point", pointshape = 21, pointsize = 3, fill.ind = novo$Date)+theme_bw()
b <- fviz_pca_biplot(res_pca, geom.ind = "point", pointshape = 21, pointsize = 2, fill.ind = novo$Date)+theme_bw()+labs(title = "", fill = "")
fviz_pca_var(res_pca, col.var = "blue")

b
dev.off()
tiff("Fig_aggfh.tiff",  height = 12, width = 16, units = 'in', compression = 'lzw', res=300)


library(corrplot)


72.274





v <- n
v['Autotrofic Counts'] <- c(72274,72274,36928,15347,17799,22764,27729)
v = cor(v)
corrplot(v, method = 'number')

quente <- c(65183773,68309830, 50332147, 56567790, 25981163,  23893897,
       24558353, 27609770, 20755017, 70560940, 18906073, 17005783,
       14352033, 18896243, 13451847)

frio <- c(22365790, 8186003, 2436823, 6836787, 1071333, 
          28793140, 29378973, 38100643, 20700907,26942000)

t.test(quente,frio)

#Regressão Múltipla

lmr <- lm(n$Spats ~ n$ph + n$DO + n$Turbidy + n$DOC + n$TN + n$`Escherichia Coli` + n$Vibrio + n$`Temp. (°C)` +
            n$P + n$Chlorophyll + n$`Microbial Counts`,  data = n)
summary(lmr)

lmre <- lm(n$Spats ~ n$`Escherichia Coli` + n$Vibrio + n$`Temp. (°C)` + n$Chlorophyll + n$`Microbial Counts`,  data = n)

summary(lmre) #Funcionou

lmrz <- lm(n$Spats ~ n$`Escherichia Coli`, data = n)
summary(lmrz)

lmrv <- lm(n$Spats ~ n$Vibrio, data = n)
summary(lmrv)


lmrs <- lm(n$Spats ~ n$Chlorophyll, data = n)
summary(lmrs)


lmry <- lm(n$Spats ~ n$`Temp. (°C)`, data = n)
summary(lmry)

lmrk <- lm(n$Spats ~ n$`Microbial Counts`, data = n)
summary(lmrk)

lmrf <- lm(n$Spats ~ n$`Microbial Counts` + n$`Temp. (°C)`, data = n)
summary(lmrf)

lmri <- lm(n$Spats ~ n$ph + n$DO + n$Turbidy + n$DOC + n$TN  + n$P,  data = n)

summary(lmri)


#Nova RLM (todo período)
tina <-  read_excel("C:/Users/USER/Desktop/BIG/BIG_2023/BIG_artigo/tabelas.xlsx", sheet = "RLM")

lmrt <- lm(tina$`Spats (millions)` ~ tina$CHL + tina$Temperature, data = n)
summary(lmrt)

#Matriz de similaridades (corrleção)+ Teste Mandell

install.packages('permute')
install.packages('vegan')
library(permute)
library(lattice)
library(vegan)
library(corrplot)

g <- cor(n)
corrplot(g)


especies <- n[,8:12]
ambiente <- n[,1:7]
ambiente.pad <- decostand(ambiente, method = "standardize")
#binary = TRUE, considerar dados presença e ausência
dist.jac <- vegdist(especies, method = "jaccard", binary = TRUE)
dist.amb.pad <- vegdist(ambiente.pad, method = "euclid")
mantel(dist.amb.pad, dist.jac, permutations = 10000)

#REVISÃO EM JUNHO DE 2023 (FIGURAS)
desov <- read_excel("C:/Users/USER/Desktop/BIG/Resumo_desovas.xlsx", sheet = "Planilha1")

idx <- order(desov$order, decreasing = FALSE)
# criar os níveis ordenados
levels <- desov$Data[idx]
# criar um factor com níveis ordenados
desov$`Data` <- factor(desov$`Data`, levels=levels, ordered=TRUE)
desov$Temperatura <- round(desov$Temperatura,0)
#rownames(desov) <- desov$Temperatura
desov$`Water seasonality*` <- factor(desov$`Water seasonality*`)


j <- ggplot(desov, aes(x = desov$Data)) +
  geom_point(aes(y = desov$Temperatura, color="Temperature"), size=2, group = 1)  + 
  
  geom_line(aes(y = desov$Temperatura, color="Temperature"), size=1, group = 1) +
  
  geom_point(aes(y = desov$`CHL`*4, color="Chlorophyll"), size=2, group = 1)  + 
  
  geom_line(aes(y = desov$`CHL`*4, color="Chlorophyll"), size=1, group = 1) +
  
  scale_y_continuous(sec.axis = sec_axis(~./4, name = "Chlorophyll (mg/m³)")) +
  #scale_x_continuous(sec.axis = sec_axis(~./3, name = "Spats(millions)")) +
  scale_fill_manual("", values = "#8494FF","#0CB702") + 
  scale_color_manual(values = c("#0CB702", "#c77CFF", "#8494FF", "#E68613")) +
 # scale_color_manual(NULL, values = "#0CB702","#8494FF") +
  labs(y = "Tempetarure (°C)", x = "", title = "3B") + 
  #xlab(bquote(Seed (K)))+
  #ylab(bquote(Period ^superscript))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(title = "dsf", title.position = "left"))
         #, order = 1), color = guide_legend(order = 2)) 

#ainda tem a fig de sobr mais acima

h <- ggplot(desov, aes(desov$Data, desov$log)) +
  geom_bar(aes(fill = "Sea Seed Production"), position = "dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(
    x = "", y="Spats (log)",fill ="", title = "3A") + 
  #scale_y_continuous(sec.axis = sec_axis(~ . / 10, name = "Scallops"))+
  scale_fill_manual(values = c("#8494FF"))

gf <- h + theme(legend.position = "bottom")
gridExtra::grid.arrange(gf, j, nrow=2)

#fig_sup
ggplot(chl, aes(x = chl$Date)) +
  #geom_point(aes(y = bg$J, color="J"), size=2, group = 1) +
  geom_line(aes(y = chl$J, color="J"), size=1, group = 2) +
  #geom_point(aes(y = bg$GW, color="GW"), size=2, group = 1) +
  geom_line(aes(y = chl$GW, color="GW"), size=1, group = 2) +
  #geom_point(aes(y = bg$GE, color="GE"), size=2, group = 1) +
  geom_line(aes(y = chl$GE, color="GE"), size=1, group = 2) +
  #geom_point(aes(y = bg$BG, color="BG"), size=2, group = 1) +
  geom_line(aes(y = chl$BG, color="BG"), size=1, group = 2) +
  
  labs(x = "Date", y = "Chlorofill (mg/m³)", color = "") +
  scale_color_manual(values = c("#0CB702", "#c77CFF", "#8494FF", "#E68613")) +
  theme_bw()

#Gráfico em 20_07_2023
year_3 <- read_excel("C:/Users/USER/Desktop/BIG/Resumo_desovas.xlsx", sheet = "lab_sea(3)")

year_3$Year <- as.numeric(signif(year_3$Year, digits = 4))
year_3$Year <- format(year_3$Year, format="%Y")

ggplot(year_3, aes(x = year_3$Year)) +
  geom_point(aes(y = year_3$Value, color=""), size=1, group = 2) +
  geom_line(aes(y = year_3$Value, color=""), size=1, group = 2) +
  
  
  labs(x = "", y = "%", color = "Survival") +
  scale_color_manual(values = c("#0CB702", "#c77CFF", "#8494FF", "#E68613")) +
  theme_bw() + theme(legend.position = "bottom")