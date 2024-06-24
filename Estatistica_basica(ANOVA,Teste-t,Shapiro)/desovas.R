library(readxl)
novo <-  read_excel("C:/Users/Leonardo/Desktop/BIG/BIG_planilha.xlsx", sheet = "dados")

n <- novo[,-1]

#Realizando Reg_Multipla (não deu certo), dps PCA

lm <- lm(n$`Produção (Desovas)` ~ n$Temperatura + n$`Escherichia Coli (NMP/100ml)` + n$`Vibrio (CFU/ml)`
         + n$`NT (mg/L)` +  n$`COD (mg/L)` + n$`Oxi (mg/L)` + n$Turbidez + n$ph, data = n)

summary(lm)  

confint(lm)

plot(lm, which = 1)
plot(lm, which = 2)

shapiro.test(lm$residuals)
plot(lm, which = 5)
summary(n)


#realizando os métodos de exclusão de variáveis
step(lm, direction = "both")

step(lm, direction = "backward")

step(lm, direction = "forward")


#outra regressão 
#lm2 <- lm(df$IBP ~ df$IDEB + df$Royal, data = df)
summary(lm2)
#checando diferença dos modelos
anova(lm2,lm1)

#PCA simples

library("stats")

cor(n, use="complete")
pairs(n, upper.panel = NULL)
PC <- princomp(n, cor = TRUE, scores= TRUE, fix_sign= TRUE)
attributes(PC)
summary(PC)
print(PC$loadings)
boxplot(PC$scores)
screeplot(PC)
biplot(PC)

n <- n[,-1]
pca <- prcomp( x = n, center = TRUE, scale. = TRUE)
attributes(pca)
summary(pca)
print(pca$loadings)
boxplot(pca$scores)
screeplot(pca)
biplot(pca)

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
fviz_pca_biplot(res_pca, geom.ind = "point", pointshape = 21, pointsize = 2, fill.ind = novo$Date)+theme_bw()+labs(title = "PCA Plot: Seed Production x Water Parameters", fill = "Date")
fviz_pca_var(res_pca, col.var = "blue")

 ##### Estudo com Linhas de tendencia simples de duas variaveis
library(readxl)
s <-  read_excel("C:/Users/Leonardo/Desktop/SST_CHL.xlsx", sheet = "dados")
idx <- order(desov$order, decreasing = FALSE)
# criar os níveis ordenados
levels <- s$Period[idx]
# criar um factor com níveis ordenados
s$Period <- factor(s$Period, levels=levels, ordered=TRUE)
library(ggplot2)

ggplot(s, aes(x = s$Period)) +
  geom_point(aes(y = s$`Temperature(Mean)`, color="Temperature"), size=2, group = 1) +
  geom_line(aes(y = s$`Temperature(Mean)`, color="Temperature"), size=1, group = 1) +
  scale_color_manual(NULL, values = c("#0CB702", "#c77CFF")) +
  geom_errorbar(aes(ymin=s$`Temperature(Mean)`-s$se_temp, ymax=s$`Temperature(Mean)`+s$se_temp), width=.1) +
  labs(title = "Temperature (ºC)", y = "Temperature (ºC)", x = "Period") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 28.5, color="red") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(order = 1), color = guide_legend(order = 2))



#chl(entrada)+ seed(mes que sai do MAr) c/ % de sobrevivencia ************************************
library(readxl)
library(reshape2)
library(ggplot2)

desov <- read_excel("C:/Users/Leonardo/Desktop/BIG/Resumo_desovas.xlsx")
year <- read_excel("C:/Users/Leonardo/Desktop/BIG/Resumo_desovas.xlsx", sheet = "lab_sea")

idx <- order(desov$order, decreasing = FALSE)
# criar os níveis ordenados
levels <- desov$`Data entra no MAR`[idx]
# criar um factor com níveis ordenados
desov$`Data entra no MAR` <- factor(desov$`Data entra no MAR`, levels=levels, ordered=TRUE)
desov$Temperatura <- round(desov$Temperatura,1)
#rownames(desov) <- desov$Temperatura
desov$`Water seasonality*` <- factor(desov$`Water seasonality*`, levels=levels, ordered=TRUE)

ggplot(desov, aes(x = desov$`Data entra no MAR`)) +
  geom_bar(aes(y = desov$`Total Sementes 2º Manejo`, fill="Sea Seed Production"), stat = "identity", alpha=0.4) +
  geom_point(aes(y = desov$`CHL(entrada)`*625, color="Chlorofill"), size=2, group = 1)  + 
  geom_label(aes(y =desov$`CHL(entrada)`*625),
             label=desov$Temperatura, 
             #fill = factor(desov$`Water seasonality*`),
             #position = "identity",
             nudge_x = 1, nudge_y = 1,
             #label.size = 0.1,
             #label.padding = unit(0.55, "lines"), # Rectangle size around label
             #label.size = 0.35,
             #color = "black",
             fill= desov$`Water seasonality*`,
             check_overlap = T
  ) +
  geom_line(aes(y = desov$`CHL(entrada)`*625, color="Chlorofill"), size=1, group = 1) +

  scale_y_continuous(sec.axis = sec_axis(~./625, name = "Chlorofill")) +
  
  scale_fill_manual("Legend      ", values = "#00BFC4") + 
  
  scale_color_manual(NULL, values = "#CD9600") +
  labs(title = "Sea seed Production x Chlorofill (mg/m )" , y = "Seeds (x 1000)", x = "Period") + 
  #xlab(bquote(Seed (K)))+
  #ylab(bquote(Period ^superscript))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(order = 1), color = guide_legend(order = 2))


#y <- melt(year, idvars = year$Year)

ggplot(year, aes(year$Year, year$value)) +
  geom_bar(aes(fill = year$Origin), position = "dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Seeds Production (Sum)" , subtitle = "(milions)",
                                                                 x = "Year", y="Seeds",fill ="Type") + 
  
  scale_fill_manual(values = c("#8494FF", "#00BE67"))



ggplot(desov, aes(x = desov$`Data entra no MAR`)) +
  geom_bar(aes(y = desov$`Total Sementes 2º Manejo`, fill="Seeds Production"), stat = "identity", alpha=0.4) +
  geom_point(aes(y = desov$Temperatura*70, color="temperature"), size=2, group = 1) +
  geom_line(aes(y = desov$Temperatura*70, color="temperature"), size=1, group = 1) +
  scale_y_continuous(sec.axis = sec_axis(~./70, name = "Temperature")) +
  scale_fill_manual("Legend      ", values = "#00BFC4") +
  scale_color_manual(NULL, values = "#0CB702") +
  labs(title = "Seed Production x Temperature", y = "Seeds (x 1000)", x = "Period") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  + theme(legend.position = "bottom") +
  guides(fill = guide_legend(order = 1), color = guide_legend(order = 2))

#avaliando com 
lm1 <- lm(desov$`Total Sementes 2º Manejo` ~ desov$Temperatura + desov$`CHL(entrada)`, data = desov)
summary(lm1)  
des1 <- desov[,-1]
cor(s1)

#agora vamos tentar seed x Clorofila


ggplot(desov, aes(x = desov$`Data entra no MAR`)) +
  geom_bar(aes(y = desov$`Total Sementes 2º Manejo`, fill="Seed Production"), stat = "identity", alpha=0.4) +
  geom_point(aes(y = desov$`CHL(entrada)`*1000, color="CHL"), size=2, group = 1) +
  geom_line(aes(y = desov$`CHL(entrada)`*1000, color="CHL"), size=1, group = 1) +
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Chlorofill")) +
  scale_fill_manual("Legend      ", values = "#00BFC4") +
  scale_color_manual(NULL, values = "#C77CFF") +
  labs(title = "Seed Production x Chlorofill", y = "Seeds (x 1000)", x = "Period") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  + theme(legend.position = "bottom") +
  guides(fill = guide_legend(order = 1), color = guide_legend(order = 2))



ggplot(desov, aes(x = desov$`Data entra no MAR`)) +
  geom_point(aes(y = desov$Temperatura, color="Temperature"), size=2, group = 1) +
  geom_line(aes(y = desov$Temperatura, color="Temperature"), size=1, group = 1) +
  
  geom_point(aes(y = desov$`CHL(entrada)`*12, color="CHL"), size=2, group = 1) +
  geom_line(aes(y = desov$`CHL(entrada)`*12, color="CHL"), size=1, group = 1) +
  scale_y_continuous(sec.axis = sec_axis(~./12, name = "Chlorofill")) +
  #scale_fill_manual(values = ) +
  scale_color_manual(NULL, values = c("#0CB702", "#c77CFF")) +
  labs(title = "Temperature x Chlorofill", y = "Temperature", x = "Period") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
theme(legend.position = "bottom") +
  guides(fill = guide_legend(order = 1), color = guide_legend(order = 2))

