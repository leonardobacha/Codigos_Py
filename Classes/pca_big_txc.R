library(readxl)

library(factoextra)
library(FactoMineR)
library(leaps)
require(MASS)
library("PerformanceAnalytics")
library(caret)
library("xlsx")
library("RcmdrMisc")  #pacote para usar colPercent e rowPercent
library(gridExtra)
library(corrplot)

rm(list=ls())



#ex2 <-read_excel("/Users/USER/Desktop/BIG_RIOS/etc/BIG_Planilha.xlsx", sheet = "pca")
ex2 <-read_excel("/Users/USER/Desktop/corr_args.xlsx", sheet = "pca")
ex2
summary(ex2)
#row.names(ex2)<-ex2$Code
row.names(ex2)<-ex2$Local
ex2 <- ex2[,-1]
#ex2['local'] 
summary(ex2)
#ex2 <- ex2[1:8,]

#ex2t  <- as.data.frame(t(ex2))
#remove_cols <- nearZeroVar(ex2, names = TRUE, freqCut = 2, uniqueCut = 20) #corte mais radical
remove_cols <- nearZeroVar(ex2, names = TRUE) #corte mais ameno
remove_cols
# Get all column names from bloodbrain_x: all_cols
all_cols <- names(ex2)
# Remove from data: bloodbrain_x_small
ex2_x_small <- ex2[ , setdiff(all_cols, remove_cols)]


#ex2_x_small[1:53,6:22] #para corte padrao near zero
#ex2_x_small[1:53,6:121] #para corte padrao near zero
#ex2_x_small[1:53,6:17] #para corte mais radical




#res.pca <- prcomp(ex2_x_small[1:53,6:121], scale = TRUE)
res.pca <- prcomp(t(ex2))
#res.pca <- prcomp(ex2[1:53,6:469])
#res.pca <- PCA(ex2_x_small[1:53,6:121], graph = FALSE,scale = TRUE)
#res.pca <- PCA(ex2[1:53,6:469], graph = FALSE,scale = TRUE)
#res.pca <- prcomp(ex2[1:53,6:469])
#res.pca <- prcomp(ex2, scale = TRUE)

res.pca
fviz_eig(res.pca)
png("pcn.png",
    width = 9*300,        # 6 x 300 pixels
    height = 7*300,
    res = 300,            # 300 pixels per inch
    pointsize = 7)        # smaller font size
fviz_pca_var(res.pca, select.var = list(contrib = 20), col.var = "black",repel = TRUE, margins =c(12,9))
dev.off()

fviz_pca_ind(res.pca,
             #col.ind = ex2$Fish,
             axes = c(2,3),
             #habillage = ex2_x_small$Fish,
             #habillage = ex2_x_small$Portion,
             col.ind = "cos2", # Color by the quality of representation
             #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             pointsize = 3,
             #pointshape = ex2_x_small$Fish,
             repel = TRUE     # Avoid text overlapping
)
dev.off()

var <- get_pca_var(res.pca)
head(var$coord, 20)

?fviz_pca_biplot
#pdf("file_pca_filo_biplot_16stotal.pdf",width=8,height=6,paper='special')
png("pca_biplot_n.png",
    width = 8*300,        # 6 x 300 pixels
    height = 6*300,
    res = 300,            # 300 pixels per inch
    pointsize = 6)        # smaller font size
fviz_pca_biplot(res.pca,
                legend.title = "Sample spots",
                repel = T,
                geom.ind =  c("point", "text"),
                alpha.ind =  1.0,
                col.ind =rownames(t(ex2)),
                #col.ind = ex2_x_small$Fish,
                select.var = list(contrib = 15),
                mean.point = FALSE,
                #col.ind =  ex2_x_small$Fish,
                #fill.ind = ex2_x_small$Portion,
                pointsize = 3,
                #geom.var = c("arrow", "text"),
                col.var = "black",
                alpha.var = "contrib",
                title = NULL
                
)
dev.off()

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
contrib <- res.var$contrib        # Contributions to the PCs
contrib
write.csv2(res.var$contrib,file = "pca_contrib_allVariables.csv")
#write.xlsx(res.var$contrib,file="pca_contrib_allVariables.xlsx",col.names = TRUE, row.names = TRUE)
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2    

#t-test
library(readxl)
ex3 <-read_excel("/Users/USER/Desktop/corr_args.xlsx", sheet = "Planilha2")
ex3 <- ex3[,-1]
t.test(ex3$CJA...2,ex3$CJA...3)
t.test(ex3$JAC...4,ex3$JAC...5)
t.test(ex3$BRA...18,ex3$BRA...19)
t.test(ex3$CEN...6,ex3$CEN...7)
t.test(ex3$JAP...8,ex3$JAP...9)
t.test(ex3$FRA...10,ex3$FRA...11)
t.test(ex3$TAQ...12,ex3$TAQ...13)
t.test(ex3$PAR...14,ex3$PAR...15)
t.test(ex3$FOZ...16,ex3$FOZ...17)
t.test(ex3$MAM...20,ex3$MAM...21)
#anova,
ex4 <-read_excel("/Users/USER/Desktop/corr_args.xlsx", sheet = "anova")


anova<-aov(ex4$Soma_Toxic_componds+ex4$Soma_args~ex4$Grupo)
summary(anova)
TukeyHSD(anova)

ex5 <-read_excel("/Users/USER/Desktop/corr_args.xlsx", sheet = "anova_2")


anova_2<-aov(ex5$Soma_Toxic_componds+ex5$Soma_args~ex5$Grupo)
summary(anova_2)
TukeyHSD(anova_2)
