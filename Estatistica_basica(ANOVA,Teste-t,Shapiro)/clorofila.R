library(ggplot2)
library(plyr)
library(dplyr)
library(patchwork)
library(magrittr)
library(readxl)
library(reshape2)
library(ggpubr)
#install.packages('magrittr')

rm(list=ls()) # remove qualquer objeto criando em sess???es anteriores
setwd("C:/Users/Leonardo/Desktop/BIG")

c <- read_excel("C:/Users/USER/Desktop/BIG/marcelo_graf.xlsx")
s <- read_excel("C:/Users/USER/Desktop/BIG/marcelo_graf.xlsx", sheet='spats')
s <- dcast(s, Year ~ Origin)


s[0:5,1] <- '2010-2014'
s[6:13,1] <- '2015-2022'

spat <- s[,-2]
spat_lab <- spat[,-3]
spat_sea <-spat[,-2]

data <- ddply(c, c("date"), summarise,
               N    = length(Chlorophyll),
               mean = mean(Chlorophyll),
               sd   = sd(Chlorophyll),
               sem = sd(Chlorophyll)/sqrt(length(Chlorophyll)))

#espelha para temperatura
dato <- ddply(c, c("date"), summarise,
              N    = length(Temperature),
              mean = mean(Temperature),
              sd   = sd(Temperature),
              sem = sd(Temperature)/sqrt(length(Temperature)))


p <- ggplot(data, aes(x=date, y=mean)) +
  geom_bar(stat="identity", fill="seagreen", width = 0.20) +
  geom_errorbar(position=position_dodge(.2), width=.1, aes(ymin=mean-sem, ymax=mean+sem)) +
  #coord_flip() + 
  #scale_y_continuous(breaks=0:30*0.5) +
  theme_test() +
  theme(axis.title = element_text(size=20),
        axis.title.y = element_text (size= 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.text.x= element_text(hjust= 0.5, vjust=0.5, size = 18)) +
  
labs(title = "p-value = 0.93", y = "Temperature (ºC)", x = "")
p


py <- ggplot(dato, aes(x=date, y=mean)) +
  geom_bar(stat="identity", fill="steelblue", width = 0.20) +
  geom_errorbar(position=position_dodge(.2), width=.1, aes(ymin=mean-sem, ymax=mean+sem)) +
  #coord_flip() + 
 # scale_y_continuous(breaks=0:30*0.5) +
  theme_test() +
  theme(axis.title = element_text(size=20),
        axis.title.y = element_text (size= 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.text.x= element_text(hjust= 0.5, vjust=0.5, size = 18)) +
  labs(title = "p-value = 0.006", y = "Chlorophyll a (mg/m³)", x = "")
 
py
#dev.off()
#tiff("Plot_chlorophyll.tiff",  height = 12, width = 16, units = 'in', compression = 'lzw', res=300)

#espelha para spats/scallop
dati <- data.frame(date=c("2010-2014", "2015-2022"),
                   mean=c(25.13,24.65),
                   sd=c(5.57,16.10))


pg <- ggplot(dati, aes(x=date, y=mean)) +
  geom_bar(stat="identity", fill="gold", width = 0.20) +
  geom_errorbar(position=position_dodge(.2), width=.1, aes(ymin=mean-sd/2, ymax=mean+sd/2)) +
  #coord_flip() + 
  #scale_y_continuous(breaks=0:30*0.5) +
  theme_test() +
  theme(axis.title = element_text(size=20),
        axis.title.y = element_text (size= 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.text.x= element_text(hjust= 0.5, vjust=0.5, size = 18)) +
  
  labs(title = "p-value = 0.99", y = "Adult Scallops (7-8 cm)", x = "")
pg


#espelha para spat_lab
datu <- ddply(spat_lab, c("Year"), summarise,
              N    = length(spats_lab),
              mean = mean(spats_lab),
              sd   = sd(spats_lab),
              sem = sd(spats_lab)/sqrt(length(spats_lab)))


pl <- ggplot(datu, aes(x=Year, y=mean)) +
  geom_bar(stat="identity", fill="coral", width = 0.20) +
  geom_errorbar(position=position_dodge(.2), width=.1, aes(ymin=mean-sem, ymax=mean+sem)) +
  #coord_flip() + 
  #scale_y_continuous(breaks=0:30*0.5) +
  theme_test() +
  theme(axis.title = element_text(size=20),
        axis.title.y = element_text (size= 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.text.x= element_text(hjust= 0.5, vjust=0.5, size = 18)) +
  
  labs(title = "p-value = 0.44", y = "Spats 0.5 mm (Lab)", x = "")
pl

#espelha para spat_lab
date <- ddply(spat_sea, c("Year"), summarise,
              N    = length(spats_sea),
              mean = mean(spats_sea),
              sd   = sd(spats_sea),
              sem = sd(spats_sea)/sqrt(length(spats_sea)))


ple <- ggplot(datu, aes(x=Year, y=mean)) +
  geom_bar(stat="identity", fill="cyan", width = 0.20) +
  geom_errorbar(position=position_dodge(.2), width=.1, aes(ymin=mean-sem, ymax=mean+sem)) +
  #coord_flip() + 
  #scale_y_continuous(breaks=0:30*0.5) +
  theme_test() +
  theme(axis.title = element_text(size=20),
        axis.title.y = element_text (size= 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.text.x= element_text(hjust= 0.5, vjust=0.5, size = 18)) +
  
  labs(title = "p-value = 0.70", y = "Spats 5 mm (Sea)", x = "")
ple

comb <- ggarrange (p, pg, pl, ple, 
                   ncol = 2, nrow = 2,
                   common.legend = FALSE, legend ="bottom", 
                   #labels= c("A", "B"),
                   hjust=-0.3,
                   font.label= list(size=16))
comb
py
