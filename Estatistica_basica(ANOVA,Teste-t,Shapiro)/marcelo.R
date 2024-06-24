library(ggplot2)
df <- data.frame(date=c("2013-2016", "2017-2022"),
                 mean=c(14.4,18.2),
                 sd=c(5.9,11.4))
df
#simples
p<-ggplot(data=df, aes(x=date, y=mean)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + geom_errorbar(aes(ymin=mean-sd/2, ymax=mean+sd/2), width=.2,
                                position=position_dodge(.9)) + 
  labs(title = "Period Comparison", subtitle = "p-value = 0,013",
  y = "Chlorophyll a (mg/m³)", x = "") 

p




#Mais rebuscado
py <- ggplot(df, aes(x=date, y=mean)) +
  geom_bar(stat="identity", fill="steelblue", width = 0.20) +
  geom_errorbar(position=position_dodge(.2), width=.1, aes(ymin=mean-sd, ymax=mean+sd)) +
  #coord_flip() + 
  # scale_y_continuous(breaks=0:30*0.5) +
  theme_test() +
  theme(axis.title = element_text(size=20),
        axis.title.y = element_text (size= 20),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.text.x= element_text(hjust= 0.5, vjust=0.5, size = 18)) +
  labs(title = "", y = "%", x = "")

py