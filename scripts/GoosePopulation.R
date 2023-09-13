#Pink footed goose population
#The reference to the dataset is the EGMP Data Centre:
#  https://gitlab.com/aewa-egmp/svalbard-population-of-pink-footed-goose/harvest-assessment-2023/-/blob/main/PFG_IPM_results.txt?ref_type=heads
rm(list=ls())
library(ggplot2)  
library(readxl)

goosepopdat<-read_xlsx("data/pfg_ipm_results_2023.xlsx",sheet='pfg_ipm_results_2023')
                       #col_types=c("numeric",'text',"numeric","numeric","numeric","numeric","numeric"))
View(goosepopdat)

#Just the Nm values
goosepopdat1<-data.frame(goosepopdat[1:32,])
goosepopdat1$mean<-as.numeric(goosepopdat1$mean)
goosepopdat1$sd<-as.numeric(goosepopdat1$sd)


ggplot(data=goosepopdat1,aes(x=year,y=mean))+geom_line()+
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd), alpha = .2)+
  theme_bw()+scale_y_continuous(limits=c(0,90),breaks=seq(0,90,by=10),labels=seq(0,90000,by=10000))+
  xlab("Year")+ylab("Population")+
  geom_vline(xintercept=2013)+
  geom_vline(xintercept=2006,lty=2)

write.csv(goosepopdat1,"data/goosepop_simple.csv")  
  