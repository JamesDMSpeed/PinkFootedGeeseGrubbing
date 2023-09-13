#Svalbard grubbing analyses
library(tidyverse)
library(ggplot2)


#file="https://npolar-my.sharepoint.com/:x:/r/personal/linn_voldstad_npolar_no/Documents/Grubbing_synthesis-Shared/Data_cleaned_forR/grubbing_plots_merged_prelim_1.csv?d=w1912586f83a7405eb379870cef5a7ce9&csf=1&web=1&e=ba202u&nav=MTVfezAwMDAwMDAwLTAwMDEtMDAwMC0wMDAwLTAwMDAwMDAwMDAwMH0"
#download.file(file,"data")

grubbingdat<-read.csv("data/grubbing_plots_merged_prelim_2.csv",header=T)
View(grubbingdat)

#Add goose population estimates to year
goosepop<-read.csv("data/goosepop_simple.csv",header=T)
names(goosepop)[4:5]<-c("GoosePop_mean",'GoosePop_sd')
grubbingdat<-left_join(grubbingdat,goosepop,join_by(Year==year))


table(grubbingdat$Year,grubbingdat$Location_Valley)

plotsperyearvalley<-grubbingdat %>% group_by(Year,Location_Valley) %>% summarise(count=n())
plotsperyearvalley
plotsperyear<-grubbingdat %>% group_by(Year) %>% summarise(count=n())

ggplot(data=plotsperyear,aes(x=Year,y=count))+geom_line()

grubbingintensity<-grubbingdat %>% group_by(Year,Location_Valley,VegetationType_reclassified,GoosePop_mean) %>% summarise(meangrubint=mean(Grubbing_Intensity))
grubbingproportion<-grubbingdat %>% group_by(Year,Location_Valley,VegetationType_reclassified,GoosePop_mean) %>% summarise(prop=sum(Grubbing_PresenceAbsence)/n())                                                                               

#Plots by time
gP<-ggplot(data=grubbingintensity,aes(x=Year,y=meangrubint))+geom_point()+
  ylab("Grubbing Intensity \n(proportion of plot grubbed)")+theme_bw()+stat_smooth(method='lm')
gL<-ggplot(data=grubbingproportion,aes(x=Year,y=prop))+geom_point()+
  ylab("Grubbing Extent \n(proportion of observations grubbed)")+theme_bw()+stat_smooth(method='lm')

plot_grid(gL,gP,align="v",axis="l",nrow=2)

#Plots by goose pop
gLg<-ggplot(data=grubbingproportion,aes(x=GoosePop_mean,y=prop))+geom_point(data=grubbingproportion,aes(color=Location_Valley,pch=VegetationType_reclassified))+
  ylab("Grubbing Extent \n(proportion of observations grubbed)")+scale_x_continuous(limits=c(55,80))+
  xlab("Goose population")+theme_bw()+
  stat_smooth(data=grubbingproportion,method='lm',aes(lty=VegetationType_reclassified))
gPg<-ggplot(data=grubbingintensity,aes(x=GoosePop_mean,y=meangrubint))+geom_point(data=grubbingintensity,aes(color=Location_Valley,pch=VegetationType_reclassified))+
  ylab("Grubbing Intensity \n(proportion of plot grubbed)")+theme_bw()+theme(legend.position='none')+
  stat_smooth(data=grubbingintensity,aes(lty=VegetationType_reclassified),method='lm')+scale_x_continuous(limits=c(55,80))

plot_grid(gLg,gPg,align="v",axis='lr',nrow=2)



#Simple glm
glm1<-glm(data=grubbingproportion,prop~GoosePop_mean*VegetationType_reclassified,family="binomial")
summary(glm1)
anova(glm1,test="Chisq")

glm2<-glm(data=grubbingdat,Grubbing_PresenceAbsence~GoosePop_mean*VegetationType_reclassified,family="binomial",offset=SpatialScale_GrubbingRecording)
summary(glm2)


#Very simple GLMM

library(lme4)
bin1<-glmer(data=grubbingdat,Grubbing_PresenceAbsence~GoosePop_mean*VegetationType_reclassified+1|Location_Valley,family = "binomial")
library(glmmTMB)

tmb1<-glmmTMB(Grubbing_PresenceAbsence~GoosePop_mean*VegetationType_reclassified + offset(SpatialScale_GrubbingRecording)+(1|Location_Valley)+(GoosePop_mean|Location_Valley), data=grubbingdat, ziformula=~0, family=binomial)
summary(tmb1)

library(DHARMa)

tmb1_simres<-simulateResiduals(tmb1)
plot(tmb1_simres)
library(effects)
effs<-allEffects(tmb1)
effs
plot(effs)
library(car)
Anova(tmb1)


library(ggeffects)
ggpredict(tmb1, terms = c("GoosePop_mean",'VegetationType_reclassified',"Location_Valley"), type = "re") %>% plot()
ggeffect(tmb1, terms = c("GoosePop_mean",'VegetationType_reclassified')) %>% plot()
  #geom_point(data=grubbingproportion,aes(x=GoosePop_mean,y=prop*100))
