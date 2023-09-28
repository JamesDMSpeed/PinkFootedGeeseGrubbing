#Svalbard grubbing analyses: 2006 to 2022

rm(list=ls())

#Load libraries
library(tidyverse)
library(ggplot2)
library(car)
library(DHARMa)
library(glmmTMB)
library(effects)
library(ggeffects)
library(cowplot)


#Load in pre-wrangeld data
grubbingdat<-read.csv("data/grubbing_plots_merged_prelim_3.csv",header=T)
grubbingdat<-grubbingdat[!is.na(grubbingdat$Year),]
View(grubbingdat)

#Add goose population estimates - joining on year
goosepop<-read.csv("data/goosepop_simple.csv",header=T)
names(goosepop)[4:5]<-c("GoosePop_mean",'GoosePop_sd')
grubbingdat<-left_join(grubbingdat,goosepop,join_by(Year==year))

#CHeck number of observations per valley and year
table(grubbingdat$Year,grubbingdat$Location_Valley)
plotsperyearvalley<-grubbingdat %>% group_by(Year,Location_Valley) %>% summarise(count=n())
plotsperyearvalley

#Total per year
plotsperyear<-grubbingdat %>% group_by(Year) %>% summarise(count=n())
ggplot(data=plotsperyear,aes(x=Year,y=count))+geom_line()+geom_point()+theme_bw()+ylab("Number of grubbing observations")
ggplot()+geom_col(data=plotsperyear,aes(x=Year,y=count))+theme_bw()+ylab("Number of grubbing observations")


ggplot(data=plotsperyearvalley,aes(x=Year,y=count,fill=Location_Valley))+geom_bar(position = "stack",stat="identity")+theme_bw()+ylab("Number of grubbing observations")


#Frequency distribution of grubbing intensity
ggplot(data=grubbingdat[grubbingdat$Grubbing_Intensity<=1.0 & grubbingdat$Grubbing_Intensity>0,])+geom_density(aes(x=Grubbing_Intensity,group=Year,color=Year),bw=0.1)+
  theme_bw()


#Grubbing intensity and extent - per year, valley and vegetation type

#Extent (proportion of total observations with grubbing present)
grubbingproportion<-grubbingdat %>% group_by(Year,Location_Valley,VegetationType_reclassified,GoosePop_mean) %>% summarise(prop=sum(Grubbing_PresenceAbsence)/n())                                                                               
#Intensity (where grubbing is present, the % area of observation unit grubbed)
grubbingintensity<-grubbingdat[grubbingdat$Grubbing_PresenceAbsence==1,] %>% group_by(Year,Location_Valley,VegetationType_reclassified,GoosePop_mean) %>% summarise(meangrubint=mean(Grubbing_Intensity))

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


#GLMMs
#Grubbing extent
#Binomial logitlink
#Fitting offset against plot size, random slope (on goose pop) and intercept for valleys. No zero inflation. 

tmb1<-glmmTMB(Grubbing_PresenceAbsence~GoosePop_mean*VegetationType_reclassified + offset(SpatialScale_GrubbingRecording)+(1|Location_Valley)+(GoosePop_mean|Location_Valley), data=grubbingdat, ziformula=~0, family=binomial(link='logit'))
summary(tmb1)

#Evaluation and visualisation
#DHARMa residuals test
tmb1_simres<-simulateResiduals(tmb1)
plot(tmb1_simres) #Looks good

#Anova test (Wald)
Anova(tmb1) #Significant effect interaction between vegetation type and goose population

#Visualising effects
effs<-allEffects(tmb1)
effs
plot(effs)

ggpredict(tmb1, terms = c("GoosePop_mean",'VegetationType_reclassified',"Location_Valley"), type = "re") %>% plot()
ggeffect(tmb1, terms = c("GoosePop_mean",'VegetationType_reclassified')) %>% plot()


ge1<-ggeffect(tmb1, terms = c("GoosePop_mean",'VegetationType_reclassified'))
  #geom_point(data=grubbingproportion,aes(x=GoosePop_mean,y=prop*100))

#Intensity
#No offset? 
#Random intercept and slope on goose population for valleys.
#Only locations where grubbing occurred (i.e. not grubbing absences)
#Ordered beta regression family
#Kubinec R (2022). "Ordered Beta Regression: A Parsimonious, Well-Fitting Model for Continuous Data with Lower and Upper Bounds." Political Analysis. doi:10.1017/pan.2022.20.
tmb2<-glmmTMB(Grubbing_Intensity~GoosePop_mean*VegetationType_reclassified +(1|Location_Valley),data=grubbingdat[grubbingdat$Grubbing_Intensity<1.0 & grubbingdat$Grubbing_Intensity>0 ,], family=ordbeta(link='logit'))
summary(tmb2)

tmb2_simres<-simulateResiduals(tmb2)
plot(tmb2_simres) #Residuals are deviating
Anova(tmb2)


effs2<-allEffects(tmb2)
effs2
plot(effs2)

ggpredict(tmb2, terms = c("GoosePop_mean",'VegetationType_reclassified',"Location_Valley"), type = "re") %>% plot()
ge2<-ggeffect(tmb2,terms = c("GoosePop_mean",'VegetationType_reclassified')) #%>% plot()

#Adding model effects to the scatterplot
gLg<-ggplot(data=grubbingproportion)+geom_point(data=grubbingproportion,aes(x=GoosePop_mean,y=prop,color=Location_Valley,pch=VegetationType_reclassified))+
  ylab("Grubbing Extent \n(proportion of observations grubbed)")+scale_x_continuous(limits=c(55,80))+
  xlab("Goose population")+theme_bw()+
#  stat_smooth(data=grubbingproportion,method='lm',aes(lty=VegetationType_reclassified))+
  geom_line(data=ge1,aes(x=x,y=predicted,lty=group))+
  geom_ribbon(data=ge1,aes(x=x, ymin = conf.low , ymax = conf.high, group=group), alpha = 0.2) 

gLg

gPg<-ggplot(data=grubbingintensity)+geom_point(data=grubbingintensity,aes(x=GoosePop_mean,y=meangrubint,color=Location_Valley,pch=VegetationType_reclassified))+
  ylab("Grubbing Intensity \n(proportion of plot grubbed)")+theme_bw()+theme(legend.position='none')+
  #stat_smooth(data=grubbingintensity,aes(lty=VegetationType_reclassified),method='lm')+
  scale_x_continuous(limits=c(55,80))+
  geom_line(data=ge2,aes(x=x,y=predicted,lty=group))+
  geom_ribbon(data=ge2,aes(x=x, ymin = conf.low , ymax = conf.high, group=group), alpha = 0.2) 
gPg
plot_grid(gLg,gPg,align="v",axis='lr',nrow=2)
ggsave("Figures/Extent_Intensity_pop.png",height=10,width=8,units='in')
