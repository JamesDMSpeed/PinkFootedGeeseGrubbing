#Svalbard grubbing analyses: 2006 to 2022

rm(list=ls())

#Load libraries
library(tidyverse)#wrangling
library(ggplot2)#Plotting
library(car)
library(DHARMa)#Residual checks
library(glmmTMB)#Modelling 
library(effects)#Visualising
library(ggeffects)#Visualising
library(cowplot)#Visualising


#Load in data, wrangled by Isabell
grubbingdat<-read.csv("data/grubbing_plots_merged.csv",header=T)
#Remove row with NA in year
grubbingdat<-grubbingdat[!is.na(grubbingdat$Year),]
View(grubbingdat)

#Add goose population estimates - joining on year
goosepop<-read.csv("data/goosepop_simple.csv",header=T)
names(goosepop)[4:5]<-c("GoosePop_mean",'GoosePop_sd')
#goosepop$GoosePop_mean<-goosepop$GoosePop_mean * 1000
grubbingdat<-left_join(grubbingdat,goosepop,join_by(Year==year))

table(grubbingdat$Year,grubbingdat$Survey_type)
#Deselecting 2015 Virve's plots on transects
grubbingdatmod <- subset(grubbingdat, !(DataShepherd %in% "Virve" & Year %in% 2015 & Survey_type %in% "plots_on_transect"))
grubbingdatmod<-grubbingdatmod[!is.na(grubbingdatmod$VegetationType_reclassified),]

#Total per year
plotsperyear<-grubbingdatmod %>% group_by(Year) %>% summarise(count=n())
ggplot(data=plotsperyear,aes(x=Year,y=count))+geom_line()+geom_point()+theme_bw()+ylab("Number of grubbing observations")
ggplot()+geom_col(data=plotsperyear,aes(x=Year,y=count))+theme_bw()+ylab("Number of grubbing observations")

#Check number of observations per valley and year (after removing Virve's)
table(grubbingdatmod$Year,grubbingdatmod$Location_Valley)
plotsperyearvalley<-grubbingdatmod %>% group_by(Year,Location_Valley) %>% summarise(count=n())
plotsperyearvalley

ggplot(data=plotsperyearvalley,aes(x=Year,y=count,fill=Location_Valley))+geom_bar(position = "stack",stat="identity")+theme_bw()+ylab("Number of grubbing observations")

#Frequency distribution of grubbing intensity
ggplot(data=grubbingdatmod[grubbingdatmod$Grubbing_Intensity<=1.0 & grubbingdatmod$Grubbing_Intensity>0,])+geom_density(aes(x=Grubbing_Intensity,group=Year,color=Year),bw=0.1)+
  theme_bw()


#Grubbing intensity and extent - per year, valley and vegetation type
#Extent (proportion of total observations with grubbing present)
grubbingproportion<-grubbingdatmod %>% group_by(Year,Location_Valley,VegetationType_reclassified,GoosePop_mean,snowmelt,mayday) %>% summarise(prop=sum(Grubbing_PresenceAbsence)/n())                                                                               
#Intensity (where grubbing is present, the % area of observation unit grubbed)
grubbingintensity<-grubbingdatmod[grubbingdatmod$Grubbing_PresenceAbsence==1,] %>% group_by(Year,Location_Valley,VegetationType_reclassified,GoosePop_mean,snowmelt,mayday) %>% summarise(meangrubint=mean(Grubbing_Intensity))

#Plots by time
gP<-ggplot(data=grubbingintensity,aes(x=Year,y=meangrubint))+geom_point(data=grubbingintensity,aes(color=Location_Valley,pch=VegetationType_reclassified))+
  ylab("Grubbing Intensity \n(proportion of plot grubbed)")+theme_bw()+stat_smooth(data=grubbingintensity,method='lm',aes(lty=VegetationType_reclassified))
gL<-ggplot(data=grubbingproportion,aes(x=Year,y=prop))+geom_point(data=grubbingproportion,aes(color=Location_Valley,pch=VegetationType_reclassified))+
  ylab("Grubbing Extent \n(proportion of observations grubbed)")+theme_bw()+stat_smooth(data=grubbingproportion,method='lm',aes(lty=VegetationType_reclassified))

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


#Checking correlations between explanatory variables
cor(select(grubbingdatmod,c(snowmelt,mayday,GoosePop_mean,Year)),use = "pairwise")#snowmelt and maytemp are correlated. Use only one (snowmelt selected as lower correlatoin with year)


#GLMMs
#Grubbing extent
#Binomial logitlink
#Fitting offset against plot size, random slope (on goose pop) and intercept for valleys. No zero inflation. 
#Snow melt fitted as additive covariate - convergence problems occur if interactions fit with goose population randomslope
tmb1<-glmmTMB(Grubbing_PresenceAbsence~GoosePop_mean*VegetationType_reclassified+snowmelt  + offset(SpatialScale_GrubbingRecording)+(1|Location_Valley)+(GoosePop_mean|Location_Valley), data=grubbingdatmod,  family=binomial(link='logit'), ziformula=~0,)
#Convergence problems. Remove the random slope
tmb1b<-glmmTMB(Grubbing_PresenceAbsence~GoosePop_mean*VegetationType_reclassified +snowmelt + offset(SpatialScale_GrubbingRecording)+(1|Location_Valley), data=grubbingdatmod,  family=binomial(link='logit'), ziformula=~0,)
summary(tmb1b)
tmb1c<-glmmTMB(Grubbing_PresenceAbsence~GoosePop_mean*VegetationType_reclassified  + offset(SpatialScale_GrubbingRecording)+(1|Location_Valley), data=grubbingdatmod,  family=binomial(link='logit'), ziformula=~0,)


#Evaluation and visualisation
#DHARMa residuals test
tmb1_simres<-simulateResiduals(tmb1b)
plot(tmb1_simres) #Looks good although some deviation in the KS

#Anova test (Wald)
Anova(tmb1b)

#Visualising effects
effs<-allEffects(tmb1b)
effs
plot(effs)

#ggpredict(tmb1b, terms = c("GoosePop_mean",'VegetationType_reclassified',"Location_Valley"), type = "re") %>% plot()
ggeffect(tmb1b, terms = c("GoosePop_mean",'VegetationType_reclassified')) %>% plot()


ge1<-ggeffect(tmb1b, terms = c("GoosePop_mean",'VegetationType_reclassified'))
  #geom_point(data=grubbingproportion,aes(x=GoosePop_mean,y=prop*100))
ggpredict(tmb1b,terms=c("GoosePop_mean [56, 79]","VegetationType_reclassified"))

#Intensity
#No offset? 
#Random intercept and slope on goose population for valleys.
#Only locations where grubbing occurred (i.e. not grubbing absences)
#Ordered beta regression family
#Kubinec R (2022). "Ordered Beta Regression: A Parsimonious, Well-Fitting Model for Continuous Data with Lower and Upper Bounds." Political Analysis. doi:10.1017/pan.2022.20.
tmb2b<-glmmTMB(Grubbing_Intensity~GoosePop_mean*VegetationType_reclassified+snowmelt +(1|Location_Valley),data=grubbingdatmod[grubbingdatmod$Grubbing_Intensity<1.0 & grubbingdatmod$Grubbing_Intensity>0 &!is.na(grubbingdatmod$snowmelt),], family=ordbeta(link='logit'))
summary(tmb2b)

tmb2_simres<-simulateResiduals(tmb2b)
plot(tmb2_simres) #Residuals are ok
Anova(tmb2b) #No effect of snowmelt - remove

tmb2<-glmmTMB(Grubbing_Intensity~GoosePop_mean*VegetationType_reclassified +(1|Location_Valley),data=grubbingdatmod[grubbingdatmod$Grubbing_Intensity<1.0 & grubbingdatmod$Grubbing_Intensity>0 ,], family=ordbeta(link='logit'))
summary(tmb2)
Anova(tmb2)

effs2<-allEffects(tmb2)
effs2
plot(effs2)

ggpredict(tmb2, terms = c("GoosePop_mean",'VegetationType_reclassified',"Location_Valley"), type = "re") %>% plot()
ge2<-ggeffect(tmb2,terms = c("GoosePop_mean",'VegetationType_reclassified')) #%>% plot()
ggpredict(tmb2,terms=c("GoosePop_mean [56, 79]","VegetationType_reclassified"))

#Adding model effects to the scatterplot
gLg<-ggplot(data=grubbingproportion)+geom_point(data=grubbingproportion,aes(x=GoosePop_mean,y=prop,color=Location_Valley,pch=VegetationType_reclassified))+
  ylab("Grubbing Prevalence \n(proportion of observations grubbed)")+scale_x_continuous(limits=c(55,80),labels=function(breaks)breaks*1000)+
  xlab("")+theme_bw()+
 #  stat_smooth(data=grubbingproportion,method='lm',aes(lty=VegetationType_reclassified))+
  geom_line(data=ge1,aes(x=x,y=predicted,lty=group))+
  geom_ribbon(data=ge1,aes(x=x, ymin = conf.low , ymax = conf.high, group=group), alpha = 0.2)+
  theme(legend.position='none')

gLg

gPg<-ggplot(data=grubbingintensity)+geom_point(data=grubbingintensity[!is.na(grubbingintensity$VegetationType_reclassified),],aes(x=GoosePop_mean,y=meangrubint,color=Location_Valley,pch=VegetationType_reclassified))+
  ylab("Grubbing Intensity \n(proportion of plot grubbed)")+theme_bw()+xlab("Goose population")+
  #stat_smooth(data=grubbingintensity,aes(lty=VegetationType_reclassified),method='lm')+
  scale_x_continuous(limits=c(55,80),labels=function(breaks)breaks*1000)+
  geom_line(data=ge2,aes(x=x,y=predicted,lty=group))+
  geom_ribbon(data=ge2,aes(x=x, ymin = conf.low , ymax = conf.high, group=group), alpha = 0.2) +
  theme(legend.key.height = unit(0.3, 'cm'))+
  labs(shape="Habitat",colour="Valley",linetype="Habitat")
gPg

plot_grid(gLg,gPg,align="v",axis='lr',nrow=2)

ggsave("Figures/Extent_Intensity_pop.png",height=10,width=8,units='in')

#Write outputs to file

sink("Outputs/ModelOutput.txt")
  print("Prevalence model - Wald tests")
  print(Anova(tmb1b))
  print("Prevalence model - Model summary")
  print(summary(tmb1b))
  print("Intensity model - Wald tests - remove snowmelt")
  print(Anova(tmb2b))
  print("Intensity model - Wald tests after snowmelt removed")
  print(Anova(tmb2))
  print("Intensity model - Model summary")
  print(summary(tmb2))
sink()  # returns output to the console

