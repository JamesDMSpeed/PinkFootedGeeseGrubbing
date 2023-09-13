library(terra)
library(tidyterra)
library(geodata)
library(ggplot2)
library(RColorBrewer)

grubmod<-rast('M:\\Grubbing 2013\\Grubbing Likelihood1.bil')

sjm<-gadm(country="SJM",level=1,"data")
svalbard<-sjm[sjm$NAME_1=="Svalbard"]
svalbardutm<-project(svalbard,grubmod)
svalbardutm2<-crop(svalbardutm,grubmod)
grubmod2<-mask(grubmod,svalbardutm)
grubmod3<-subst(grubmod2,0,NA)

ggplot()+geom_spatraster(data=grubmod3)+
  #scale_fill_continuous(low='yellow',high='red3',na.value=0,'Grubbing suitability',labels=c("Low","High"))+
  scale_fill_gradientn(colours = c("yellow", "orange", "red"),na.value=NA,
                       limits = c(0,100), breaks = c(20, 50, 100),
                       labels = c("Low", "", "High"),
                       "")+
      theme_bw()+geom_sf(data=svalbardutm2,fill=NA,col="grey",lwd=0.3)#+
  theme(legend.direction="horizontal")


  
