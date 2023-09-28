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

grubmod4<-aggregate(grubmod3,fact=5,fun="max")

ggplot()+geom_spatraster(data=grubmod3/100)+
  scale_fill_steps(n.breaks=10,low="yellow",high="red3",na.value=NA,"Grubbing probability")+
#scale_fill_continuous(low='yellow',high='red3',na.value=0,'Grubbing suitability',labels=c("Low","High"))+
 # scale_fill_gradientn(colours = c("yellow","orange","red", "red3", "red4"),na.value=NA,
#                       limits = c(0,100),,breaks=c(0,100),
 #                      labels = c("Low", "High"),
#                       "")+
      theme_bw()+geom_sf(data=svalbardutm2,fill=NA,col="grey",lwd=0.3)+
      theme(legend.position=c(0.85,0.2),legend.background = element_blank(),legend.box.background = element_rect(colour = "black"))
ggsave("Figures/grubbingmodelmap.png",height=8,width=8,units="in")

gm2<-rast("M:\\OldPCHarddrive\\Desktop\\PhD GIS\\Svalbard GIS Modelling\\habonfullext\\w001001.adf")
writeRaster(gm2,"data/grubbingmod.tif")
gm3<-rast("data/grubbingmod.tif")

gm2ag<-aggregate(gm2,fact=20,fun='mean',na.rm=T)
gm21<-subst(gm2ag,0,NA)
ggplot()+geom_spatraster(data=gm2)  
plot(gm2ag)
plot(gm2)
