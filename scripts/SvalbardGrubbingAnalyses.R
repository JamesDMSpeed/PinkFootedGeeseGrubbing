#Svalbard grubbing analyses
library(tidyverse)
library(ggplot2)


#file="https://npolar-my.sharepoint.com/:x:/r/personal/linn_voldstad_npolar_no/Documents/Grubbing_synthesis-Shared/Data_cleaned_forR/grubbing_plots_merged_prelim_1.csv?d=w1912586f83a7405eb379870cef5a7ce9&csf=1&web=1&e=ba202u&nav=MTVfezAwMDAwMDAwLTAwMDEtMDAwMC0wMDAwLTAwMDAwMDAwMDAwMH0"
#download.file(file,"data")

grubbingdat<-read.csv("data/grubbing_plots_merged_prelim_1.csv",header=T)
View(grubbingdat)


table(grubbingdat$Year,grubbingdat$Location_Valley)
