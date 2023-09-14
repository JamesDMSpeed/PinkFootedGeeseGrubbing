## Cleaning script for grubbing data
## Output: A merged file with all grubbing data

## Target column names
## Year, Date, Location_Valley, Sublocation, Survey_type, UTM_E, UTM_N, Datum, UTMZone, 
## DataShepherd, Grubbing_PresenceAbsence, Grubbing_Intensity, GrubbingType, SpatialScale_GrubbingRecording, 
## VegetationType_as_provided, VegetationType_Reclassified)

## Load Libraries

library(tidyverse)

## RENE

Plots_alldata_COATandnonCOAT <- read_delim("data/GrubbingDataRaw/Plots_alldata_COATandnonCOAT.txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE)

plots_rene<-Plots_alldata_COATandnonCOAT %>% 
  filter(!(Recorder == 'LEL' & Valley=="Reindalen")) %>% 
  select(Year=Year, Date=Date, Sublocation=Valley, Survey_type=Plot_type, UTM_E=GPS_X, UTM_N=GPS_Y, Datum=Coordinate_system, Grubbing_PresenceAbsence=Grubbing_present, Grubbing_Intensity=Area_impacted, VegetationType_as_provided=Veg_type_reduced) %>% 
  filter((!Survey_type == 'COAT')) %>% #Remove COAT data -loaded from seperate file
  mutate(DataShepherd="Rene") %>% 
  mutate(SpatialScale_GrubbingRecording= case_when(Survey_type == "Valleywalk" ~ 1,
                                                   Survey_type == "Large" ~ 4)) %>% 
  mutate(Grubbing_Intensity= case_when(Grubbing_Intensity == "L" ~ 0.025,
                                       Grubbing_Intensity == "M" ~ 0.15,
                                       Grubbing_Intensity == "H" ~ 0.375,
                                       Grubbing_Intensity == "VH" ~ 0.75)) %>% 
  drop_na(Grubbing_PresenceAbsence) %>% 
  mutate(Grubbing_PresenceAbsence= case_when(Grubbing_PresenceAbsence == "Y" ~ 1,
                                             Grubbing_PresenceAbsence == "N" ~ 0)) %>% 
  mutate(VegetationType_reclassified= case_when(VegetationType_as_provided == 0 ~ NA,
                                                VegetationType_as_provided == 1 ~ NA,
                                                VegetationType_as_provided == 2 ~ "dry",
                                                VegetationType_as_provided == 2.5 ~ "dry",
                                                VegetationType_as_provided == 3 ~ "dry",
                                                VegetationType_as_provided == 3.5 ~ "dry",
                                                VegetationType_as_provided == 4 ~ "mesic",
                                                VegetationType_as_provided == 5 ~ "mesic",
                                                VegetationType_as_provided == 6 ~ "wet",
                                                VegetationType_as_provided == 7 ~ "wet",
                                                VegetationType_as_provided == 8 ~ "wet")) %>% 
  mutate(Location_Valley= case_when(Sublocation == "Istjorndalen"|Sublocation == "Kalvdalen"| Sublocation == "Passdalen"|
                                      Sublocation == "Semmeldalen"|Sublocation == "Skiferdalen" ~ "Semmeldalen",
                                    Sublocation == "Bodalen"|Sublocation == "Colesdalen" ~ "Colesdalen")) %>% 
  select(!Date) %>% 
  mutate(VegetationType_as_provided=as.factor(VegetationType_as_provided))


## JAMES

Transects_James <- read_table("data/GrubbingDataRaw/Transects_James.txt")

# with CentralNewGrubbing as grubbing intensity variable, presence absence calculated from same variable

transect_james<-Transects_James %>% select(Year=Year, Date=Date, Sublocation=Valley, Survey_type=Survey_type, UTM_E=X, UTM_N=Y, Datum=Datum, UTMZone=Zone, Grubbing_Intensity=CentralNewGrubbing, VegetationType_as_provided=Vegetation_type2) %>% 
  mutate(Grubbing_Intensity=Grubbing_Intensity/100) %>% 
  mutate(Grubbing_PresenceAbsence = ifelse((Grubbing_Intensity > 0), 1, 0)) %>% 
  mutate(DataShepherd="James") %>% 
  mutate(SpatialScale_GrubbingRecording=0.1225) %>% 
  mutate(Survey_type="plots_on_transect") %>% 
  mutate(VegetationType_reclassified= case_when(VegetationType_as_provided == 0 ~ NA,
                                                VegetationType_as_provided == 1 ~ NA,
                                                VegetationType_as_provided == 2 ~ "dry",
                                                VegetationType_as_provided == 3 ~ "dry",
                                                VegetationType_as_provided == 3.5 ~ "dry",
                                                VegetationType_as_provided == 4 ~ "mesic",
                                                VegetationType_as_provided == 5 ~ "mesic",
                                                VegetationType_as_provided == 6 ~ "wet",
                                                VegetationType_as_provided == 7 ~ "wet",
                                                VegetationType_as_provided == 8 ~ "wet")) %>% 
  mutate(Location_Valley= case_when(Sublocation == "Adventdalen"|Sublocation == "Plataafjellet" ~ "Adventdalen",
                                    Sublocation == "Bohemanflya" ~ "Bohemanflya",
                                    Sublocation == "Brattlidalen"|Sublocation == "Fredheim" |Sublocation == "Noisdalen" |Sublocation == "Sassendalen" ~ "Sassendalen",
                                    Sublocation == "Semmeldalen" ~ "Semmeldalen",
                                    Sublocation == "Colesdalen" ~ "Colesdalen")) %>% 
  select(!Date) %>% 
  mutate(VegetationType_as_provided=as.factor(VegetationType_as_provided))



## COAT PLOTS

coat_grubbing_plot_2017_2022_ie <- read_csv("data/GrubbingDataRaw/coat_grubbing_plot_2017_2022_ie.csv")

pf_2022 <- read_delim("data/GrubbingDataRaw/coat_sv_veg_2022_site_plots.txt", delim = "\t", escape_double = FALSE,trim_ws = TRUE)
coordinates_coat<-pf_2022 %>% select(site_id, gps_east, gps_north) %>% 
  unique()


coat_plots <- coat_grubbing_plot_2017_2022_ie %>%  select(Year=year, Location_Valley = locality , Sublocation = section, Grubbing_PresenceAbsence=grubbing_pa, GrubbingType=grubbing_cat, VegetationType_as_provided = type_of_sites_ecological, site_id) %>% 
  mutate(DataShepherd="Virve") %>% 
  mutate(SpatialScale_GrubbingRecording=0.25) %>% 
  mutate(Survey_type="plots") %>% 
  mutate(Grubbing_Intensity= case_when(GrubbingType == "n" ~ 0,
                                       GrubbingType == "p" ~ 0.05,
                                       GrubbingType == "b" ~ 0.25,
                                       GrubbingType == "c" ~ 0.75)) %>% 
  left_join(coordinates_coat) %>% 
  rename(UTM_E= gps_east, UTM_N =gps_north) %>% 
  filter((!Sublocation == 'bjo')) %>% 
  select(!site_id) %>% 
  mutate(VegetationType_reclassified= case_when(VegetationType_as_provided == "dis" ~ "mesic",
                                                VegetationType_as_provided == "mos" ~ "mesic",
                                                VegetationType_as_provided == "dry" ~ "dry")) %>% 
  mutate(Location_Valley= case_when(Location_Valley == "adv" ~ "Adventdalen",
                                    Location_Valley == "sas" ~ "Sassendalen",
                                    Location_Valley == "bro" ~ "Brogger",
                                    Location_Valley == "alk" ~ "Alkhornet")) %>% 
  mutate(VegetationType_as_provided=as.factor(VegetationType_as_provided))


## ÅSHILD TRANSECTS

transect_ashild_2011 <- read_delim("data/GrubbingDataRaw/transect_ashild_2011.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

transect_ashild_2012 <- read_delim("data/GrubbingDataRaw/transect_ashild_2012.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

transect_ashild_coordinates <- read_delim("data/GrubbingDataRaw/transect_ashild_coordinates.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

transect_2011<-transect_ashild_2011 %>% select(year:p_a) %>% 
  mutate(date=as.factor(date)) %>% 
  mutate(habitat=as.factor(habitat)) %>% 
  rename(Grubbing_PresenceAbsence=p_a) %>% 
  mutate(grub_SPR = replace(grub_SPR , grub_SPR  == 'na', 0)) %>% 
  mutate(grub_SPR=as.numeric(grub_SPR)) %>%  
  mutate(Grubbing_Intensity=grub_SPR/100)


veg_classes<-transect_2011 %>%  select(transect_plot, habitat) %>% 
  unique()
coordinates<-transect_ashild_coordinates  %>%  select(transect_plot, utm_x, utm_y) %>% 
  unique()

transect_2012<-transect_ashild_2012 %>% select(year:grub) %>% 
  mutate(date=as.factor(date)) %>% 
  mutate(habitat=as.factor(habitat)) %>% 
  full_join(veg_classes,  by="transect_plot") %>% 
  select(!habitat.x) %>% 
  rename(habitat=habitat.y) %>% 
  drop_na(year) %>% 
  group_by(year, date, place, transect, plot, transect_plot, frame_no, habitat) %>% 
  summarise(Grubbing_Intensity = mean(grub, na.rm = TRUE),
            Grubbing_PresenceAbsence = max(grub, na.rm = TRUE))

transect_ashild<-bind_rows(transect_2011, transect_2012) %>% 
  left_join(coordinates) %>% 
  drop_na(year) %>% 
  select(Year=year, Date=date, Sublocation=place, UTM_E=utm_x, UTM_N=utm_y, Grubbing_PresenceAbsence, Grubbing_Intensity , VegetationType_as_provided=habitat) %>% 
  mutate(VegetationType_as_provided= case_when(VegetationType_as_provided == "5" ~ "dry",
                                               VegetationType_as_provided == "10" ~ "wet",
                                               VegetationType_as_provided == "11" ~ "mesic",
                                               VegetationType_as_provided == "12" ~ "mesic",
                                               VegetationType_as_provided == "13" ~ "mesic",
                                               VegetationType_as_provided == "14" ~ "dry",
                                               VegetationType_as_provided == "15" ~ "dry",
                                               VegetationType_as_provided == "16" ~ "dry",
                                               VegetationType_as_provided == "17" ~ "dry")) %>% 
  mutate(DataShepherd="Aashild") %>% 
  mutate(SpatialScale_GrubbingRecording=0.25) %>% 
  mutate(Survey_type="plots_in_transects") %>% 
  mutate(Location_Valley= "Adventdalen") %>% 
  select(!Date) %>% 
  mutate(VegetationType_as_provided=as.factor(VegetationType_as_provided)) 


## Bind grubbing datasets toether and save

grubberson<-bind_rows(plots_rene, transect_james, coat_plots, transect_ashild) %>% drop_na(Location_Valley) 



## Environmental data

## May temperatures
PFGDays_01062023 <- read_delim("data/PFGDays_01062023.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

may_temp <- PFGDays_01062023 %>% 
  mutate(temp_positive = ifelse((mean_temp > 0), 1, 0)) %>% 
  mutate(station_name= case_when(station == "SN99840" ~ "Longyearbyen",
                                 station == "SN99910" ~ "New_Aalesund")) %>% 
  group_by(station_name, year) %>% 
  summarise(may_positive_days = sum(temp_positive, na.rm = TRUE))


Location_Valley<-c("Adventdalen", "Alkhornet", "Bohemanflya", "Colesdalen", "Sassendalen", "Semmeldalen", "Brogger")
station_name<-c("Longyearbyen", "Longyearbyen", "Longyearbyen", "Longyearbyen", "Longyearbyen", "Longyearbyen", "New_Aalesund")

weather_match<-as.data.frame(cbind(Location_Valley, station_name))

may_positive_days<-left_join(may_temp, weather_match) %>% rename(Year=year)

## Merge with grubbing file

grubberson<-left_join(grubberson, may_positive_days)



## Export merged file
write_csv(grubberson, file = "data/grubbing_plots_merged.csv")




