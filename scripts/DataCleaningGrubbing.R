## Cleaning script for grubbing data
## Output: A merged file with all grubbing data

## Target column names
## Year, Date, Location_Valley, Sublocation, Survey_type, UTM_E, UTM_N, Datum, UTMZone, 
## DataShepherd, Grubbing_PresenceAbsence, Grubbing_Intensity, GrubbingType, SpatialScale_GrubbingRecording, 
## VegetationType_as_provided, VegetationType_Reclassified)

## Load Libraries

library(tidyverse)
library(sf)
library(tidyr)

## RENE

Plots_alldata_COATandnonCOAT <- read_delim("data/GrubbingDataRaw/Plots_alldata_COATandnonCOAT.txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE)

Valleywalkplots_with2019andcoordinates <- read_delim("data/GrubbingDataRaw/Valleywalkplots_with2019andcoordinates.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Largeplotswithvalleynames_forupload <- read_delim("data/GrubbingDataRaw/Largeplotswithvalleynames_forupload.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

plots_rene<-Largeplotswithvalleynames_forupload %>% 
  mutate(Veg_type=as.character(Veg_type)) %>% 
  mutate(Veg_type2=as.character(Veg_type2))%>%
  select(!Patch_size_bare)%>%
  rename(Plot_type=Plot_Type) %>% 
  bind_rows(Valleywalkplots_with2019andcoordinates)%>% 
  filter(!(Recorder == 'LEL' & Valley=="Reindalen")) %>% 
  select(Year=Year, Date=Date, Sublocation=Valley, Survey_type=Plot_type, UTM_E=GPS_X, UTM_N=GPS_Y, Datum=Coordinate_system, Grubbing_PresenceAbsence=Grubbing_present, Grubbing_Intensity=Area_impacted, VegetationType_as_provided=Veg_type_reduced) %>% 
#  filter((!Survey_type == 'COAT')) %>% #Remove COAT data -loaded from seperate file
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
                                      Sublocation == "Semmeldalen"|Sublocation == "Skiferdalen"|Sublocation == "Stuttdalen"|
                                      Sublocation == "Synndalen" ~ "Semmeldalen", Sublocation == "Bodalen"|Sublocation == "Colesdalen" ~ "Colesdalen",
                                      Sublocation == "Reindalen" ~ "Reindalen", Sublocation == "Fardalen" ~ "Fardalen")) %>% 
  select(!Date) %>% 
  mutate(VegetationType_as_provided=as.factor(VegetationType_as_provided)) %>% 
  drop_na(VegetationType_reclassified)


#Convert ED1950 to WGS84
rene_ed<-plots_rene %>% filter(Datum == 'ED1950') %>% 
  mutate(UTM_E=as.numeric(UTM_E)) %>% drop_na(UTM_E) %>% 
  st_as_sf(coords = c("UTM_E", "UTM_N"), crs = 23033) %>% 
  st_transform(32633) %>% 
  dplyr::mutate(UTM_E = sf::st_coordinates(.)[,1],
                UTM_N= sf::st_coordinates(.)[,2]) %>% st_drop_geometry()

#Join back to dataset
plots_rene<-plots_rene %>% 
  filter(!Datum == 'ED1950') %>%
  bind_rows(rene_ed) %>% 
  select(!Datum)



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


james_ed<-transect_james %>% filter(Datum == 'ED50') %>% 
  st_as_sf(coords = c("UTM_E", "UTM_N"), crs = 23033) %>% 
  st_transform(32633) %>% 
  dplyr::mutate(UTM_E = sf::st_coordinates(.)[,1],
                UTM_N= sf::st_coordinates(.)[,2]) %>% st_drop_geometry()

#Join back to dataset
transect_james<-transect_james %>% 
  filter(!Datum == 'ED50') %>%
  bind_rows(james_ed) %>% 
  select(!Datum) %>% 
  drop_na(VegetationType_reclassified)


## COAT PLOTS

coat_grubbing_plot_2017_2022_ie <- read_csv("data/GrubbingDataRaw/coat_grubbing_plot_2017_2022_ie.csv")
coat_grubbing_plot_2023<-read_table("data/GrubbingDataRaw/coat_sv_veg_2023_site_plots_grubbing_variable_to_IE_05102023.txt")

pf_2022 <- read_delim("data/GrubbingDataRaw/coat_sv_veg_2022_site_plots.txt", delim = "\t", escape_double = FALSE,trim_ws = TRUE)
coordinates_coat<-pf_2022 %>% select(site_id, gps_east, gps_north) %>% 
  unique()

coat_plots_2023 <-coat_grubbing_plot_2023 %>% 
  mutate(grubbing_cat = ifelse(is.na(grubbing_plot), "n", grubbing_plot)) %>%
  mutate(Grubbing_Intensity= case_when(grubbing_cat == "n" ~ 0,
                                       grubbing_cat == "p" ~ 0.05,
                                       grubbing_cat == "b" ~ 0.25,
                                       grubbing_cat == "c" ~ 0.75)) %>% 
  mutate(grubbing_pa = ifelse((Grubbing_Intensity > 0), 1, 0)) %>% 
  select(!Grubbing_Intensity) %>% 
  select(!grubbing_plot)


coat_plots <- coat_grubbing_plot_2017_2022_ie %>%  bind_rows(coat_plots_2023) %>% 
  select(Year=year, Location_Valley = locality , Sublocation = section, Grubbing_PresenceAbsence=grubbing_pa, GrubbingType=grubbing_cat, VegetationType_as_provided = type_of_sites_ecological, site_id) %>% 
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
  mutate(VegetationType_reclassified= case_when(VegetationType_as_provided == "5" ~ "dry",
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


## VIRVES 2015 VEGETATION TRANSECTS
vegetation_transect_virve_2015 <- read_delim("data/GrubbingDataRaw/vegetation_transect_virve_2015.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

virve_veg_2015<-vegetation_transect_virve_2015 %>% 
  select(UTM_E=x_coord, UTM_N=y_coord, Sublocation=locality, picking, bulldozing, VegetationType_as_provided = vegetation_type) %>% 
  mutate(VegetationType_as_provided = factor(VegetationType_as_provided)) %>% 
  filter(!(VegetationType_as_provided=='bare_ground'|VegetationType_as_provided=='crust'|
             VegetationType_as_provided == "dried_moss" |VegetationType_as_provided == "flooded_flats"|
             VegetationType_as_provided == "grass" |VegetationType_as_provided == "gravel"|
             VegetationType_as_provided == "gravel_ridge" |VegetationType_as_provided == "gully"|
             VegetationType_as_provided == "mud" |VegetationType_as_provided == "open_patches"|
             VegetationType_as_provided == "pond" |VegetationType_as_provided == "ridge"|
             VegetationType_as_provided == "river" |VegetationType_as_provided == "river_bed"|
             VegetationType_as_provided == "river_mud" |VegetationType_as_provided == "rock"|
             VegetationType_as_provided == "sand" |VegetationType_as_provided == "sandbed"|
             VegetationType_as_provided == "stream")) %>%
  mutate(VegetationType_reclassified= case_when(VegetationType_as_provided == "cassiope" ~ "dry",
                                               VegetationType_as_provided == "dryas_established" ~ "dry",
                                               VegetationType_as_provided == "dryas_exposed" ~ "dry",
                                               VegetationType_as_provided == "heath" ~ "dry",
                                               VegetationType_as_provided == "heath_exposed" ~ "dry",
                                               VegetationType_as_provided == "mild_snow_bed" ~ "mesic",
                                               VegetationType_as_provided == "moss" ~ "mesic",
                                               VegetationType_as_provided == "moss_tundra" ~ "mesic",
                                               VegetationType_as_provided == "snow_bed" ~ "mesic",
                                               VegetationType_as_provided == "tundra_mire" ~ "mesic",
                                               VegetationType_as_provided == "wetland" ~ "wet")) %>% 
  mutate(DataShepherd="Virve") %>% 
  mutate(SpatialScale_GrubbingRecording=2) %>% 
  mutate(Survey_type="plots_on_transect") %>% 
  mutate(Location_Valley= case_when(Sublocation == "adventdalen" ~ "Adventdalen",
                                    Sublocation == "alkehornet" ~ "Alkhornet",
                                    Sublocation == "bjorndalen" ~ "Adventdalen",
                                    Sublocation == "bohemanflya" ~ "Bohemanflya",
                                    Sublocation == "brogger" ~ "Brogger",
                                    Sublocation == "colesdalen" ~ "Colesdalen",
                                    Sublocation == "dicksonfjorden" ~ "Dicksonfjorden",
                                    Sublocation == "kaffioyra" ~ "Kaffioyra",
                                    Sublocation == "kongsfjorden" ~ "Kongsfjorden",
                                    Sublocation == "sarsoyra" ~ "Sarsoyra",
                                    Sublocation == "sassendalen" ~ "Sassendalen")) %>% 
  mutate(picking = ifelse((picking > 0), 0.05, 0)) %>% 
  mutate(bulldozing = ifelse((bulldozing > 0), 0.25, 0)) %>% 
  rowwise() %>%
  mutate(Grubbing_Intensity=max(picking,bulldozing)) %>% 
  mutate(Grubbing_PresenceAbsence = ifelse((Grubbing_Intensity > 0), 1, 0)) %>% 
  select(!(picking:bulldozing)) %>% 
  mutate(Year=2015) %>% 
  drop_na(Grubbing_PresenceAbsence)

## VIRVES 2015 PF

virve_veginfo<- virve_veg_2015 %>% 
  select(UTM_E, UTM_N, VegetationType_as_provided, VegetationType_reclassified, Location_Valley, DataShepherd) 

point_frequency_2015 <- read_delim("data/GrubbingDataRaw/point_frequency_2015.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)


virve_pf_2015<-point_frequency_2015 %>% 
  select(UTM_E=coords.x1, UTM_N=coords.x2, Sublocation=locality, picking, bulldozing, plant_part) %>%
  filter(plant_part=="veg") %>% 
  mutate(UTM_E=as.numeric(UTM_E)) %>% 
  mutate(UTM_N=as.numeric(UTM_N)) %>% 
  left_join(virve_veginfo, join_by(UTM_E, UTM_N)) %>% 
  drop_na(VegetationType_as_provided) %>% 
  mutate(SpatialScale_GrubbingRecording=0.25) %>% 
  mutate(Survey_type="plots") %>% 
  mutate(picking = ifelse((picking > 0), 0.05, 0)) %>% 
  mutate(bulldozing = ifelse((bulldozing > 0), 0.25, 0)) %>% 
  rowwise() %>%
  mutate(Grubbing_Intensity=max(picking,bulldozing)) %>% 
  mutate(Grubbing_PresenceAbsence = ifelse((Grubbing_Intensity > 0), 1, 0)) %>% 
  select(!(picking:bulldozing)) %>% 
  mutate(Year=2015) %>% 
  select(!plant_part)
  

  
  
## VIRVE 2016 VEGETATION PLOTS
plots_2016_grubbing_COAT_Virve_Ravolainen_14092023 <- read_delim("data/GrubbingDataRaw/plots_2016_grubbing_COAT_Virve_Ravolainen_14092023.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
virve_veg_2016 <- plots_2016_grubbing_COAT_Virve_Ravolainen_14092023 %>% 
  select(UTM_E=gps_east, UTM_N=gps_north, Sublocation=locality, Grubbing_PresenceAbsence= grubbing_plot, VegetationType_as_provided = habitat_type) %>% 
  mutate(Grubbing_PresenceAbsence = as.numeric(replace(Grubbing_PresenceAbsence , Grubbing_PresenceAbsence  == 'p', 1))) %>% 
  mutate(Location_Valley= case_when(Sublocation == "adventdalen" ~ "Adventdalen",
                                    Sublocation == "alkhornet" ~ "Alkhornet",
                                    Sublocation == "bjoerndalen" ~ "Adventdalen",
                                    Sublocation == "bohemanflya" ~ "Bohemanflya",
                                    Sublocation == "broegger" ~ "Brogger",
                                    Sublocation == "engelsbukta" ~ "Brogger",
                                    Sublocation == "gaasebu" ~ "Brogger",
                                    Sublocation == "janssonhaugen" ~ "Adventdalen",
                                    Sublocation == "simlestupet" ~ "Brogger",
                                    Sublocation == "sassendalen" ~ "Sassendalen",
                                    Sublocation == "trygghamna" ~ "Alkhornet")) %>% 
  mutate(DataShepherd="Virve") %>% 
  mutate(SpatialScale_GrubbingRecording=0.25) %>% 
  mutate(Survey_type="plots") %>% 
  mutate(VegetationType_reclassified= case_when(VegetationType_as_provided == "seabird" ~ "mesic",
                                                VegetationType_as_provided == "non_seabird" ~ "mesic",
                                                VegetationType_as_provided == "dryas" ~ "dry")) %>% 
  mutate(Year=2016) %>% 
  drop_na(Grubbing_PresenceAbsence) %>% 
  drop_na(UTM_E)




## COAT TRANSECT
masterfile_coat_grubbing_transect_2017_2022_ie <- read_csv("data/GrubbingDataRaw/masterfile_coat_grubbing_transect_2017_2022_ie.csv")
ts_2023 <- read_delim("data/GrubbingDataRaw/coat_sv_grubbing_transects_2023.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)

grubbing_transect_coordinates_coat <- read_delim("data/GrubbingDataRaw/grubbing_transect_coordinates_coat.txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE)

grubbing_transect_coordinates_coat <- grubbing_transect_coordinates_coat %>% filter(point=='START')


coat_transect<- masterfile_coat_grubbing_transect_2017_2022_ie %>% 
  bind_rows(ts_2023) %>% 
#  distinct(year, site_id, .keep_all = T) %>% 
  left_join(grubbing_transect_coordinates_coat) %>% 
  select(Year=year, Location_Valley = locality , Sublocation = section, VegetationType_as_provided = type_of_sites_ecological, site_id, UTM_E=x_coord, UTM_N=y_coord, dist_from_start_cm, grubbing_pa, disturbance) %>% 
  mutate(DataShepherd="Virve") %>% 
  filter((!Sublocation == 'bjo')) %>%
  mutate(VegetationType_reclassified= case_when(VegetationType_as_provided == "dis" ~ "mesic",
                                                VegetationType_as_provided == "mos" ~ "mesic",
                                                VegetationType_as_provided == "dry" ~ "dry")) %>% 
  mutate(Location_Valley= case_when(Location_Valley == "adv" ~ "Adventdalen",
                                    Location_Valley == "sas" ~ "Sassendalen",
                                    Location_Valley == "bro" ~ "Brogger",
                                    Location_Valley == "alk" ~ "Alkhornet")) %>% 
  select(!site_id) %>% 
  drop_na(UTM_N) %>% 
  drop_na(disturbance) %>% 
  group_by(Year, Location_Valley, Sublocation, VegetationType_as_provided, UTM_E, UTM_N, VegetationType_reclassified) %>% 
  summarise(Grubbing_Intensity=sum(grubbing_pa/3000)) %>%
  mutate(Grubbing_PresenceAbsence = ifelse((Grubbing_Intensity > 0), 1, 0)) %>% 
  mutate(SpatialScale_GrubbingRecording=0.3) %>% 
  mutate(Survey_type="transect")



## JESPER TRANSECT
transect_jesper_temp <- read_delim("data/GrubbingDataRaw/transect_jesper_temp.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

jesper_transect_grubbing <- read_delim("data/GrubbingDataRaw/jesper_transect_grubbing.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

jesper_transect_length <- read_delim("data/GrubbingDataRaw/jesper_transect_length.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

jesper_all_locations <- read_delim("data/GrubbingDataRaw/jesper_all_locations.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

extra<-data.frame("Gaset5", 2022, 0, 24.0)
names(extra)<-c("Plot_Nr","Year", "Grubbing_Intensity", "Length")

transect_jesper <- transect_jesper_temp %>% 
st_as_sf(coords = c("UTM_E", "UTM_N"), crs = 4326) %>% 
  st_transform(32633) %>% 
  dplyr::mutate(UTM_E = sf::st_coordinates(.)[,1],
                UTM_N= sf::st_coordinates(.)[,2]) %>% st_drop_geometry()

jesper_transect<-jesper_transect_grubbing %>% 
  drop_na(start_cm_of_grub) %>% 
  mutate(grubbing = (end_cm_of_grub - start_cm_of_grub)+0.01) %>%
  group_by(Plot_Nr, Year) %>% 
  summarise(Grubbing_Intensity=sum(grubbing)) %>% 
  ungroup() %>% 
  left_join(jesper_transect_length) %>% 
  mutate(Grubbing_Intensity=Grubbing_Intensity/Length) %>% 
  bind_rows(extra) %>% 
  mutate(SpatialScale_GrubbingRecording=Length/100) %>% 
  mutate(Grubbing_PresenceAbsence = ifelse((Grubbing_Intensity > 0), 1, 0)) %>% 
  mutate(Survey_type="transect") %>% 
  mutate(DataShepherd="Jesper") %>%
  select(-Length) %>% 
  mutate(VegetationType_reclassified="wet") %>% 
  select(-Plot_Nr)




## Bind grubbing datasets together and save

#snow_coordinates<-bind_rows(plots_rene, transect_james, coat_plots, transect_ashild, virve_veg_2015, virve_veg_2016, transect_jesper, coat_transect) %>% 
#  drop_na(Location_Valley) %>% drop_na(UTM_E) %>% 
#  distinct(Year, UTM_N, UTM_E, .keep_all = T) %>% 
#  select(Year, UTM_E, UTM_N, Location_Valley) %>% 
#  rowid_to_column("ID")

#write_csv(snow_coordinates, file = "data/snow_coordinates.csv")




## Environmental data

## May temperatures
PFGDays_01062023 <- read_delim("data/PFGDays_01062023.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

may_temp <- PFGDays_01062023 %>% 
  mutate(temp_positive = ifelse((mean_temp > 0), 1, 0)) %>% 
  mutate(station_name= case_when(station == "SN99840" ~ "Longyearbyen",
                                 station == "SN99910" ~ "New_Aalesund")) %>% 
  group_by(station_name, year) %>% 
  summarise(may_positive_days = sum(temp_positive, na.rm = TRUE))


Location_Valley<-c("Adventdalen", "Alkhornet", "Bohemanflya", "Colesdalen", "Sassendalen", "Semmeldalen", "Dicksonfjorden", "Fardalen", "Reindalen", "Brogger",  "Kaffioyra", "Sarsoyra", "Kongsfjorden")
station_name<-c("Longyearbyen", "Longyearbyen", "Longyearbyen", "Longyearbyen", "Longyearbyen", "Longyearbyen", "Longyearbyen","Longyearbyen","Longyearbyen","New_Aalesund", "New_Aalesund", "New_Aalesund", "New_Aalesund")

weather_match<-as.data.frame(cbind(Location_Valley, station_name))

may_positive_days<-left_join(may_temp, weather_match) %>% rename(Year=year)

snow_coordinates <- read_csv("data/snow_coordinates.csv")


## Snowfree days
SM_snowfree_DOY <- read_csv("data/SM_snowfree_DOY_points_2005-2021.csv", col_names=FALSE)

snowmelt <-SM_snowfree_DOY %>% 
  column_to_rownames("X1") %>% 
  t %>% 
  as.data.frame %>% 
  rownames_to_column("ID_old") %>% 
  bind_cols(snow_coordinates) %>% 
  select(-ID_old) %>% 
  pivot_longer('2005':'2021', names_to = "Year_pivot", values_to = "snowmelt") %>% 
  mutate(Year_diff=Year-as.numeric(Year_pivot)) %>% 
  filter(Year_diff==0) %>% 
  filter(snowmelt>0) %>% 
  select(ID, Year, UTM_E, UTM_N,snowmelt) %>% 
  mutate(across(where(is.numeric), round, 2))   ##needed to round coordinates for join to work

##Tair
SM_tair <- read_csv("data/SM_tair_points_MJJ_2005-2021.csv", col_names=FALSE) 

air<-SM_tair%>% 
  filter(X2==5) %>% 
  pivot_longer(X4:X28640, names_to = "ID_old", values_to = "temp") %>% 
  mutate(temp =na_if(temp, -9999)) %>% 
  group_by(X1,ID_old) %>% 
  summarise(positive=sum(temp>0)) %>% 
  pivot_wider(names_from = "X1", values_from = "positive") %>% 
  mutate(across(c('ID_old'), substr, 2, nchar(ID_old))) %>% 
  mutate(ID=as.numeric(ID_old)) %>% 
  mutate(ID=ID-3) %>% 
  select(!ID_old) %>% 
  pivot_longer('2005':'2021', names_to = "Year", values_to = "mayday") %>% 
  mutate(Year=as.numeric(Year)) 


may_positive_days_model<-snow_coordinates %>% 
  left_join(air) %>% 
  mutate(across(where(is.numeric), round, 2))



## Add all files and merge with environmental data
grubberson<-bind_rows(plots_rene, transect_james, coat_plots, transect_ashild, virve_veg_2015, virve_veg_2016, virve_pf_2015, coat_transect, jesper_transect) %>% 
  drop_na(Location_Valley) %>% drop_na(UTM_E) %>%
  mutate(across(where(is.numeric), round, 2)) %>% 
  left_join(snowmelt, join_by(Year, UTM_E, UTM_N)) %>% 
  left_join(may_positive_days_model, join_by(Year, UTM_E, UTM_N)) %>% 
  select(!UTMZone) %>% 
  select(!GrubbingType) %>% 
  select(!(ID.x)) %>% 
  select(!(ID.y)) %>% 
  mutate(Location_Valley=Location_Valley.x) %>% 
  select(!(Location_Valley.x)) %>% 
  select(!(Location_Valley.y))


## Export merged file
write_csv(grubberson, file = "data/grubbing_plots_merged.csv")




