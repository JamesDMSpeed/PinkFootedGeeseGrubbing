## Cleaning script for grubbing data
## Output: A merged file with all grubbing data

## Target column names
## Year, Date, Location_Valley, Sublocation, Survey_type, UTM_E, UTM_N, Datum, UTMZone, 
## DataShepherd, Grubbing_PresenceAbsence, Grubbing_Intensity, GrubbingType, SpatialScale_GrubbingRecording, 
## VegetationType_as_provided, VegetationType_Reclassified)

## Load Libraries

library(tidyverse)
library(sf)

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

#Add a dummy dataset for 2023
coat_plots_2023 <- coat_plots %>% 
  filter(Year==2022) %>% 
  mutate(Year=2023) %>% 
  bind_rows(coat_plots)

coat_plots<-coat_plots_2023


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
  mutate(SpatialScale_GrubbingRecording=0.25) %>% 
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
                                    Sublocation == "simlestupet" ~ "Simlestupet",
                                    Sublocation == "sassendalen" ~ "Sassendalen",
                                    Sublocation == "trygghamna" ~ "Trygghamna")) %>% 
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

grubbing_transect_coordinates_coat <- read_delim("data/GrubbingDataRaw/grubbing_transect_coordinates_coat.txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE)

coat_transect<- masterfile_coat_grubbing_transect_2017_2022_ie %>% 
  distinct(year, site_id, .keep_all = T) %>% 
  left_join(grubbing_transect_coordinates_coat) %>% 
  select(Year=year, Location_Valley = locality , Sublocation = section, VegetationType_as_provided = type_of_sites_ecological, site_id, UTM_E=x_coord, UTM_N=y_coord) %>% 
  mutate(DataShepherd="Virve") %>% 
  mutate(VegetationType_reclassified= case_when(VegetationType_as_provided == "dis" ~ "mesic",
                                                VegetationType_as_provided == "mos" ~ "mesic",
                                                VegetationType_as_provided == "dry" ~ "dry")) %>% 
  mutate(Location_Valley= case_when(Location_Valley == "adv" ~ "Adventdalen",
                                    Location_Valley == "sas" ~ "Sassendalen",
                                    Location_Valley == "bro" ~ "Brogger",
                                    Location_Valley == "alk" ~ "Alkhornet")) %>% 
  filter((!Sublocation == 'bjo')) %>% 
  select(!site_id) %>% 
  drop_na(UTM_N)

#Add a dummy dataset for 2023
coat_transect_2023 <- coat_transect %>% 
  filter(Year==2022) %>% 
  mutate(Year=2023) %>% 
  bind_rows(coat_transect)

coat_plots<-coat_plots_2023

## JESPER TRANSECT
transect_jesper_temp <- read_delim("data/GrubbingDataRaw/transect_jesper_temp.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

transect_jesper <- transect_jesper_temp %>% 
st_as_sf(coords = c("UTM_E", "UTM_N"), crs = 4326) %>% 
  st_transform(32633) %>% 
  dplyr::mutate(UTM_E = sf::st_coordinates(.)[,1],
                UTM_N= sf::st_coordinates(.)[,2]) %>% st_drop_geometry()




## Bind grubbing datasets together and save

snow_coordinates<-bind_rows(plots_rene, transect_james, coat_plots, transect_ashild, virve_veg_2015, virve_veg_2016, transect_jesper, coat_transect) %>% 
  drop_na(Location_Valley) %>% drop_na(UTM_E) %>% 
  distinct(Year, UTM_N, UTM_E, .keep_all = T) %>% 
  select(Year, UTM_E, UTM_N, Location_Valley) %>% 
  rowid_to_column("ID")

write_csv(snow_coordinates, file = "data/snow_coordinates.csv")



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

## Add all files and merge with environmental data
grubberson<-bind_rows(plots_rene, transect_james, coat_plots, transect_ashild, virve_veg_2015, virve_veg_2016) %>% 
  drop_na(Location_Valley) %>% drop_na(UTM_E) %>% 
  left_join(may_positive_days) %>% 
  select(!UTMZone) %>% 
  select(!GrubbingType)


## Export merged file
write_csv(grubberson, file = "data/grubbing_plots_merged.csv")




