ts_2023 <- read_delim("data/GrubbingDataRaw/coat_sv_veg_2023_cleaned_grubbing_transects_ie.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

## Clean data <- this scripts handles the reduced field sheets, first it splits the table by type of disturbance and fills
## out the data for each cm. Then it joins the datasets together

df_crater<-ts_2023 %>% select(site_id=id, start_cm_of_grub, end_cm_of_grub, cratering) %>% 
  filter(cratering != 0) %>% 
  mutate(samples = (end_cm_of_grub - start_cm_of_grub)+1) %>% 
  uncount(samples)%>% 
  unite(site_group, site_id, start_cm_of_grub, sep= "-", remove = FALSE) %>%
  group_by(site_group)%>%
  mutate(count = row_number()) %>% 
  mutate(cm = (start_cm_of_grub + count)-1) %>% 
  ungroup() %>%
  select (site_id, cratering, dist_from_start_cm =cm)


df_bulldozing<-ts_2023 %>% select(site_id=id, start_cm_of_grub, end_cm_of_grub, bulldozing) %>% 
  filter(bulldozing != 0) %>%
  mutate(start_cm_of_grub=ifelse(start_cm_of_grub==0,1, start_cm_of_grub)) %>% 
  mutate(samples = (end_cm_of_grub - start_cm_of_grub)+1) %>% 
  filter((samples > 0) %>% replace_na(TRUE)) %>% 
  mutate_at(c('samples'), ~replace_na(.,1)) %>% 
  uncount(samples) %>% 
  unite(site_group, site_id, start_cm_of_grub, sep= "-", remove = FALSE) %>%
  group_by(site_group)%>% 
  mutate(count = row_number()) %>% 
  mutate(cm = (start_cm_of_grub + count)-1) %>% 
  ungroup() %>%
  select (site_id, bulldozing, dist_from_start_cm =cm)

df_picking<- ts_2023 %>% select(site_id=id, start_cm_of_grub, end_cm_of_grub, picking) %>% 
  filter(picking != 0) %>% 
  mutate (dist_from_start_cm = start_cm_of_grub) %>% 
  select(site_id, picking, dist_from_start_cm)

df_frame <- ts_2023 %>% distinct(id, .keep_all= TRUE) %>% 
  select(id:observer_2) %>%
  rename (site_id = id) %>% 
  mutate(count = 3000) %>% 
  uncount(count) %>% 
  group_by(site_id)%>%
  mutate(dist_from_start_cm = row_number()) %>% 
  ungroup()


ts_2023 <-df_frame %>% left_join(df_crater) %>%
  left_join(df_bulldozing) %>%
  left_join(df_picking) %>% 
  mutate_at(c('cratering', 'bulldozing', 'picking'), ~replace_na(.,0)) %>% 
  mutate(disturbance = case_when(picking > 0 ~ 'p',
                                 bulldozing > 0 ~ 'b',
                                 cratering > 0 ~ 'c')) %>%
  mutate(site_id = factor(site_id)) %>% 
  mutate(disturbance = ifelse(is.na(disturbance), 0, disturbance))%>% 
  mutate(grubbing_pa = case_when(picking > 0 ~ 1,
                                 bulldozing > 0 ~ 1,
                                 cratering > 0 ~ 1)) %>% 
  mutate(grubbing_pa = ifelse(is.na(grubbing_pa), 0, grubbing_pa)) %>% 
  mutate(across(where(is_character),as_factor)) %>% 
  select(-month, -date, -observer_1, -observer_2) %>% 
  mutate(across(c(group_of_sites_spatial, site, year), factor))

## Save file
write_csv(ts_2023, file = "data/GrubbingDataRaw/coat_sv_grubbing_transects_2023.csv")
