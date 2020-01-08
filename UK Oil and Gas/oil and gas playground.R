##Jack Hasler
#4/1/2019
#UK Oil and Gas Data Exploration

#Data from: 
#https://data-ogauthority.opendata.arcgis.com/datasets/oga-field-production-pprs-wgs84

library(tidyverse)
library(readr)
library(countrycode)
library(wbstats)
library(haven)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(shiny)
library(stringr)
library(rgdal)
library(sp)
oil_raw_data <- read_csv("OGA_Field_Production_PPRS_WGS84 (1).csv")



sorted_oil <- oil_raw_data %>%
  group_by(PERIODDATE) %>%
  summarise(oil_mass = sum(OILPRODMAS, na.rm = TRUE),
            oil_vol = sum(OILPRODM3, na.rm = TRUE),
            drygas_mass = sum(as.numeric(DGASPRODMA), na.rm = TRUE),
            drygas_vol = sum(as.numeric(DGASPROKSM), na.rm = TRUE),
            assocgas_mass = sum(AGASPRODMA, na.rm = TRUE),
            assocgas_vol = sum(AGASPROKSM, na.rm = TRUE),
            condgas_mass = sum(as.numeric(GCONDMASS), na.rm = TRUE),
            condgas_vol = sum(as.numeric(GCONDVOL), na.rm = TRUE)
            ) %>%
  arrange(PERIODDATE)


energyprod <- wb(indicator = 'EG.ELC.FOSL.ZS')

energyprod <- energyprod %>%
  filter(iso3c == 'GBR') %>%
  mutate(Year = as.integer(date)) %>%
  filter(Year >= 1975)

energyprod$date <- as.Date(energyprod$date, format = '%Y')

sorted_oil <- merge(sorted_oil, energyprod, by.x = 'PERIODDATE', by.y = 'date', all = TRUE)

ggplot(sorted_oil) + 
  geom_point(aes(PERIODDATE,oil_vol/100000, color = 'Oil Volume (per 100,000 m3)')) +
  geom_point(aes(PERIODDATE,drygas_vol/100000, color = 'Dry Natural Gas Volume (per 100,000 ksm3)')) +
  geom_point(aes(PERIODDATE,assocgas_vol/100000, color = 'Associated Natural Gas Volume (per 100,000 ksm3)')) +
  geom_smooth(aes(PERIODDATE, value, color = 'Perecent of Total Energy from Oil, Gas, and Coal')) + 
  scale_color_manual(name = '', values = c('Oil Volume (per 100,000 m3)' = 'Red', 'Dry Natural Gas Volume (per 100,000 ksm3)' = 'Blue', 'Associated Natural Gas Volume (per 100,000 ksm3)' = 'Green', 'Perecent of Total Energy from Oil, Gas, and Coal' = 'Black')) + 
  xlab('Year') + ylab("")


APE <- read_sav('./UKDA-7404-spss/spss/spss14/audit_of_political_engagement_1-10_data.sav')

sorted_oil_2 <- oil_raw_data %>%
  group_by(PERIODYR, FIELDNAME, SHAPESTArea, SHAPESTLength) %>%
  summarise(oil_mass = sum(OILPRODMAS, na.rm = TRUE),
            oil_vol = sum(OILPRODM3, na.rm = TRUE),
            drygas_mass = sum(as.numeric(DGASPRODMA), na.rm = TRUE),
            drygas_vol = sum(as.numeric(DGASPROKSM), na.rm = TRUE),
            assocgas_mass = sum(AGASPRODMA, na.rm = TRUE),
            assocgas_vol = sum(AGASPROKSM, na.rm = TRUE),
            condgas_mass = sum(as.numeric(GCONDMASS), na.rm = TRUE),
            condgas_vol = sum(as.numeric(GCONDVOL), na.rm = TRUE)
  ) %>%
  arrange(FIELDNAME,PERIODYR)

##
oil_spatial <- shapefile('./OGA_Field_Production_PPRS_WGS84/OGA_Field_Production_PPRS_WGS84.shp')

oil_sf <- st_as_sf(oil_spatial)


spatial_uk <- readOGR('./NUTS_Level_1_January_2018_Generalised_Clipped_Boundaries_in_the_United_Kingdom')
spatial_uk <- st_as_sf(spatial_uk)

APE_select <- APE %>%
  dplyr::select(Wave,UniqueID,respnum,q1,q2,q4a_14,q4a_15,q5_4,q5_5,q5_6,q5_7,QB5B_07,QB5B_09,q6,q6_b1,q6_b2,q6_b3,q7_01,
         q7_05,QB5_12,q8,q12_5,QB1,QB6_3,q16_1,q16_2,q16_3,q16_4,q16_8,q16_9,q22_03,q22_04,q23_03,q23_04,QB3_4,
         gor,gender,age,work,marr,ethn,ethny,numhh,income,qual,urban,numcar,intacc) %>%
  group_by(gor,Wave) %>%
  summarise_all(mean)

new_ape <- merge(spatial_uk,APE_select, by.x = 'objectid', by.y = 'gor')

ggplot() + 
  geom_sf(new_ape, color = age)
+ 
  facet_wrap(Wave)

oil_sf <- oil_sf %>%
  group_by(PERIODYRMN,FIELDNAME) %>%
  summarise(oil_mass = sum(OILPRODMAS, na.rm = TRUE),
            oil_vol = sum(OILPRODM3, na.rm = TRUE),
            drygas_mass = sum(as.numeric(DGASPRODMA), na.rm = TRUE),
            drygas_vol = sum(as.numeric(DGASPROKSM), na.rm = TRUE),
            assocgas_mass = sum(AGASPRODMA, na.rm = TRUE),
            assocgas_vol = sum(AGASPROKSM, na.rm = TRUE),
            condgas_mass = sum(as.numeric(GCONDMASS), na.rm = TRUE),
            condgas_vol = sum(as.numeric(GCONDVOL), na.rm = TRUE))

oil_sf_2000 <- oil_sf %>%
  filter(PERIODYRMN == '200001')

tmap_arrange(
  tm_shape(oil_sf_2000) + tm_fill('oil_vol'),
  tm_shape(oil_sf_2000) + tm_fill('assocgas_vol'),
  tm_shape(oil_sf_2000) + tm_fill('drygas_vol')
)

test <- tm_shape(oil_sf) + tm_fill('oil_vol') + tm_facets(along = 'PERIODYRMN')
tmap_animation(test, filename = "test.gif", delay = 25)

oil_foritfied <- fortify(oil_spatial)
#####################




test <- tm_shape(oil_spatial) + tm_polygons() + 
  tm_facets(along = 'PERIODDATE', free.coords = F)


######################







wvs_survey <- readRDS("D:/jhasl/Google Drive/Active Papers/UK Oil and Gas/F00008390-WVS_Longitudinal_1981_2016_r_v20180912.rds")

wvs_survey <- wvs_survey %>%
  select(S003,S012,S020,S023,B001,B002,B003,B004,B005,B006,B007,B008,B009,B010,B011,B012,B013,B014,B015,B016,B017,B018,B019,B020,B021,B022,B023,B024,B025,B026,B027,B028,B029,B030,B031)


wvs_avg <- wvs_survey %>%
  filter(S012 != -36)

wvs_avg$B001 <- ifelse(wvs_avg$B001 < 0,NA,wvs_avg$B001)
wvs_avg$B002 <- ifelse(wvs_avg$B002 < 0,NA,wvs_avg$B002)
wvs_avg$B003 <- ifelse(wvs_avg$B003 < 0,NA,wvs_avg$B003)
wvs_avg$B004 <- ifelse(wvs_avg$B004 < 0,NA,wvs_avg$B004)
wvs_avg$B005 <- ifelse(wvs_avg$B005 < 0,NA,wvs_avg$B005)
wvs_avg$B006 <- ifelse(wvs_avg$B006 < 0,NA,wvs_avg$B006)
wvs_avg$B007 <- ifelse(wvs_avg$B007 < 0,NA,wvs_avg$B007)
wvs_avg$B008 <- ifelse(wvs_avg$B008 < 0,NA,wvs_avg$B008)
wvs_avg$B009 <- ifelse(wvs_avg$B009 < 0,NA,wvs_avg$B009)
wvs_avg$B010 <- ifelse(wvs_avg$B010 < 0,NA,wvs_avg$B010)
wvs_avg$B011 <- ifelse(wvs_avg$B011 < 0,NA,wvs_avg$B011)
wvs_avg$B012 <- ifelse(wvs_avg$B012 < 0,NA,wvs_avg$B012)
wvs_avg$B013 <- ifelse(wvs_avg$B013 < 0,NA,wvs_avg$B013)
wvs_avg$B014 <- ifelse(wvs_avg$B014 < 0,NA,wvs_avg$B014)
wvs_avg$B015 <- ifelse(wvs_avg$B015 < 0,NA,wvs_avg$B015)
wvs_avg$B016 <- ifelse(wvs_avg$B016 < 0,NA,wvs_avg$B016)
wvs_avg$B017 <- ifelse(wvs_avg$B017 < 0,NA,wvs_avg$B017)
wvs_avg$B018 <- ifelse(wvs_avg$B018 < 0,NA,wvs_avg$B018)
wvs_avg$B019 <- ifelse(wvs_avg$B019 < 0,NA,wvs_avg$B019)
wvs_avg$B020 <- ifelse(wvs_avg$B020 < 0,NA,wvs_avg$B020)
wvs_avg$B021 <- ifelse(wvs_avg$B021 < 0,NA,wvs_avg$B021)
wvs_avg$B022 <- ifelse(wvs_avg$B022 < 0,NA,wvs_avg$B022)
wvs_avg$B023 <- ifelse(wvs_avg$B023 < 0,NA,wvs_avg$B023)
wvs_avg$B024 <- ifelse(wvs_avg$B024 < 0,NA,wvs_avg$B024)
wvs_avg$B025 <- ifelse(wvs_avg$B025 < 0,NA,wvs_avg$B025)
wvs_avg$B026 <- ifelse(wvs_avg$B026 < 0,NA,wvs_avg$B026)
wvs_avg$B027 <- ifelse(wvs_avg$B027 < 0,NA,wvs_avg$B027)
wvs_avg$B028 <- ifelse(wvs_avg$B028 < 0,NA,wvs_avg$B028)
wvs_avg$B029 <- ifelse(wvs_avg$B029 < 0,NA,wvs_avg$B029)
wvs_avg$B030 <- ifelse(wvs_avg$B030 < 0,NA,wvs_avg$B030)
wvs_avg$B031 <- ifelse(wvs_avg$B031 < 0,NA,wvs_avg$B031)

wvs_avg$date <- as.Date(as.character(wvs_avg$S012), format = '%Y%m%d')
wvs_avg$country <- countrycode(wvs_avg$S003, origin = 'iso3n', destination= 'country.name')

wvs_avg1 <- wvs_avg %>%
  group_by(S020, country) %>%
  summarise_all(funs(mean(., na.rm = T)))

wvs_avg_temp <- wvs_avg1 %>%
  filter(!is.na(B008) & country == 'Norway')

ggplot(wvs_avg_temp) +
  geom_line(aes(S020,B008, color = country))

