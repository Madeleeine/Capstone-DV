library(shiny)
library(shinydashboard)
library(lubridate)
library(dplyr)
library(stringr)
library(plotly)
library(ggplot2)
library(readr)
library(leaflet)
library(countrycode)
library(tidyr)
library(ggthemes)
library(rgdal)
library(maps)
library(ggmap)
library(dashboardthemes)

wheat<-read_csv("Wheat.csv")
maize<-read_csv("Maize.csv")
rice<-read_csv("Rice.csv")
soybean<-read_csv("Soybean.csv")
wheat<-wheat %>% 
  select(-`Flag Codes`,-FREQUENCY)
maize<-maize %>% 
  select(-`Flag Codes`,-FREQUENCY)
rice<-rice %>% 
  select(-`Flag Codes`,-FREQUENCY)
soybean<-soybean %>% 
  select(-`Flag Codes`,-FREQUENCY)
df_crop<-rbind(wheat,maize,rice,soybean)
df_crop<-df_crop %>% 
  mutate(SUBJECT=sapply(SUBJECT,str_to_title)) %>% 
  mutate(MEASURE = sub("THND_TONNE","Thousand Tonnnes",MEASURE)) %>%
  mutate(INDICATOR = sub("CROPYIELD","Crop Yield",INDICATOR)) %>% 
  rename(Country=LOCATION,
         Year=TIME,
         Indicator=INDICATOR,
         Measure=MEASURE,
         Subject=SUBJECT,
         Total=Value
  )
distinct_country<-df_crop_iso %>% distinct(Country)


df_crop_index<-df_crop %>% 
  filter(Country %in% c("BRICS","WLD","OECD")) %>% 
  mutate(Country=ifelse(Country=="WLD","Worldwide",Country))
df_crop$Country<-countrycode(df_crop$Country,"iso3c","country.name")
df_crop_iso<-df_crop%>% 
  drop_na()

df_crop_index_wide<-pivot_wider(df_crop_index,names_from = "Subject", values_from = "Total")
for (i in 1:nrow(df_crop_index_wide)) {
  df_crop_index_wide$Total[i] <- df_crop_index_wide$Wheat[i] + df_crop_index_wide$Maize[i]+df_crop_index_wide$Rice[i]+df_crop_index_wide$Soybean[i]}
distinct_world<-df_crop_index %>% 
  distinct(Country)

df_crop_iso<-df_crop_iso%>% 
  mutate(Country = sub("United States","USA",Country)) %>% 
  mutate(Country = sub("United Kingdom","UK",Country))
df <- world.cities %>%
  filter(capital == 1) %>%
  dplyr::select(Country= country.etc, lat, lng = long) %>%
  left_join(df_crop_iso, by = "Country") %>% 
  drop_na()


beefnveal<-read_csv("Beef and Veal.csv")
sheep<-read_csv("Sheep.csv")
pork<-read_csv("Pork.csv")
poultry<-read_csv("Poultry.csv")
beefnveal<-beefnveal %>% 
  select(-`Flag Codes`,-FREQUENCY)
sheep<-sheep %>% 
  select(-`Flag Codes`,-FREQUENCY)
pork<-pork %>% 
  select(-`Flag Codes`,-FREQUENCY)
poultry<-poultry %>% 
  select(-`Flag Codes`,-FREQUENCY)
df_meat<-rbind(beefnveal,sheep,pork,poultry)
df_meat<-df_meat %>% 
  mutate(SUBJECT=sapply(SUBJECT,str_to_title)) %>% 
  mutate(MEASURE = sub("THND_TONNE","Thousand Tonnnes",MEASURE)) %>%
  mutate(INDICATOR = sub("MEATCONSUMP","Meat Consumption",INDICATOR)) %>% 
  mutate(SUBJECT = sub("Pig","Pork",SUBJECT)) %>% 
  rename(Country=LOCATION,
         Year=TIME,
         Indicator=INDICATOR,
         Measure=MEASURE,
         Subject=SUBJECT,
         Total=Value)

df_meat_index<-df_meat %>% 
  filter(Country %in% c("BRICS","WLD","OECD")) %>% 
  mutate(Country=ifelse(Country=="WLD","Worldwide",Country))
df_meat$Country<-countrycode(df_meat$Country,"iso3c","country.name")
df_meat_iso<-df_meat%>% 
  drop_na()

df_meat_index_wide<-pivot_wider(df_meat_index,names_from = "Subject", values_from = "Total")
for (i in 1:nrow(df_crop_index_wide)) {
  df_meat_index_wide$Total[i] <- df_meat_index_wide$Beef[i] + df_meat_index_wide$Sheep[i]+df_meat_index_wide$Pork[i]+df_meat_index_wide$Poultry[i]}
distinct_world2<-df_meat_index %>% 
  distinct(Country)
df_meat_iso<-df_meat_iso%>% 
  mutate(Country = sub("United States","USA",Country)) %>% 
  mutate(Country = sub("United Kingdom","UK",Country))
df_1 <- world.cities %>%
  filter(capital == 1) %>%
  dplyr::select(Country= country.etc, lat, lng = long) %>%
  left_join(df_meat_iso, by = "Country") %>% 
  drop_na()

distinct_year<-df_crop_iso %>%distinct(Year)
df_world_meat<-df_meat_index %>% 
  filter(Country=="Worldwide") 
df_world_crop<-df_crop_index %>% 
  filter(Country=="Worldwide")
distinct_meats<-df_meat_iso %>%distinct(Subject)

df_meat_iso_wide<-pivot_wider(df_meat_iso,names_from = "Subject", values_from = "Total")
df_crop_iso_wide<-pivot_wider(df_crop_iso,names_from = "Subject", values_from = "Total")
for (i in 1:nrow(df_meat_iso_wide)) {
  df_meat_iso_wide$Total[i] <- df_meat_iso_wide$Beef[i] + df_meat_iso_wide$Sheep[i]+df_meat_iso_wide$Pork[i]+df_meat_iso_wide$Poultry[i]}
for (i in 1:nrow(df_crop_iso_wide)) {
  df_crop_iso_wide$Total[i] <- df_crop_iso_wide$Wheat[i] + df_crop_iso_wide$Maize[i]+df_crop_iso_wide$Rice[i]+df_crop_iso_wide$Soybean[i]}
df_general_crop<-df_crop_iso_wide %>%
  select(Country,Year,Total_Crop=Total)
df_general_meat<-df_meat_iso_wide %>%
  select(Country,Year,Total_meat=Total)




shinyApp(ui = ui,server = server)