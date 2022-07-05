library(tidyr)
library(tidyverse)
library(dplyr)
library(readxl)
library(Rcpp)
library(tibble)
library(readr)

setwd("C:/Users/jenna/OneDrive/Data_Repository/DSP_HAB")


stations = read.csv("CDEC_Stations.csv")

long = separate_rows(stations, "Sensors",  convert = TRUE)

long$Sensors = as.integer(long$Sensors)

sensors = read.csv("CDEC_sensor_code_list.csv") 


#sensor_list= data_frame(
#  number = c("1", "20", "21", "25", "28", "61", "62", "100", "221"), 
#  sensor.name= c("RIV STG", "FLOW", "VLOCITY", "TEMP W", "CHLORPH", "DIS OXY", "PH VAL", "EL COND", "TURB WF"),
 # units = c("FEET", "CFS", "FT/SEC", "DEG F", "ug/L", "MG/L", "PH", "uS/cm", "FNU")
#

#$number = as.integer(sensor_list$number)

long = rename(long, Sensor.No = Sensors)

test2 <- dplyr::inner_join(long, sensors, by = "Sensor.No")

check = test2 %>% 
  select(STA, Station.Name, Latitude, Longitude, County.Name, Agency.Name, Sensor.No, Sensor, Description, Units) %>% 
  filter(Sensor.No == "1" | Sensor.No == "20" |Sensor.No == "21" | Sensor.No =="25" | Sensor.No == "28"|Sensor.No =="61"| Sensor.No == "62" | Sensor.No == "100" | Sensor.No == "221") %>%
  group_by(STA)

write_csv(check, "check.csv")

check2<-subset(check, select= -c(Sensor.No, Description, Units))

check2 %>% 
  pivot_wider(check2, names_from = Sensor, values_from = Units)


flow.stations = dplyr::filter(check, Sensor %in% c("FLOW", "VLOCITY"))

chl.stations = dplyr::filter(check, Sensor %in% c("CHLORPH"))

wq.stations = dplyr::filter(check, Sensor %in% c("TURB WF", "PH VAL", "DIS OXY"))


#trying to make it wide so each variable has it's own row, with a true/false statement
#wide = check2 %>% 
  pivot_wider(check2, names_from = c(STA, Station.Name, Latitude, Longitude, County.Name, Agency.Name), values_from = Sensor)
  
  
# Map Testing
  
#load libraries
  #GIS libraries
  library(ggmap)
  library(deltamapr)
  library(sf)
  require(sf)
  require(deltamapr)
  library(ggsn) #for north arrow
  
  
#Flow stations
  
 flow = st_as_sf(flow.stations, coords = c("Longitude", "Latitude"), crs = 4326) 
  
  
flow.map =  ggplot()+
    geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+ 
    geom_sf(data = flow, aes(fill = Agency.Name), shape = 21, color = "black", size = 3)+
    theme_bw()+
    scale_x_continuous(limits = c(-122.5, -121.2)) +
    scale_y_continuous( limits = c(37.65, 38.4)) 

#Chlorophyll stations


chl = st_as_sf(wq.stations, coords = c("Longitude", "Latitude"), crs = 4326) 


chl.map =  ggplot()+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+ 
  geom_sf(data = wq, aes(fill = Agency.Name), shape = 21, color = "black", size = 3)+
  theme_bw()+
  scale_x_continuous(limits = c(-122.5, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.4)) 

#water quality stations

wq = st_as_sf(wq.stations, coords = c("Longitude", "Latitude"), crs = 4326) 

wq.map =  ggplot()+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+ 
  geom_sf(data = wq, aes(fill = Agency.Name), shape = 21, color = "black", size = 3)+
  theme_bw()+
  scale_x_continuous(limits = c(-122.5, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.4)) 


               