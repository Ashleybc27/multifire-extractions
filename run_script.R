##run script

library(sf)
library(raster)
library(tidyverse)
library(rgdal)
library(ggplot2)
library(exactextractr)
setwd()

all <- st_read('clipped_wildfires/clipped_wildfires.shp') ##wildfire shapefiles from MTBS
plot(all)
st_crs(all)
cashp <- st_read('ca_eco_l3/ca_eco_l3.shp') ##using ecoregions level 3
snshp <- subset(cashp, cashp$NA_L3CODE=="6.2.12") ##select for Sierra Nevada
allnew <- st_transform(all, crs=st_crs(snshp)) ## match crs
allnew$year <- year(allnew$Ig_Date) #year of ignition
st_crs(allnew)
st_crs(snshp)

snfishp <- st_intersection(allnew,snshp) #select only fires in the desired region

years <- (2004:2021) ##MTBS starts in 1984 so 2004 is the minimum year with 20 yrs previous

burnedarea <- inxfires(snfishp,years,20)
burnedarea <- as.data.frame(burnedarea) ##reburned area per year

###outpolys are used for the severity extractions

##single year for speed tests#
#singleyear <- highhigh(outpolys, inityear = 2021,rangeyears = 1:20)

##this will take a long time 
##path is where all MTBS burn severity maps by year are 
#with additional starting characters of the files (enter up until year)
repeathighsev <- allyearshighhigh(outpolys ,allyears = 2004:2021,rangeyears = 1:20,path='allyearsseverity/mtbs_CA_')

summedall <- repeathighsev %>% group_by(reburn_year) %>%summarise(totalarea = sum(highhigh))





