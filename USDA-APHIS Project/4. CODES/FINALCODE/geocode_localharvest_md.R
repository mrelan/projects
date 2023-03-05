library(tidygeocoder)
library(data.table)
library(tidyverse)
library(tigris)
library(rgdal)
library(foreign)
library(mapsapi)
library(readr)

lhmd<-read_csv("G:\\Shared drives\\2020 FIRE-APHIS\\INTERMEDIATE\\scraped_localharvest_md.csv")


lhmd <- lhmd %>% filter(!is.na(address))

key="AIzaSyD9g0_mF28VTSd-BhlvnEfZ3ux84BI_7mo" 

i<-1
doc = mp_geocode(
  addresses = lhmd$Address[i],
  key = key,
  quiet = TRUE
)
pnt = mp_get_points(doc)

alladd<-c()

for (i in 1:nrow(lhmd)){
  print(i)
  doc = mp_geocode(
    addresses = lhmd$address[i],
    key = key,
    quiet = TRUE
  )
  pnt = mp_get_points(doc)
  alladd<-rbind(alladd,pnt)
}

write.csv(alladd,"G:\\Shared drives\\2020 FIRE-APHIS\\INTERMEDIATE\\geocoded_localharvest_md.csv", row.names = FALSE)
