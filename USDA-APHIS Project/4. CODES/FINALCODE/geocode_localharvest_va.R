library(tidygeocoder)
library(data.table)
library(tidyverse)
library(tigris)
library(rgdal)
library(foreign)
library(mapsapi)
library(readr)

lhva<-read_csv("G:\\Shared drives\\2020 FIRE-APHIS\\INTERMEDIATE\\scraped_localharvest_va.csv")

lhva <- lhva %>% filter(!is.na(address))


key="AIzaSyD9g0_mF28VTSd-BhlvnEfZ3ux84BI_7mo" 

i<-1
doc = mp_geocode(
  addresses = lhva$address[i],
  key = key
)
pnt = mp_get_points(doc)

alladd<-c()

for (i in 1:nrow(lhva)){
  print(i)
  doc = mp_geocode(
    addresses = lhva$address[i],
    key = key,
    quiet = TRUE
  )
  pnt = mp_get_points(doc)
  alladd<-rbind(alladd,pnt)
}

write.csv(alladd,"G:\\Shared drives\\2020 FIRE-APHIS\\INTERMEDIATE\\geocoded_localharvest_va.csv", row.names = FALSE)
