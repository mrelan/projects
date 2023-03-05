library(tidygeocoder)
library(data.table)
library(tidyverse)
library(tigris)
library(rgdal)
library(foreign)
library(mapsapi)
library(readr)

lhde<-fread("G:\\Shared drives\\2020 FIRE-APHIS\\INTERMEDIATE\\scraped_localharvest_de.csv")


lhde <- lhde[-c(1,5,18,23,29,43,44,48), ]

key="AIzaSyD9g0_mF28VTSd-BhlvnEfZ3ux84BI_7mo" 

i<-1
doc = mp_geocode(
  addresses = lhde$address[i],
  key = key,
  quiet = TRUE
)
pnt = mp_get_points(doc)

alladd<-c()

for (i in 1:nrow(lhde)){
  print(i)
  doc = mp_geocode(
    addresses = lhde$address[i],
    key = key,
    quiet = TRUE
  )
  pnt = mp_get_points(doc)
  alladd<-rbind(alladd,pnt)
}

write.csv(alladd,"G:\\Shared drives\\2020 FIRE-APHIS\\INTERMEDIATE\\geocoded_localharvest_de.csv", row.names = FALSE)
