library(tidygeocoder)
library(data.table)
library(tidyverse)
library(tigris)
library(rgdal)
library(foreign)
library(mapsapi)

lh <- fread("G:\\Shared drives\\2020 FIRE-APHIS\\INTERMEDIATE\\scraped_mdbest_poultry.csv", header=TRUE)


key="AIzaSyD9g0_mF28VTSd-BhlvnEfZ3ux84BI_7mo" 

i <- 1
doc = mp_geocode(
  addresses = lh$address[i],
  key = key,
  quiet = TRUE
)
pnt = mp_get_points(doc)

alladd<-c()

for (i in 1:nrow(lh)){
  print(i)
  doc = mp_geocode(
    addresses = lh$address[i],
    key = key,
    quiet = TRUE
  )
  pnt = mp_get_points(doc)
  alladd <- rbind(alladd, pnt)
}

write.csv(alladd,"G:\\Shared drives\\2020 FIRE-APHIS\\INTERMEDIATE\\geocoded_mdbest_poultry.csv", row.names = FALSE)
