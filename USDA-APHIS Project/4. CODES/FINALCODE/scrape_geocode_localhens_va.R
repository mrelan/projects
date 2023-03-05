library(rvest)
library(tidyverse)
library(curl)
library(tidygeocoder)
library(data.table)
library(tigris)
library(rgdal)
library(foreign)
library(mapsapi)

# Scraping farm addresses from local hens

locations <- html("https://localhens.com/find-a-farm-results/?post%5B0%5D=farm&tax%5Bproduct_type%5D%5B0%5D=12&address%5B0%5D=Virginia%2C%20USA&distance=200&units=imperial&title&per_page=50&lat=37.431573&lng=-78.656894&form=1&action=fs&state=Virginia&country=US")

list <- locations %>%
  html_nodes(".farm-link") %>%
  html_text()

list <- data.frame(list)

list2 <- list %>% mutate(name=tolower(str_remove(list,"Learn More "))) %>%
  mutate(name=str_replace_all(name, " ", "-"))

list2$name[4] <- "franchescas-dawn-farm"
list2$name[7] <- "gods-whisper-farm"
list2$name[12] <- "farms/ginns-lil-farm-creations"
list2$name[19] <- "woodlawn-farms-blue-andalusian-chickens-cotton-patch-geese"
list2$name[27] <- "thompsons-fresh-eggs"
list2$name[39] <- "yamamma"
list2$name[40] <- "violets-hens"



list3 <- list %>% mutate(name=str_remove(list,"Learn More "))

address <- c()
phone <- c()

for (i in 1:nrow(list2)) {
  print(i)
  link<-paste0("https://localhens.com/farms/",list2$name[i],"/")
  
  
  farm <- html(link)
  farmad <- farm %>%
    html_nodes(".farm-address") %>%
    html_text()
  address[i] <- farmad
  
  farmph<-farm %>%
    html_nodes(".farm-phone") %>%
    html_text()
  if (is_empty(farmph)) {
    phone[i]<-NA
  }
  else {
    phone[i]<-farmph
  }
}


address <- data.frame(address)
phone<-data.frame(phone)
N <- nrow(address)

output <- cbind(list3[1:N,], address, phone)
output$list <- NULL

write.csv(output, "G:\\Shared drives\\2020 FIRE-APHIS\\INTERMEDIATE\\scraped_localhens_va.csv", row.names = FALSE)

# Geocoding the addresses using Google API

lhva <- output %>% mutate_all(as.character)

key="AIzaSyD9g0_mF28VTSd-BhlvnEfZ3ux84BI_7mo" 

i <- 1
doc = mp_geocode(
  addresses = lhva2$address[i],
  key = key
)
pnt = mp_get_points(doc)

result <- c()

for (i in 1:nrow(lhva)){
  print(i)
  doc = mp_geocode(
    addresses = lhva$address[i],
    key = key, 
    quiet = TRUE
  )
  pnt = mp_get_points(doc)
  result <- rbind(result, pnt)
}

write.csv(result, "G:\\Shared drives\\2020 FIRE-APHIS\\INTERMEDIATE\\geocoded_localhens_va.csv", row.names = FALSE)



