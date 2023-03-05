library(rvest)
library(tidyverse)
library(curl)
library(tidygeocoder)
library(data.table)
library(tigris)
library(rgdal)
library(foreign)
library(mapsapi)

#Scraping addresses of farms in MD from local hens

locations <- html("https://localhens.com/find-a-farm-results/?post%5B0%5D=farm&tax%5Bproduct_type%5D%5B0%5D=12&address%5B0%5D=Maryland%2C%20USA&distance=200&units=imperial&title&per_page=50&lat=39.045755&lng=-76.641271&form=1&action=fs&state=Maryland&country=US")

list <- locations %>%
  html_nodes(".farm-link") %>%
  html_text()

list <- data.frame(list)

list2 <- list %>% mutate(name=tolower(str_remove(list,"Learn More "))) %>%
  mutate(name=str_replace_all(name, " ", "-"))

list2$name[11] <- "its-a-beautiful-day-farm"
list2$name[17] <- "haydens-coop"
list2$name[18] <- "aines-acres"

list3<-list %>% mutate(name=str_remove(list,"Learn More "))

address <- c()
phone <- c()

for (i in 1:nrow(list2)) {
  print(i)
  link<-paste0("https://localhens.com/farms/",list2$name[i],"/")
  
  farm<-html(link)
  farmad<-farm %>%
    html_nodes(".farm-address") %>%
    html_text()
  address[i]<-farmad
  
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

output<-cbind(list3[1:N,], address, phone)
output$list <- NULL

write.csv(output, "G:\\Shared drives\\2020 FIRE-APHIS\\INTERMEDIATE\\scraped_localhens_md.csv", row.names = FALSE)

#Finding the geocode of the addresses using Google API 

lhmd <- output %>% mutate_all(as.character)

key="AIzaSyD9g0_mF28VTSd-BhlvnEfZ3ux84BI_7mo" 

i <- 1
doc = mp_geocode(
  addresses = lhmd$address[i],
  key = key,
  quiet = TRUE
)
pnt = mp_get_points(doc)

result<-c()

for (i in 1:nrow(lhmd)){
  doc = mp_geocode(
    addresses = lhmd$address[i],
    key = key,
    quiet = TRUE
  )
  pnt = mp_get_points(doc)
  result <- rbind(result, pnt)
}

write.csv(result, "G:\\Shared drives\\2020 FIRE-APHIS\\INTERMEDIATE\\geocoded_localhens_md.csv", row.names = FALSE)


