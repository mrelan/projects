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
locations <- html("https://localhens.com/find-a-farm-results/?post%5B0%5D=farm&tax%5Bproduct_type%5D%5B0%5D=12&address%5B0%5D=Delaware%2C%20USA&distance=200&units=imperial&title&per_page=10&lat=38.910832&lng=-75.527670&form=1&action=fs&state=Delaware&country=US")
list <- locations %>%
  html_nodes(".farm-link") %>%
  html_text()

list <- data.frame(list)

list2 <- list %>% mutate(name=tolower(str_remove(list,"Learn More "))) %>%
  mutate(name = str_replace_all(name, " ", "-"))

list2 <- list2 %>%
  mutate(name = str_replace_all(name, '\\+', ""))

list2$name[7] <- "chi-nana-guins-farm"

list3 <- list %>% mutate(name=str_remove(list,"Learn More "))


address <- c()
phone <- c()

for (i in 1:nrow(list2)) {
  print(i)
  link <- paste0("https://localhens.com/farms/",list2$name[i],"/")
  
  
  farm <-html(link)
  farmad <- farm %>%
    html_nodes(".farm-address") %>%
    html_text()
  address[i] <- farmad
  
  farmph<-farm %>%
    html_nodes(".farm-phone") %>%
    html_text()
  phone[i]<-farmph
}

address <- data.frame(address)
phone<-data.frame(phone)
N <- nrow(address)

output <- cbind(list3[1:N,], address, phone)
output$list <- NULL
output <- data.frame(output)
write.csv(output, "G:\\Shared drives\\2020 FIRE-APHIS\\INTERMEDIATE\\scraped_localhens_de.csv", row.names = FALSE)
# Geocoding addresses using Google API

lhde <- output

lhde2 <- lhde %>% mutate_all(as.character)
#lhde2 <- lhde %>% mutate(address2=ifelse(!str_detect(address, "DE"), lead(name), NA)) %>%
#  mutate(address3=ifelse(!str_detect(address, "DE"), lead(address), NA)) %>%
#  mutate(address=paste(address2, address3)) %>%
#  filter(!is.na(address2)) %>%
#  select(name, address) %>%
#  mutate(address=str_remove_all(address, "\""))

key="AIzaSyD9g0_mF28VTSd-BhlvnEfZ3ux84BI_7mo" 

i <- 1
doc = mp_geocode(
  addresses = lhde2$address[i],
  key = key,
  quiet = TRUE
)
pnt = mp_get_points(doc)

result <- c()

for (i in 1:nrow(lhde2)){
  doc = mp_geocode(
    addresses = lhde2$address[i],
    key = key,
    quiet = TRUE
  )
  pnt = mp_get_points(doc)
  result <- rbind(result, pnt)
}

write.csv(result, "G:\\Shared drives\\2020 FIRE-APHIS\\INTERMEDIATE\\geocoded_localhens_de.csv", row.names = FALSE)




