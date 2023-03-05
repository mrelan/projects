library(tidyverse)
library(data.table)
library(readxl)
library(mapsapi)

vapdf<-read_excel("G:/Shared drives/2020 FIRE-APHIS/DATA/VA Grown/Virginia Grown.xlsx", col_names=F)

#get addresses separatly
vapdf2<- vapdf %>% 
  filter(vapdf$...1 != "NA") %>%
  mutate(address_y_n=ifelse(str_detect(...1 , "VA"), "1","0"))
vapdf2<- vapdf2 %>%
  mutate(address=ifelse(str_detect(address_y_n, "1"), "Y","N"))
vapdf2<-vapdf2 %>%
  filter(address=="Y")
names(vapdf2)<-c("Address", "col1", "col2")

vapdf2<-str_sub(vapdf2$Address, "1", "-14")

vapdf2<- data.frame(1:153, vapdf2)
names(vapdf2)<- c("Order", "Address")

test<-vapdf2 %>% mutate(C= str_extract(Address,'\\D*(?=\\d)'))
lst <- strsplit(test$C, split = " ")

test$Address <- mapply(function(x,y){gsub(paste0(x,collapse = "|"), "",test$Address[y])},lst,1:length(lst))
test$Address <- mapply(function(x,y)gsub(x,"",y) ,gsub(" ", "|",test$C), test$Address)

vapdf2<- test%>%
  filter(!is.na(c(C)))

#fix wrong Addresses
vapdf2$Address[10]<-"7356 Osborne Tpk Richmond VA 23231"
vapdf2$Address[18]<-"6165 River Road West Columbia  VA 23038"
vapdf2$Address[24]<-"1 Avenue of the Arts Newport News  VA 23606"
vapdf2$Address[41]<-"4730 Hammock Lane Norfolk  VA 23518"
vapdf2$Address[43]<-"2223 Wiltshire Road Rockville  VA 23146"
vapdf2$Address[57]<-"1675 Old Church Road Mechanicsville  VA 23111"
vapdf2$Address[69]<-"333 Eakle Road Staunton  VA 24401"
vapdf2$Address[85]<-"Ballards Mill Road Free Union  VA 22940"
vapdf2$Address[111]<-"95 Wilson Trail Stuart  VA 24171"

#delete wrong columns
vapdf4<-vapdf2 %>%
  slice(-c(11, 15, 50,95, 112, 126))

#get names separately
vapodf3<-vapdf %>% mutate(C= str_extract(...1 , '\\D*(?!\\d)'))


lst <- strsplit(vapodf3$C, split = " ")

vapodf3$...1 <- mapply(function(x,y){gsub(paste0(x,collapse = "|"), "",vapodf3$...1[y])},lst,1:length(lst))
vapodf3$...1 <- mapply(function(x,y)gsub(x,"",y) ,gsub(" ", "|",vapodf3$C), vapodf3$...1)


vapodf3<-vapodf3 %>%
  filter(!is.na(C))%>%
  mutate(address_y_n=ifelse(str_detect(C, "VA"), "1","0"))

vapodf3<- vapodf3 %>%
  filter(address_y_n==0) 


#fix mismatches
vapodf3$C[1]<-"7 Acres Farm"
vapodf3$C[105]<- "Rte. 639 Farmers' Market"

#delete wrong columns
vapdf5<-vapodf3 %>%
  slice(-c(17, 28, 65, 85, 92,97,107,145,148))
names(vapdf5)<- c("col1", "Name", "col2")

#merge names and addresses
poultry_n_a<-cbind(vapdf5, vapdf4)%>%
  dplyr::select(Name, Address)



#get phone numbers separately
vapdf6<- vapdf %>%
  mutate(phone=str_sub(...1, -12,-1))%>%
  mutate(phone_yn=ifelse(str_detect(phone, "-"), 1, 0))%>%
  filter(phone_yn==1)%>%
  dplyr::select(phone)

#manually fix phone numbers in excel...
write.csv(vapdf6,"G:/Shared drives/2020 FIRE-APHIS/OUTPUT/Intermediate/Janna/VA_grown_poultry_num.csv")
write.csv(poultry_n_a, "G:/Shared drives/2020 FIRE-APHIS/OUTPUT/Intermediate/Janna/VA_grown_poultry_wnames.csv")

#start geocoding
geocode_p<- fread("G:/Shared drives/2020 FIRE-APHIS/OUTPUT/VA Grown/VA grown poultry names_nums.csv")
geocode_p<-geocode_p[-(1:1), ] 
geocode_p<-geocode_p[,-(1:1)] 

names(geocode_p)<- c("Name", "Address", "Phone")

listsp<-geocode_p$Address 

dlists<-stri_enc_toutf8(listsp)

outputpoultry<-c()

for(i in 1:length(listsp)){
  print(i)
  doc = mp_geocode(address = listsp[i],key = key)
  pnt = mp_get_points(doc)
  outputpoultry<-rbind(outputpoultry,pnt)
}

outputpoultry2 <- cbind.data.frame(geocode_p$Name, outputpoultry)
outputpoultry2<-cbind.data.frame(geocode_p$Phone, outputpoultry2)


names(outputpoultry2)<-c("Phone", "Name", "id", "status", "address", "address_google", "location_type", "pnt")

write.csv(outputpoultry2, "G:/Shared drives/2020 FIRE-APHIS/Intermediate/vagrown_poultry.csv") 

