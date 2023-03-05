library(data.table)
library(tidyverse)
library(readxl)
library(tidygeocoder)

vaeggdf<-read.csv("G:/Shared drives/2020 FIRE-APHIS/OUTPUT/Intermediate/Janna/Virginia Grown Eggs.csv", header=F)


vaeggdf2<- vaeggdf %>%
  filter(vaeggdf$V1 != "NA") %>%
  mutate(address_y_n=ifelse(str_detect(V1, "VA"), "1","0"))

vaeggdf2<- vaeggdf2 %>%
  mutate(address=ifelse(str_detect(address_y_n, "1"), "Y","N"))

vaeggdf2<-vaeggdf2 %>%
  filter(address=="Y")

names(vaeggdf2)<-c("Address", "col1", "col2")

vaeggdf2 <- 
  str_sub(vaeggdf2$Address, "1", "-14")

vaeggdf2<- data.frame(1:211, vaeggdf2)
names(vaeggdf2)<- c("Order", "Address")

vaeggdf2$Address<-as.character(vaeggdf2$Address)


# the first "C" column, used to delete farm names from the addresses
test<-vaeggdf2 %>% mutate(C= str_extract(Address,'\\D*(?=\\d)'))
lst <- strsplit(test$C, split = " ")

#either of these works, run both
test$Address <- mapply(function(x,y){gsub(paste0(x,collapse = "|"), "",test$Address[y])},lst,1:length(lst))
test$Address <- mapply(function(x,y)gsub(x,"",y) ,gsub(" ", "|",test$C), test$Address)
vaeggdf3<-test

#the second "C", used to isolate the farm names
vaeggdf5<-vaeggdf %>% mutate(C= str_extract(V1, '\\D*(?!\\d)'))

lst <- strsplit(vaeggdf5$C, split = " ")

vaeggdf5$V1 <- mapply(function(x,y){gsub(paste0(x,collapse = "|"), "",vaeggdf5$V1[y])},lst,1:length(lst))
vaeggdf5$V1 <- mapply(function(x,y)gsub(x,"",y) ,gsub(" ", "|",vaeggdf5$C), vaeggdf5$V1)


vaeggdf5<-vaeggdf5 %>%
  mutate(address_y_n=ifelse(str_detect(C, "VA"), "1","0"))

vaeggdf5<- vaeggdf5 %>%
  filter(address_y_n==0) 

vaeggdf5$C[1]<-"5 Riders Farm"
vaeggdf5$C[2]<-"7 Acres Farm"
vaeggdf5$C[156]<-"Rte. 639 Farmers' Market"

#change errors in the names
vaeggdf5test<-vaeggdf5 %>%
  slice(-c(12,44,71,105, 199, 148, 131,166, 158))

#change errors in the addresses
vaeggdf3$Address[7]<- "1117 Suiter Road Bland VA 24315"
vaeggdf3$Address[11]<-  "PO Box 791 Abingdon  VA 24212"
vaeggdf3$Address[18]<-"7356 Osborne Tpk Richmond VA 23231"
vaeggdf3$Address[44]<-'	1771 Liskey Road Harrisonburg VA 22801'
vaeggdf3$Address[54]<-"Gainsville  VA 20155"
vaeggdf3$Address[55]<-"6699 Spring Mall Drive Springfield  VA 22150"
vaeggdf3$Address[70]<-"15583 Forest Road Forest  VA 24551"
vaeggdf3$Address[80]<-"1675 Old Church Road Mechanicsville  VA 23111"
vaeggdf3$Address[103]<- "PO Box 887 Powhatan  VA 23139"
vaeggdf3$Address[104]<- "137 Highland Drive Lebanon  VA 24266"
vaeggdf3$Address[127]<-"Ballards Mill Road Free Union  VA 22940"
vaeggdf3$Address[130]<-"780 Butterwood Road Charlotte C.H.  VA 23923"
vaeggdf3$Address[137]<-"1037 Gap Run Rd Paris  VA 20130"
vaeggdf3$Address[143]<- "43 Pure Meadows Lane Swoope  Va 24479"
vaeggdf3$Address[204]<-"Booker T. Washington Hwy & Enterprise Ln Moneta  VA 24121"

#delete ones w/ missing info
vaeggdf3<-vaeggdf3%>%
  slice(-c(25,26,76,105, 160, 179, 189))

#get phone numbers
vaeggdf6<-vaeggdf%>%
  mutate(phone=str_sub(V1, -12,-1))%>%
  mutate(phone_yn=ifelse(str_detect(phone, "-"), 1, 0))%>%
  filter(phone_yn==1)



vaeggdf4<-cbind.data.frame(vaeggdf5test, vaeggdf3)
vaeggdf4<-vaeggdf4[, -7:-12]%>%
dplyr::select(C, Address)
vaeggdf4$C<- str_squish(vaeggdf4$C)


names(vaeggdf4)<- c("Name", "Address")
vaeggdf4<- rbind(c("Bluefield VA Farmers Market", "Walnut Street Bluefield VA 24605"), vaeggdf4)
vaeggdf4<- rbind(c("Fredericksburg Farmers Market-Mayfield Neighborhood Market", "Corner of Tyler and Dixon Streets Fredericksburg VA 22401"), vaeggdf4)
vaeggdf4<- rbind(c("Portsmouth Farmers Market-PortsEvents", "Court and High Street Portsmouth VA 23704"), vaeggdf4)
vaeggdf4<- rbind(c("Shady Oaks", "Blueberry Lane Burr Hill VA 22433"), vaeggdf4)
vaeggdf4<- rbind(c("Staunton Farmers' Market", "Corner of Byers and Johnson Streets Staunton VA 24401"), vaeggdf4)
vaeggdf4<- rbind(c("Cross Street & the Fountain Green Tappahannock VA 22560"), vaeggdf4)

#manually match addresses and phone #s using the va grown eggs website
write.csv(vaeggdf4, "G:/Shared drives/2020 FIRE-APHIS/OUTPUT/Intermediate/Janna/VA_grown_eggs_wnames.csv")
write.csv(vaeggdf6, "G:/Shared drives/2020 FIRE-APHIS/OUTPUT/Intermediate/Janna/VA_egg_pnumbers.csv")

#start geocoding the addresses
 geocode_eggs<- read_csv("G:/Shared drives/2020 FIRE-APHIS/OUTPUT/VA Grown/VA_grown_eggs_names_numbers.csv")
 geocode_eggs<-geocode_eggs[-(1:1), ] 
 geocode_eggs<- geocode_eggs[, -1:-2]

 names(geocode_eggs)<- c("Name", "Address", "Phone") 
 
 listseggs<-geocode_eggs$Address 
 
 dlists<-stri_enc_toutf8(listseggs)
 
 outputeggs<-c()
 
 for(i in 1:length(listseggs)){
   print(i)
   doc = mp_geocode(address = listseggs[i],key = key)
   pnt = mp_get_points(doc)
   outputeggs<-rbind(outputeggs,pnt)
 }

 
 outputeggs2 <- cbind.data.frame(geocode_eggs$Name, outputeggs)
 outputeggs2<-cbind.data.frame(geocode_eggs$Phone, outputeggs2)

 
 names(outputeggs2)<-c("Phone", "Name", "id", "status", "address", "address_google", "location_type", "pnt")

 write.csv(outputeggs2, "G:/Shared drives/2020 FIRE-APHIS/Intermediate/vagrown_eggs.csv") 
 