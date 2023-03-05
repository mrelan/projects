
library(tidygeocoder)
library(tidyverse)
library(data.table)
library(readxl)
library(mapsapi)


mddf<-read_excel("G:/Shared drives/2020 FIRE-APHIS/DATA/MDA/MD_ Approved_ Packers_and_Wholesalers.xlsx")

mdv<- paste(mddf$Address, mddf$City, sep= " ")

mddf2<- data.frame(1:751, mdv)
mddf2$mdv<-as.character(mddf2$mdv)
names(mddf2)<- c("Order", "Address")

mddf2<-cbind.data.frame(mddf$Name , mddf2)
mddf2<-mddf2[-2]
names(mddf2)<-c("Name", "Address")

key="AIzaSyA2vplXkRpa1QxMe5YxfBsLoO5qPJ9eFaE" 

#filter for Maryland farms and geocode them
mddf3<-mddf2 %>%
  filter(str_detect(Address, "MD"))

listsmd<-mddf3$Address 

dlists<-stri_enc_toutf8(listsmd)

outputmd<-c()

for(i in 1:length(listsmd)){
  print(i)
  doc = mp_geocode(address = listsmd[i],key = key)
  pnt = mp_get_points(doc)
  outputmd<-rbind(outputmd,pnt)
}

outputmd <- cbind.data.frame(mddf3$Name, outputmd)
names(outputmd)<-c("Name", "id", "status", "address", "address_google", "location_type", "pnt")

#filter for Delaware farms and geocode them
mddf4<-mddf2 %>%
  filter(str_detect(Address, " DE "))
mddf4<-mddf4[-(1:1), , drop = FALSE]
mddf4<-mddf4[-(6:7), , drop = FALSE]
listsde<-mddf4$Address 

dlists<-stri_enc_toutf8(listsde)

outputde<-c()

for(i in 1:length(listsde)){
  print(i)
  doc = mp_geocode(address = listsde[i],key = key)
  pnt = mp_get_points(doc)
  outputde<-rbind(outputde,pnt)
}

outputde<- cbind.data.frame(mddf4$Name, outputde)
names(outputde)<-c("Name", "id", "status", "address", "address_google", "location_type", "pnt")

#filter for Virginia farms and geocode them
mddf5<-mddf2 %>%
  filter(str_detect(Address, " VA "))

listsva<-mddf5$Address 

dlists<-stri_enc_toutf8(listsva)

outputva<-c()

for(i in 1:length(listsva)){
  print(i)
  doc = mp_geocode(address = listsva[i],key = key)
  pnt = mp_get_points(doc)
  outputva<-rbind(outputva,pnt)
}

outputva<- cbind.data.frame(mddf5$Name, outputva)
names(outputva)<-c("Name", "id", "status", "address", "address_google", "location_type", "pnt")

#combine all the outputs into one dataframe and save it as a csv
finaloutput<-rbind(outputmd,outputde, outputva)

write.csv(finaloutput, "G:/Shared drives/2020 FIRE-APHIS/INTERMEDIATE/MDA.csv")

