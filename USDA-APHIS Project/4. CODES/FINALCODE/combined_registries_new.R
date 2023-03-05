library(data.table)
library(tidyverse)
library(stringi)
library(rgeos)
library(raster)
library(tmap)
library(rgdal)
library(tigris)
library(RColorBrewer)

setwd("G:/Shared drives/2020 FIRE-APHIS/INTERMEDIATE/")

#combine npip data #############################################
clean_npip <- function(npip1, npip2) {
  
  colnames<-names(npip1)
  names(npip1)<-tolower(colnames)
  npip1<-npip1 %>% mutate(num=str_length(name)) %>%
    mutate(name=substr(name,1,num-2)) %>%
    dplyr::select(name, address, phone, hatchery, independent, dealer, e)
  
  npip2<-npip2 %>%  mutate(x=substr(pnt, 3,15)) %>%
    mutate(num=str_length(V6)) %>%
    mutate(y=substr(V6,1,num-1)) %>%
    dplyr::select(address, address_google, x, y)
  npip2$x<-as.numeric(npip2$x)
  npip2$y<-as.numeric(npip2$y)
  names(npip2)<-c("address", "address_google", "x", "y")
  
  npip<-merge(npip1, npip2, by="address")
  return(npip)
}

#count addresses without street number
count_no_street <- function(npip) {
  npip2 <- npip %>% mutate(words = str_count(address_google, ", ")) 
  out <- npip2 %>% filter(words<=2 | is.na(words))
  numout<-nrow(out)
  final <- npip2 %>% filter(words>2)
  outlist<- list(out = out, final = final, numout = numout)
  return(outlist)
}

npip1<-fread("cleaned_npip_de.csv")
npip2<-fread("geocoded_npip_de.csv", header=TRUE)
npip_de<-clean_npip(npip1, npip2)

npip1<-fread("cleaned_npip_md.csv")
npip2<-fread("geocoded_npip_md.csv", header=TRUE)
npip_md<-clean_npip(npip1, npip2)

npip1<-fread("cleaned_npip_va.csv")
npip2<-fread("geocoded_npip_va.csv", header=TRUE)
npip_va<-clean_npip(npip1, npip2)

npip<-rbind(npip_va, npip_de, npip_md) 

npip<-npip %>% 
  distinct() %>% 
  mutate(address=str_to_title(address)) %>%
  mutate(name=str_to_title(name)) %>%
  rename(npip_hatchery=hatchery, npip_ind=independent, npip_dealer=dealer, npip_e=e) %>%
  mutate(phone=str_replace_all(phone, "\\(", "")) %>%
  mutate(phone=str_replace_all(phone, "\\) ", "-")) %>%
  mutate(phone=str_replace_all(phone, "\\)", "-")) %>%
  mutate(dset_npip=1)

npip_list <- count_no_street(npip)

#clean localharvest.org #############################################################
clean_localharvest <- function(lh1, lh2) {
  lh1<-lh1 %>% filter(!is.na(address))
  
  lh2<-lh2 %>% mutate(x=substr(location_type, 3,15)) %>%
    mutate(num=str_length(pnt)) %>%
    mutate(y=substr(pnt,1,num-1)) %>%
    dplyr::select(address, x, y)
  lh2$x<-as.numeric(lh2$x)
  lh2$y<-as.numeric(lh2$y)
  names(lh2)<-c("address_google", "x", "y")
  
  lh<-cbind(lh1,lh2)
  return(lh)
}

lh1<-fread("scraped_localharvest_de.csv", header=TRUE)
lh2<-fread("geocoded_localharvest_de.csv")
lh_de<-clean_localharvest(lh1,lh2)

lh1<-fread("scraped_localharvest_md.csv", header=TRUE)
lh2<-fread("geocoded_localharvest_md.csv")
lh_md<-clean_localharvest(lh1,lh2)

lh1<-fread("scraped_localharvest_va.csv", header=TRUE)
lh2<-fread("geocoded_localharvest_va.csv")
lh_va<-clean_localharvest(lh1,lh2)

lharvest<-rbind(lh_va, lh_de, lh_md)

lharvest<-lharvest %>% 
  distinct() %>% 
  mutate(address=str_to_title(address)) %>%
  mutate(name=str_to_title(name)) %>%
  mutate(phone=str_replace_all(phone, "\\(", "")) %>%
  mutate(phone=str_replace_all(phone, "\\) ", "-")) %>%
  mutate(dset_lharvest=1)

lharvest_list <- count_no_street(lharvest)

#clean localhens.com #############################################################
lh1<-fread("scraped_localhens_de.csv")
lh2<-fread("geocoded_localhens_de.csv")
lh_de<-clean_localharvest(lh1,lh2)

lh1<-fread("scraped_localhens_md.csv")
lh2<-fread("geocoded_localhens_md.csv")
lh_md<-clean_localharvest(lh1,lh2)

lh1<-fread("scraped_localhens_va.csv")
lh2<-fread("geocoded_localhens_va.csv")
lh_va<-clean_localharvest(lh1,lh2)

lhens<-rbind(lh_va, lh_de, lh_md)

lhens<-lhens %>% 
  distinct() %>% 
  mutate(address=str_to_title(address)) %>%
  mutate(name=str_to_title(name)) %>%
  mutate(phone=str_replace_all(phone, "\\(", "")) %>%
  mutate(phone=str_replace_all(phone, "\\) ", "-")) %>%
  mutate(dset_lhens=1)

lhens_list <- count_no_street(lhens)

#clean mdbest_eggs #############################################################
clean_mdbest <- function(mdb1, mdb2) {
  mdb1<-mdb1 %>% mutate(nchar=str_length(address)) %>% 
    filter(nchar>0) %>%
    dplyr::select(name, address, phone)
  
  mdb2<-mdb2 %>% mutate(x=substr(pnt, 3,15)) %>%
    mutate(num=str_length(V6)) %>%
    mutate(y=substr(V6,1,num-1)) %>%
    dplyr::select(address_google, x, y)
  mdb2$x<-as.numeric(mdb2$x)
  mdb2$y<-as.numeric(mdb2$y)
  names(mdb2)<-c("address_google", "x", "y")
  
  lh<-cbind(mdb1,mdb2)
  return(lh)
}

mdb1<-fread("scraped_mdbest_eggs.csv")
mdb2<-fread("geocoded_mdbest_eggs.csv", header=TRUE)
mdb_eggs<-clean_mdbest(mdb1,mdb2)

mdb1<-fread("scraped_mdbest_poultry.csv")
mdb2<-fread("geocoded_mdbest_poultry.csv", header=TRUE)
mdb_poult<-clean_mdbest(mdb1,mdb2)

mdbest<-rbind(mdb_eggs, mdb_poult)

mdbest<-mdbest %>% 
  distinct() %>%
  mutate(dset_mdbest=1)

mdbest_list <- count_no_street(mdbest)

#VA Grown######################################################
clean_vagrown <- function(vagrown) {
  vagrown2<-vagrown %>% mutate(x=substr(location_type, 3,15)) %>%
    mutate(num=str_length(pnt)) %>%
    mutate(y=substr(pnt,1,num-1)) %>%
    dplyr::select(V2, Phone, status, address, x, y)
  vagrown2$x<-as.numeric(vagrown2$x)
  vagrown2$y<-as.numeric(vagrown2$y)
  names(vagrown2)<-c("phone", "name", "address", "address_google", "x", "y")
  return(vagrown2)
}

vagrown<-fread("vagrown_eggs.csv")
eggs<-clean_vagrown(vagrown)

vagrown<-fread("vagrown_poultry.csv")
poult<-clean_vagrown(vagrown)

vagrown<-rbind(eggs, poult) %>% 
  distinct() %>%
  mutate(phone=str_replace_all(phone, " ", "-")) %>%
  mutate(dset_vagrown=1)

vagrown_list <- count_no_street(vagrown)

#MDA######################################################
mda<-fread("MDA.csv")

mda2<-mda %>% mutate(x=substr(location_type, 3,15)) %>%
  mutate(num=str_length(pnt)) %>%
  mutate(y=substr(pnt,1,num-1)) %>%
  dplyr::select(V2, status, address, x, y) %>%
  distinct()

mda2$x<-as.numeric(mda2$x)
mda2$y<-as.numeric(mda2$y)
names(mda2)<-c("name", "address", "address_google", "x", "y")

mda<-mda2 %>% 
  distinct() %>% 
  mutate(address=str_to_title(address)) %>%
  mutate(name=str_to_title(name)) %>%
  mutate(dset_mda=1)

mda_list <- count_no_street(mda)

#Make full dataset######################################################

alldf<-merge(npip, lharvest, by=c("address_google", "x", "y"), all=TRUE)

alldf2<-alldf %>% 
  mutate(address=ifelse(!is.na(address.x), address.x, address.y)) %>%
  mutate(phone=ifelse(!is.na(phone.x), phone.x, phone.y)) %>%
  mutate(name=ifelse(!is.na(name.x), name.x, name.y)) %>%
  dplyr::select(-address.x, -address.y, -phone.x, -phone.y, -name.x, -name.y)

alldf3<-merge(alldf2, lhens, by=c("address_google", "x", "y"), all=TRUE)

alldf3<-alldf3 %>% 
  mutate(address=ifelse(!is.na(address.x), address.x, address.y)) %>%
  mutate(phone=ifelse(!is.na(phone.x), phone.x, phone.y)) %>%
  mutate(name=ifelse(!is.na(name.x), name.x, name.y)) %>%
  dplyr::select(-address.x, -address.y, -phone.x, -phone.y, -name.x, -name.y)

alldf4<-merge(alldf3, mdbest, by=c("address_google", "x", "y"), all=TRUE)

alldf4<-alldf4 %>% 
  mutate(address=ifelse(!is.na(address.x), address.x, address.y)) %>%
  mutate(phone=ifelse(!is.na(phone.x), phone.x, phone.y)) %>%
  mutate(name=ifelse(!is.na(name.x), name.x, name.y)) %>%
  dplyr::select(-address.x, -address.y, -phone.x, -phone.y, -name.x, -name.y)

alldf5<-merge(alldf4, vagrown, by=c("address_google", "x", "y"), all=TRUE)

alldf5<-alldf5 %>% 
  mutate(address=ifelse(!is.na(address.x), address.x, address.y)) %>%
  mutate(phone=ifelse(!is.na(phone.x), phone.x, phone.y)) %>%
  mutate(name=ifelse(!is.na(name.x), name.x, name.y)) %>%
  dplyr::select(-address.x, -address.y, -phone.x, -phone.y, -name.x, -name.y)

alldf6<-merge(alldf5, mda, by=c("address_google", "x", "y"), all=TRUE)

alldf6<-alldf6 %>% 
  mutate(address=ifelse(!is.na(address.x), address.x, address.y)) %>%
  mutate(name=ifelse(!is.na(name.x), name.x, name.y)) %>%
  dplyr::select(-address.x, -address.y, -name.x, -name.y) %>%
  distinct() %>%
  mutate(x=ifelse(str_detect(address_google, ", MD") | 
                                 str_detect(address_google, ", VA") | 
                                 str_detect(address_google, ", DE"), x, NA)) %>%
  mutate(y=ifelse(str_detect(address_google, ", MD") | 
                                 str_detect(address_google, ", VA") | 
                                 str_detect(address_google, ", DE"), y, NA)) %>%
  mutate(address_google=ifelse(str_detect(address_google, ", MD") | 
                                 str_detect(address_google, ", VA") | 
                                 str_detect(address_google, ", DE"), address_google, NA)) %>%
  dplyr::select(name, address, address_google, x, y, website, phone, dset_npip, npip_e, dset_mda,
                dset_vagrown, dset_lharvest, dset_lhens, dset_mdbest)

write.csv(alldf6, "allregistries.csv", row.names = F)
  
alldf7 <- alldf6 %>% mutate(words = str_count(address_google, ", ")) 
out <- alldf7 %>% filter(words<=2 | is.na(words))
numout<-nrow(out)
final <- alldf7 %>% 
  filter(words>2) %>%
  mutate(dataset=ifelse(dset_npip==1 & is.na(dset_lharvest) & is.na(dset_lhens) & 
                          is.na(dset_mdbest) & is.na(dset_vagrown) & is.na(dset_mda), "NPIP", NA)) %>%
  
  mutate(dataset=ifelse(dset_npip==1 & is.na(dset_lharvest) & is.na(dset_lhens) & 
                          dset_mdbest==1 & is.na(dset_vagrown) & dset_mda==1, "NPIP & MDBest & MDA", dataset)) %>%
  
  mutate(dataset=ifelse(dset_npip==1 & is.na(dset_lharvest) & is.na(dset_lhens) & 
                          is.na(dset_mdbest) & is.na(dset_vagrown) & dset_mda==1, "NPIP & MDA", dataset)) %>%
  
  mutate(dataset=ifelse(dset_npip==1 & is.na(dset_lharvest) & is.na(dset_lhens) & 
                          is.na(dset_mdbest) & dset_vagrown==1 & is.na(dset_mda), "NPIP & VAGrown", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & is.na(dset_lharvest) & is.na(dset_lhens) & 
                          is.na(dset_mdbest) & dset_vagrown==1 & dset_mda==1, "MDA & VAGrown", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & is.na(dset_lharvest) & is.na(dset_lhens) & 
                          is.na(dset_mdbest) & is.na(dset_vagrown) & dset_mda==1, "MDA", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & dset_lharvest==1 & is.na(dset_lhens) & 
                          dset_mdbest==1 & is.na(dset_vagrown) & dset_mda==1, "MDA & LHarvest & MDBest", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & dset_lharvest==1 & is.na(dset_lhens) & 
                          is.na(dset_mdbest) & is.na(dset_vagrown) & dset_mda==1, "MDA & LHarvest", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & is.na(dset_lharvest) & dset_lhens==1 & 
                          is.na(dset_mdbest) & is.na(dset_vagrown) & dset_mda==1, "MDA & LHens", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & is.na(dset_lharvest) & dset_lhens==1 & 
                          dset_mdbest==1 & is.na(dset_vagrown) & dset_mda==1, "MDA & LHens & MDBest", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & is.na(dset_lharvest) & is.na(dset_lhens) & 
                          dset_mdbest==1 & is.na(dset_vagrown) & dset_mda==1, "MDA & MDBest", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & is.na(dset_lharvest) & is.na(dset_lhens) & 
                          is.na(dset_mdbest) & dset_vagrown==1 & is.na(dset_mda), "VAGrown", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & dset_lharvest==1 & is.na(dset_lhens) & 
                          is.na(dset_mdbest) & dset_vagrown==1 & is.na(dset_mda), "VAGrown & LHarvest", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & is.na(dset_lharvest) & dset_lhens==1 & 
                          is.na(dset_mdbest) & dset_vagrown==1 & is.na(dset_mda), "VAGrown & LHens", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & dset_lharvest==1 & is.na(dset_lhens) & 
                          is.na(dset_mdbest) & is.na(dset_vagrown) & is.na(dset_mda), "LHarvest", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & dset_lharvest==1 & dset_lhens==1 & 
                          is.na(dset_mdbest) & is.na(dset_vagrown) & is.na(dset_mda), "LHarvest & LHens", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & dset_lharvest==1 & is.na(dset_lhens) & 
                          dset_mdbest==1 & is.na(dset_vagrown) & is.na(dset_mda), "LHarvest & MDBest", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & is.na(dset_lharvest) & dset_lhens==1 & 
                          dset_mdbest==1 & is.na(dset_vagrown) & is.na(dset_mda), "LHens & MDBest", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & is.na(dset_lharvest) & dset_lhens==1 & 
                          is.na(dset_mdbest) & is.na(dset_vagrown) & is.na(dset_mda), "LHens", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & is.na(dset_lharvest) & is.na(dset_lhens) & 
                          dset_mdbest==1 & is.na(dset_vagrown) & is.na(dset_mda), "MDBest", dataset)) %>%
  
  mutate(dataset=ifelse(dset_npip==1 & is.na(dset_lharvest) & is.na(dset_lhens) & 
                          is.na(dset_mdbest) & is.na(dset_vagrown) & is.na(dset_mda), "NPIP", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & dset_lharvest==1 & is.na(dset_lhens) & 
                          is.na(dset_mdbest) & is.na(dset_vagrown) & is.na(dset_mda), "LHarvest", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & is.na(dset_lharvest) & is.na(dset_lhens) & 
                          is.na(dset_mdbest) & dset_vagrown==1 & is.na(dset_mda), "VAGrown", dataset)) %>%
  
  mutate(dataset=ifelse(is.na(dset_npip) & is.na(dset_lharvest) & is.na(dset_lhens) & 
                          is.na(dset_mdbest) & is.na(dset_vagrown) & dset_mda==1, "MDA", dataset)) 

final2<-final %>% mutate(dataset=ifelse(is.na(dataset), "MDA & LHens", dataset)) %>%
  dplyr::select(name, address_google, x, y, address, website, phone, dataset) %>%
  rename(DataSet=dataset)

#map location of farms###########################################################################
mapdf<-final2

coordinates(mapdf)=~x+y

info <- ogrInfo("G:/Shared drives/2020 FIRE-APHIS/DATA/Shapefiles/cb_2018_us_state_500k/cb_2018_us_state_500k.shp", require_geomType = "wkbPolygon")
states<-readOGR("G:/Shared drives/2020 FIRE-APHIS/DATA/Shapefiles/cb_2018_us_state_500k/cb_2018_us_state_500k.shp",info[["layer"]],require_geomType = "wkbPolygon")
idx<-(states@data$STUSPS=="VA" | states@data$STUSPS=="MD" | states@data$STUSPS=="DE")
dmv<-states[idx,]

cnty<-counties(state=c("VA", "MD", "DE"))

tempdir <- "G:/Shared drives/2020 FIRE-APHIS/DATA/Shapefiles"
writeOGR(obj = cnty, dsn<-tempdir, layer = "dmvcounties", driver = "ESRI Shapefile", overwrite_layer = TRUE)

###################################################

final3<-final2 %>% 
  mutate(dset=ifelse(DataSet=="VAGrown" | DataSet=="MDA" | DataSet=="NPIP" | 
                       DataSet=="MDA & VAGrown" | DataSet=="NPIP & MDA" | DataSet=="NPIP & VAGrown", DataSet, NA)) %>%
  mutate(dset=ifelse(str_detect(DataSet,"NPIP") & is.na(dset), "NPIP & Other", dset)) %>%
  mutate(dset=ifelse(str_detect(DataSet,"VAGrown") & is.na(dset), "VAGrown & Other", dset)) %>%
  mutate(dset=ifelse(str_detect(DataSet,"MDA") & is.na(dset), "MDA & Other", dset)) %>%
  mutate(dset=ifelse(is.na(dset), "Other", dset))

write.csv(final3, "G:/Shared drives/2020 FIRE-APHIS/INTERMEDIATE/allregistries_map.png", row.names = F)

mapdf<-final3
coordinates(mapdf)=~x+y
col1 <- brewer.pal(n=10, "Spectral")

map<-tm_shape(cnty)+tm_borders("gray80")+
  tm_shape(mapdf)+tm_dots("DataSet", palette=col1,size=0.1)+
  tm_layout(legend.text.size=1, legend.title.size=2, attr.outside=TRUE, attr.position = c("left", "bottom"))

tmap_options(output.dpi = 500)
tmap_save(tm=map, filename="G:/Shared drives/2020 FIRE-APHIS/INTERMEDIATE/registriesmap.png")

final4<- final3 %>% group_by(dset) %>% tally() %>%
  mutate(lab.ypos = cumsum(n) - 0.3*n)

col1 <- brewer.pal(n=10, "Spectral")
ggplot(final4, aes(x="", y=n, fill=dset))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  scale_fill_manual(values=col1)+
  theme_void()+
  theme(axis.text.x=element_blank()) +
  labs(x = NULL, y = NULL, fill = NULL)+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text=element_text(size=7))

ggsave("G:/Shared drives/2020 FIRE-APHIS/INTERMEDIATE/registriespie.png")  

final5<-final3 %>% filter(str_detect(dset,"Other")) %>% 
  filter(str_detect(address_google,", VA"))

final6<-final3 %>% filter(str_detect(address_google,", MD"))
