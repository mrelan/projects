library(gtrendsR)
library(tidyverse)
library(data.table)
library(lubridate)
library(gridExtra)
library(tmap)
library(rgdal)
library(raster)
library(tigris)
library(RColorBrewer)

words<-c("\"keeping chickens\"", 
         "\"chicken coop\"", 
         "\"chicken tractor\"")

df<-gtrends(words, 
            geo = "US", 
            time = "2010-01-01 2019-12-31",
            cat = 66)

df2<-df$interest_by_dma
df3<-df2 %>% filter(!is.na(hits))

df4<-df3 %>% mutate(GEOID = ifelse(location=="Washington DC (Hagerstown MD)", "25180", NA )) %>% 
  mutate(GEOID = ifelse(location=="Salisbury MD", "41540", GEOID ))  %>% 
  mutate(GEOID = ifelse(location=="Baltimore MD", "12580", GEOID ))  %>% 
  mutate(GEOID = ifelse(location=="Bluefield-Beckley-Oak Hill WV","14140", GEOID ))  %>% 
  mutate(GEOID = ifelse(location=="Roanoke-Lynchburg VA", "40220", GEOID ))  %>% 
  mutate(GEOID = ifelse(location=="Harrisonburg VA", "25500", GEOID ))  %>% 
  mutate(GEOID = ifelse(location=="Charlottesville VA", "16820", GEOID ))  %>% 
  mutate(GEOID = ifelse(location=="Richmond-Petersburg VA", "40060", GEOID ))  %>% 
  mutate(GEOID = ifelse(location=="Norfolk-Portsmouth-Newport News VA", "47260", GEOID ))  %>% 
  mutate(GEOID = ifelse(location=="Philadelphia PA", "37980", GEOID )) %>%
  filter(!is.na(GEOID)) 

info <- ogrInfo("/Volumes/GoogleDrive/Shared drives/2020 FIRE-APHIS/OUTPUT/Shapefiles/metro.shp", require_geomType = "wkbPolygon")
metro<-readOGR("/Volumes/GoogleDrive/Shared drives/2020 FIRE-APHIS/OUTPUT/Shapefiles/metro.shp",info[["layer"]],
               require_geomType = "wkbPolygon")
col <- rev(brewer.pal(n=7, "RdYlBu"))
colorpal <- col[3:7]

df5 <- df4 %>% filter(keyword == "chicken coop")
trenddf <- geo_join(metro, df5, 'GEOID', 'GEOID')
trendmap<-trenddf[!is.na(trenddf@data$hits),] #remove empty values

title<-paste0("Hits")
tmap_mode("plot")
map1<-tm_shape(metro)+tm_borders("gray20", alpha=0.2)+
  tm_shape(trendmap)+tm_fill("hits",palette=colorpal, title=title, breaks=c(0,20,40,60,80,100))+
  tm_text("location", size=0.7, col="black", remove.overlap = TRUE)+
  tm_layout(title = "chicken coop",legend.text.size=0.7, legend.title.size=1)

df6 <- df4 %>% filter(keyword == "chicken tractor")
trenddf <- geo_join(metro, df6, 'GEOID', 'GEOID')
trendmap2<-trenddf[!is.na(trenddf@data$hits),] #remove empty values

map2<-tm_shape(metro)+tm_borders("gray20", alpha=0.2)+
  tm_shape(trendmap2)+tm_fill("hits",palette=colorpal, title=title, breaks=c(0,20,40,60,80,100))+
  tm_text("location", size=0.7, col="black")+
  tm_layout(title = "chicken tractor", legend.text.size=0.7, legend.title.size=1)

map<-tmap_arrange(map1, map2, nrow=2)
tmap_save(map,
          filename="G:/Shared drives/2020 FIRE-APHIS/INTERMEDIATE/trends_keeping.png",
          dpi=500, )
#####################################################################
words<-c("\"keeping chickens\"", 
         "\"chicken coop\"", 
         "\"chicken tractor\"")

metros<-c("US-DC-511", "US-MD-576", "US-MD-512", "US-TN-531", "US-WV-559", 
          "US-VA-573", "US-VA-569", "US-VA-584", "US-VA-556", "US-NA-544", "US-PA-504")

allwords<-c()

for (i in 1:length(words)){
  df<-c()
  for (j in 1:length(metros)) {
    
    tryCatch({
      temp<-gtrends(
        keyword = words[i],
        geo = metros[j],
        time = "2010-01-01 2019-12-31",
        cat = 66)
      
    }, error=function(err) {
      temp<-c()
    }, finally = {
      tbl<-as_tibble(temp$interest_over_time)
      df<-rbind(df,tbl)
    })
    
    Sys.sleep(1)
  }
  allwords<-rbind(df,allwords)
}
write.csv(allwords, "gtrends_keeping.csv")  

df<-fread("gtrends_keeping.csv")

df2<-df %>% mutate(location =  ifelse(geo=="US-DC-511", "Washington DC (Hagerstown MD)", NA )) %>% 
  mutate(location =  ifelse(geo=="US-MD-576", "Salisbury MD", location ))  %>% 
  mutate(location =  ifelse(geo=="US-MD-512", "Baltimore MD", location ))  %>% 
  mutate(location =  ifelse(geo=="US-WV-559","Bluefield-Beckley-Oak Hill WV", location ))  %>% 
  mutate(location =  ifelse(geo=="US-VA-573", "Roanoke-Lynchburg VA", location ))  %>% 
  mutate(location =  ifelse(geo=="US-VA-569", "Harrisonburg VA", location ))  %>% 
  mutate(location =  ifelse(geo=="US-VA-584", "Charlottesville VA", location ))  %>% 
  mutate(location =  ifelse(geo=="US-VA-556", "Richmond-Petersburg VA", location ))  %>% 
  mutate(location =  ifelse(geo=="US-NA-544", "Norfolk-Portsmouth-Newport News VA", location ))  %>% 
  mutate(location =  ifelse(geo=="US-PA-504", "Philadelphia PA", location )) %>%
  filter(!is.na(location)) %>%
  mutate(week=lubridate::isoweek(ymd(date))) 

locations<-c("Washington DC \n (Hagerstown MD)", 
             "Salisbury MD",
             "Baltimore MD", 
             "Tri-Cities TN-VA",
             "Bluefield-Beckley-Oak Hill WV",
             "Roanoke-Lynchburg VA",
             "Harrisonburg VA",
             "Charlottesville VA",
             "Richmond-Petersburg VA",
             "Norfolk-Portsmouth-\n Newport News VA",
             "Philadelphia PA")

col <- brewer.pal(n=11, "Spectral")
colorpal <- c(col[1],col[3],col[11])

plot_analysis <- list()
for (j in 1:length(metros)) {
  
  df4 <- df2 %>% filter(str_detect(geo,metros[j])) %>%
    mutate(keyword=str_replace_all(keyword, "[[:punct:]]", " "))
  
  if (nrow(df4)>0) {
    plot_analysis[[metros[j]]] <- ggplot(df4, aes(x = week, y = hits, group=keyword)) +
      geom_point(shape=1, aes(color=keyword))+
      geom_smooth(size=1,aes(color=keyword))+
      scale_color_manual(values=colorpal)+
      theme_minimal()+
      xlab("Week") +
      ylab("Hits") +
      ggtitle(paste("Hits in",locations[j]))+
      theme(
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        plot.title = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y= element_text(size = 8)
      )
  }
}
allplots<-ggarrange(plot_analysis[[1]],plot_analysis[[3]], 
                    plot_analysis[[4]],plot_analysis[[5]], 
                    nrow=2, ncol=2,common.legend = TRUE, legend="bottom")
ggsave("G:/Shared drives/2020 FIRE-APHIS/INTERMEDIATE/trendsplot_keeping.png", 
       allplots, dpi=500,
       height = 6, width=5, units="in")
