library(googleway)
library(data.table)
library(plyr)
library(stringr)

key = ""

places <-
  fread("G:/Shared drives/2020 FIRE-APHIS/INTERMEDIATE/places_combined.csv")

output <- as.data.frame(c())

for (j in 5502:nrow(places)) {
  place_id <- places$place_id[j]
  
  data <- google_place_details(place_id, key = key)
  
  add <- data$result$formatted_address
  phone <- data$result$formatted_phone_number
  web <- data$result$website
  reviews <- data$result$reviews
  status <- data$result$business_status
  
  reviews_text <- c()
  
  if (!is.null(reviews)) {
    for (i in 1:nrow(reviews)) {
      reviews_text <-
        paste(reviews_text, reviews[i, ]$text, sep = paste(" ", i, ") ", sep = ""))
    }
  }
  
  out <-
    as.data.frame(cbind(place_id, phone, web, status, reviews_text))
  output <- rbind.fill(output, out)
}

df <- merge(places, output, by = "place_id")
df <- unique(df)

write.csv(
  df,
  "G:/Shared drives/2020 FIRE-APHIS/INTERMEDIATE/places_reviews.csv",
  row.names = FALSE
)

df<-read.csv("G:/Shared drives/2020 FIRE-APHIS/INTERMEDIATE/places_reviews.csv")

df2<-df %>% mutate(n_chick=str_count(reviews_text, "chick")) %>%
  mutate(n_egg=str_count(reviews_text, "egg")) %>%
  mutate(n_poult=str_count(reviews_text, "poult")) %>%
  mutate(n_bird=str_count(reviews_text, "bird")) %>%
  mutate(allwords=n_chick+n_egg+n_bird+n_poult) %>%
  mutate(allwords=ifelse(is.na(allwords), 0, allwords))

write.csv(df2, "G:/Shared drives/2020 FIRE-APHIS/INTERMEDIATE/places_reviews.csv", row.names = FALSE)
