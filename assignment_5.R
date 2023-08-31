#1
fantano <- readr::read_csv("fantano.csv")

#2
#a)
library(dplyr)
library(tidyverse)
fantano_filter <- fantano %>%
  drop_na(total_length)

#b)
fantano_filter <- fantano_filter %>%
  mutate(album_length =
           str_replace(fantano_filter$total_length,"Total Length:", ""),
         album_length = 
           str_trim(album_length),
         album_length =
           str_to_lower(album_length))

#c)
fantano_filter <- fantano_filter %>%
  tidyr::extract(col = album_length, into = "album_minutes_only",
                 regex = "(\\d+)+(?= minute)", remove = F) %>%
  mutate(album_minutes_only = as.double(album_minutes_only))


#d)
fantano_filter <-fantano_filter %>%
  tidyr::extract(col = album_length, into = "hours",
                 regex = "(\\d+)+(?= hour)", remove = F) %>%
  mutate(hours_minutes = str_replace(hours, "1", "60")) %>%
  mutate(hours_minutes  = as.double(hours_minutes))

#e)
fantano_filter <-fantano_filter %>%
  rowwise() %>%
  mutate(total_length_minutes = sum(hours_minutes,album_minutes_only, na.rm=T))

#f)
longest_album <- fantano_filter %>%
  arrange(desc(total_length_minutes)) %>%
  head(1) %>%
  pull("album")

#3
#a)
fantano <- fantano %>%
  mutate(hip_hop = ifelse(str_detect(genres, "Hip Hop"),1,0))

#b)
fantano<- fantano %>%
  mutate(producer_number = stringr::str_count(producer, ',')+1)
  

#c)
most_producers <- fantano %>%
  arrange(desc(producer_number))%>%
  head(1) %>%
  pull("album")

#4
#a)
fantano_graph <- fantano %>%
  mutate(date = lubridate::mdy(release_date)) %>%
  separate(col = date, into = c("year","month","day"), sep = "\\-") %>%
  filter(year == "2022" & hip_hop == "1") %>%
  select(artist, album, fantano_rating)

#b)
fantano_graph  <- fantano_graph %>% 
  mutate(artist_album = glue::glue('{artist}: {album}'))

#c)
fantano_graph_subset <- fantano_graph %>%
  arrange(desc(fantano_rating)) %>%
  head(15)

#d)

fantano_graph_final <- fantano_graph_subset %>%
  mutate(artist_album = fct_reorder(artist_album,fantano_rating),
         data = fantano_graph_subset) %>% 
  ggplot(aes(x =fantano_rating, y = artist_album)) + 
  geom_col() + 
  coord_cartesian(xlim = c(60,90)) + 
  labs(x = "Fantano Score",
       y = "",
       title = "Top 15 Hip Hop Albums of 2022",
       subtitle = "Anthony Fantano Ratings") + 
  theme_minimal() 




