
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
states <- map_data("state")

fcc <- readr::read_csv("fcc_complaints_CA-2021.csv")
fcc<- janitor::clean_names(fcc)

fcc <- fcc %>%
  mutate(time_of_issue_clean = str_replace_all(fcc$time_of_issue,
                                               c("[.]" = "", "AM" = "am",
                                                 "PM" = "pm",
                                                 "P>M>" = "pm",
                                                 "a m" = "am",
                                                 "2:8" = "2:08",
                                                 "pm/" = "pm",
                                                 "pm," = "pm",
                                                 "p,m" = "pm",
                                                 "am," = "am",
                                                 "a,m" = "am",
                                                 "p m" = "pm",
                                                 "11:1" = "11:01",
                                                 "pm>" = "pm",
                                                 "3:5" = "3:05",
                                                 "ama" = "am",
                                                 "pmp" = "pm",
                                                 "am," = "am",
                                                 "10:8" = "10:08"
                                               ))) %>%
  mutate(time_of_issue_clean = parse_time(time_of_issue_clean)) 

fcc_clean_spam_loc <- fcc %>%
  mutate(city = tolower(fcc$city))%>%
  filter(zip != "00000") %>%
  filter(issue == "Unwanted Calls") %>%
  select(time_of_issue_clean, issue, city, state, zip,
         location_center_point_of_the_zip_code)
fcc_clean_spam_loc <-fcc_clean_spam_loc%>%
  rename(subregion = city)

fcc_clean_spam_loc <- fcc_clean_spam_loc %>%
  separate(col = location_center_point_of_the_zip_code, 
           into = c("a", "b"), sep = ",")
fcc_clean_spam_loc <- fcc_clean_spam_loc %>%
  separate(col = a, 
           into = c("c", "lat"), sep = "\\(")
fcc_clean_spam_loc <- fcc_clean_spam_loc %>%
  separate(col = b, 
           into = c("long", "d"), sep = "\\)")
fcc_clean_spam_loc <- fcc_clean_spam_loc %>%
  mutate(lat = as.numeric(lat)) %>%
  mutate(long = as.numeric(long)) 

acs <- readr::read_csv("acs_data.csv")
acs <- acs %>%
  tidyr::extract(col = name, into = "zip",
                 regex = "(\\d\\d\\d\\d\\d)", remove = F)

fcc_joined <- left_join(fcc_clean_spam_loc , acs , by = "zip")

city_spam <- fcc_joined %>%
  group_by(city) %>%
  summarize(count = n())

time_spam <- fcc_joined %>%
  group_by(time_of_issue_clean) %>%
  summarize(count = n())

call_distribution <- time_spam %>%
  ggplot(mapping = aes(x = time_of_issue_clean)) +
  geom_histogram(alpha = 0.8, stat = "count") +
  theme_minimal() +
  scale_x_time(breaks = scales::date_breaks("3 hour"),
               labels = scales::time_format("%I:%M %p")) + ## edit the time_format
  theme(legend.position = "bottom") + ## edit this line
  labs(x = "", y = "Number of FCC Complaints", fill = "",
       title = "Distribution of Phone Complaints to the FCC",
       subtitle = "From January 2021 - July 2022") +
  ggthemes::scale_fill_fivethirtyeight()
call_distribution

ca_df <- subset(states, region == "california")
counties <- map_data("county")
ca_county <- subset(counties, region == "california")
ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
ca_base + theme_nothing()

ca_base + theme_nothing() + 
  geom_polygon(data = ca_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA) 

ca<- ggplot(data = fcc_clean_spam_loc, mapping = aes(x = long,y = lat))+
  coord_fixed(1.3)+
  geom_polygon(color = "black", fill = "gray")
ca + theme_nothing()
  
caspam <- left_join(fcc_clean_spam_loc,ca_county, by = "subregion")
caspam <- caspam %>%
  group_by(subregion) %>%
  mutate(count = n())

a <- caspam %>%
  group_by(subregion) %>%
  summarise(count = n())

  
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

fcc_clean_spam_loc <- fcc_clean_spam_loc %>%
  group_by(subregion) %>%
  mutate(count = n())

ggplot(fcc_clean_spam_loc, aes(long, lat)) + 
  geom_point(size = .25, show.legend = FALSE) +
  coord_quickmap()

elbow_room1 <- ca_base + 
  geom_polygon(data = fcc_clean_spam_loc,
               aes(long, lat, fill = count, group = subregion), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

elbow_room1 
elbow_room1 + scale_fill_gradient(trans = "log10")
  
  