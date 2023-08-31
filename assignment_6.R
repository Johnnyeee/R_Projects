
# Part 1

vegetable <- readr::read_csv("vegetable_oil.csv")

#1
vegetable <- vegetable %>% 
  mutate(is_bean = ifelse(str_detect(crop_oil, "bean"), 1, 0),
         is_seed = ifelse(str_detect(crop_oil, "seed"), 1, 0),
         production = scales::comma(vegetable$production),
         united_states = ifelse(vegetable$entity == "United States",
                                "United States", "Other Countries"))


#2
vegetable <- vegetable %>% 
  tidyr::extract(crop_oil, into = "other_names", "(\\(copra\\))", remove = F)


#3
vegetable_summary <- vegetable %>% 
  summarize(max_production = max(as.numeric(gsub(",","",vegetable$production)), na.rm = T), 
            min_production = min(as.numeric(gsub(",","",vegetable$production)), na.rm = T))

#4
graphic <- vegetable %>%
  mutate(production = as.numeric(gsub(",","",vegetable$production))) %>%
  filter(is_bean == 1) %>% 
  group_by(year, united_states) %>% 
  summarize(avg_production =  mean(production, na.rm = T)) %>% 
  ggplot(aes(year, avg_production)) +
  geom_line() +
  geom_point(alpha = 0.2) +
  facet_wrap(~united_states) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Yearly Bean Production Over Time", subtitle = "From 1961-2014") +
  ggthemes::theme_fivethirtyeight()

#Part2 
#1
forest <- clean_names(readr::read_csv("forest.csv"))
forest_area <- clean_names(readr::read_csv("forest_area.csv"))
brazil_loss <- clean_names(readr::read_csv("brazil_loss.csv"))
soybean_use <- clean_names(readr::read_csv("soybean_use.csv"))
vegetable_oil <- clean_names(readr::read_csv("vegetable_oil.csv"))

#2
#a
forest_keys <- c("entity","code","year")
#b
forest_soybean<-left_join(forest,soybean_use)
#c
forest_soybean_intersection<-inner_join(soybean_use,forest)
#d
all_forest_data<-vegetable_oil%>%
  left_join(forest_area, by =forest_keys)%>%
  left_join(forest, by = forest_keys)%>%
  left_join(soybean_use, by =forest_keys)


#3
#a
world <- ggplot2::map_data("world")%>%
  filter(region!="Antarctica")
#b
forest_area_16<-forest_area %>%
  filter(year=="2016") %>%
  filter(!is.na(forest_area))
#c
differences <- lubridate::setdiff(world$region, forest_area_16$entity)
#d
us_standardizer <- function(x){
  replacement_string <- x
  for(word in replacement_string){
    if (str_to_lower(word) %in% c("united states of america", "united states", "us","usa")){
      replacement_string[which(replacement_string==word)] <- "United States"
    }
  }
  return(replacement_string)
}

#e
world<-world %>% mutate(region=us_standardizer(region))
#f
world_area<-left_join(world, forest_area_16,c("region"="entity"))

#g
world_area_graph <- world_area %>%
  ggplot(aes(x = long, y = lat, group = group, fill = forest_area)) + 
  geom_polygon(color = "black", size = 0.09) + 
  scale_fill_gradient(low = "yellow", high = "dark green") + 
  ggthemes::theme_map() + 
  labs(title = "Global Forest Area", subtitle = "(Year 2016)") +
  theme(legend.background=element_rect(fill = alpha("white", 0.5))) 
