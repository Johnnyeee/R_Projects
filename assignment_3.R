#1
library(tidyverse)
uof <- read_csv('uof_louisville.csv')

#2
#a
library(dplyr)
frequent_hour <- uof %>%
  drop_na(time_of_occurrence) %>%
  mutate(hour = lubridate::hour(time_of_occurrence)) %>%
  count(hour, sort = T) %>%
  slice(1) %>%
  pull(hour)

#b
least_frequent_month <- uof %>%
  mutate(month = lubridate::month(date_of_occurrence)) %>%
  count(month, sort = T)  %>%
  slice(12) %>%
  pull(month)


#c
most_frequent_day <- uof %>%
  mutate(day = lubridate::wday(uof$date_of_occurrence,
                               label = TRUE, abbr = TRUE)) %>%
  count(day, sort = T) %>%
  slice(1) %>%
  pull(day)

#d
day_distribution <- uof %>% 
  count(day = lubridate::day(date_of_occurrence), sort = T) %>%
  mutate(fraction = n/sum(n))%>%
  janitor::adorn_totals()

#3
#a
force_used_1 <- unique(uof$force_used_1)

#b
force_used_2 <- unique(uof$force_used_2)

#c
all_force <- uof %>%
  distinct(force_used_1,
           force_used_2,
           force_used_3,
           force_used_4,
           force_used_5,
           force_used_6,
           force_used_7,
           force_used_8) %>% 
  t() %>%
  c() %>%
  unique()
all_force

#d
violent_force <- c("take down", "hobble", "ecw cartridge deployed", "knee strike(s)",
                   "12 ga. sock round", "take-down", "impact weapon",
                   "kick", "deadly force used")

#e
uof <- uof %>%
  mutate(violent_uof_1 = ifelse(force_used_1 %in% violent_force, 1, 0))
mean(uof$violent_uof_1)
#f
violent_force_service_table <- uof %>%
  filter(violent_uof_1 == 1) %>%
  count(service_rendered,sort = T) %>%
  mutate(fraction = n/sum(n))%>%
  janitor::adorn_totals()

#4
#a
uof_filtered <- uof %>% 
  mutate(force_used_1_effective_binary = 
           ifelse(uof$force_used_1_effective == "yes",1,0)) %>%
  filter(citizen_gender == "male"|citizen_gender =="female") %>%
  filter(!is.na(citizen_race))


#b
uof_filtered_table <- uof_filtered %>%
  group_by(citizen_gender,citizen_race) %>%
  summarize(effective_1 = sum(force_used_1_effective_binary,na.rm=T)
            ,counts = n()) %>%
  janitor::adorn_totals() %>%
  mutate(fraction_effective = effective_1/counts)


#####################################
library(tidyverse)
uof <- read_csv('uof_louisville.csv')

a <- c("empty hand control","empty hand strikes" )
uof <- uof |>
  mutate(most_force = 
           ifelse(force_used_2 %in% a ,1,0))

most_force_officer <- uof |>
  filter(most_force == 1) |>
  select(officer_name, id, citizen_gender,citizen_race)
most_force_officer


#2
violent_force <- c("take down", "hobble", "ecw cartridge deployed", "knee strike(s)",
                   "12 ga. sock round", "take-down", "impact weapon",
                   "kick", "deadly force used")

uof_filtered <- uof %>% 
  mutate(violent_uof_1 = 
           ifelse(force_used_1 %in% violent_force,1,0)) %>%
  mutate(violent_uof_2 = 
           ifelse(force_used_2 %in% violent_force,1,0)) %>%
  mutate(violent_uof_3 = 
           ifelse(force_used_3 %in% violent_force,1,0)) %>%
  mutate(violent_uof_4 = 
           ifelse(force_used_4 %in% violent_force,1,0)) %>%
  mutate(violent_uof_5 = 
           ifelse(force_used_5 %in% violent_force,1,0)) %>%
  mutate(violent_uof_6 = 
           ifelse(force_used_6 %in% violent_force,1,0)) %>%
  mutate(violent_uof_7 = 
           ifelse(force_used_7 %in% violent_force,1,0)) %>%
  mutate(violent_uof_8 = 
           ifelse(force_used_8 %in% violent_force,1,0)) 


most_violent <- uof_filtered |>
  group_by(officer_name, id) |>
  summarize(baoli = sum(violent_uof_1,violent_uof_2,violent_uof_3,
                        violent_uof_4,violent_uof_5,violent_uof_6,
                        violent_uof_7,violent_uof_8))



injured_yes <- uof |>
  filter(officer_injured == "yes")

injured_no <- uof |>
  filter(officer_injured == "no")

ARMAacf(c(0,0,0.4))
polyroot(c(1,0,0,0.4))



