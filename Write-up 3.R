library(tidyverse)
uof <- read_csv('uof_louisville.csv')

a <- c("empty hand control","empty hand strikes" )
uof <- uof |>
  mutate(most_force = 
           ifelse(force_used_8 %in% a ,1,0))

most_force_officer <- uof |>
  filter(most_force == 1) |>
  select(officer_name)
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
  group_by(officer_name, id, officer_involved) |>
  summarize(baoli = sum(violent_uof_1,violent_uof_2,violent_uof_3,
                         violent_uof_4,violent_uof_5,violent_uof_6,
                         violent_uof_7,violent_uof_8)) |>
  filter(baoli %in% c(2,3))

percentage <- most_violent |>
  group_by(officer_involved) |>
  count()
  
