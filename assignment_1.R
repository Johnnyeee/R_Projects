#1
column_one <- c(145,1,4,10,3)
column_two <- c("hello","welcome","to", "Econ","145")
column_one_first_position <- column_one[1]
column_two_fifth_position <- column_two[5]
column_three <- c(0,0,17, NA, 15)
tibble_one <- tibble(column_one,column_two,column_three)
data_type_one <- typeof(column_one_first_position)
data_type_two <- typeof(column_two_fifth_position)

#2
summary_stats_column_one <- c(mean(column_one), sd(column_one),
                              var(column_one))
summary_stats_column_three <- c(mean(column_three, na.rm = T),
                                sd(column_three, na.rm = T),
                              var(column_three, na.rm = T))

#3
library(tidyverse)
tidyverse_packages<- c("ggplot2", "purrr","tibble","dplyr","tidyr",
                        "stringr","readr","forcats")
school_crime=read_csv("assign_1.csv")
school_crime_colnames<- colnames(school_crime)
library(janitor)
school_crime <- clean_names(school_crime)
na_location<- is.na(school_crime$location)
