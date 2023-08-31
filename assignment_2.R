#Part 1 (1)
library(tidyverse)
festival_data <- read_csv("assign_2.csv")

#(2)
festival_data_1 <- festival_data %>%
  filter(X == 1)
n_repeated_1 <- nrow(festival_data_1)

#(3)
festival_data_unique <- festival_data %>%
  distinct()

#(4)
festival_data_unique <- festival_data_unique %>%
  mutate(extend_b = ifelse(extend == "Yes",1,0))

#(5) Private
extend_1_hours <- mean((festival_data_unique %>%
                          filter(extend_b == 1))$hours_attend, na.rm=T)

extend_0_hours <- mean((festival_data_unique %>%
                          filter(extend_b == 0))$hours_attend, na.rm=T)

extend_more_hours <- extend_1_hours-extend_0_hours

#(6)
f_data_spend_entertainment_total_amounts <- festival_data_unique %>%
  distinct(spend_entertainment_total) %>%
  drop_na()

#(7)
spend_more_100 <- c("$500-$999", "$200-$499")
festival_data_unique  <- festival_data_unique %>%
  mutate(s_entertainment_total_more_100 = 
           ifelse(spend_entertainment_total == "$500-$999"
                  |spend_entertainment_total =="$200-$499", 1, 0))
festival_data_unique$s_entertainment_total_more_100[is.na(festival_data_unique$s_entertainment_total_more_100)] <- 0

#(8) Private
spend_entertainment_total_more_100_women <- festival_data_unique %>%
  filter(gender=="Female")
spend_entertainment_total_more_100_women <- mean(spend_entertainment_total_more_100_women$s_entertainment_total_more_100, na.rm=T)

#(9)
spend_entertainment_total_more_100_demo <- festival_data_unique %>%
  group_by(gender,age) %>%
  summarize(se_t_m_100_demo = mean(s_entertainment_total_more_100))