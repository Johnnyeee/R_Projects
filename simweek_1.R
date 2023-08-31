#1
ibt_clean <- readr::read_csv("ibt_testdata.csv")
ibt_clean <- ibt_clean[-c(1,2),]
#2
nationality_groups <- tibble(usa = 12,
                             not_usa = 30,
                             other = 8)
  
library(dplyr)
library(tidyverse)
library(ggplot2)
#3
graph<- ibt_clean %>%
  drop_na(Q65_Page.Submit, Q66_Page.Submit) %>%
  dplyr::select(Q65_Page.Submit, Q66_Page.Submit) %>%
  mutate(Insect_pleasant_first = as.numeric(Q65_Page.Submit)) %>%
  mutate(Flowers_pleasant_second = as.numeric(Q66_Page.Submit)) %>%
  dplyr::select(Insect_pleasant_first,Flowers_pleasant_second) %>%
  pivot_longer(c('Insect_pleasant_first', 'Flowers_pleasant_second'), names_to = "test_type",
               values_to = "value")
  
df_mean <- graph %>% 
  group_by(test_type) %>% 
  summarize(average = mean(value)) %>%
  ungroup()

p <- ggplot(aes(x= test_type,y= value), data = graph)
p.1 <- p + 
  geom_boxplot()+
  geom_point(data = df_mean,
             mapping = aes(x = test_type, y = average),
             color = "red") +
  ylim(0,200)+
  xlab("Test Types")+
  ylab("Numbers of Seconds")+
  labs(title="Plot of Two Tests Responses Time",
       subtitle = "(Without Outliers)") +
  theme_bw()

p1 <- ggplot(aes(x= test_type,y= value), data = graph)
p1.1 <- p1 + 
  geom_boxplot()+
  geom_point(data = df_mean,
             mapping = aes(x = test_type, y = average, color="Mean")) +
  scale_color_manual(name = "",
                     breaks=c('Mean'),
                     values=c('Mean'='red'))+
  ylim(0,700)+
  xlab("Test Types")+
  ylab("Numbers of Seconds")+
  labs(title="Plot of Two Tests Responses Time") +
  theme_bw()


figure <- ggarrange(p.1, p1.1,
                    ncol = 2, nrow = 1)
figure






