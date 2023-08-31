#1
ibt_clean <- readr::read_csv("ibt_testdata.csv")
ibt_clean <- ibt_clean[-c(1,2),]

#2
race_groups_1<-ibt_clean %>%
  group_by(race_ethnicity) %>% 
  summarise(n())

race_groups <- tibble(Race_Asian = sum(str_detect(ibt_clean$race_ethnicity, "^Chinese|^Korean|^Asia|^asia|^korean|^chinese"),na.rm = T),
                      Race_Non_asian = 44 - sum(str_detect(ibt_clean$race_ethnicity, "^Chinese|^Korean|^Asia|^asia|^korean|^chinese"),na.rm = T),
                      Race_NA = 6)
race_groups_clean <- race_groups %>%
  pivot_longer(c('Race_Asian', 'Race_Non_asian', 'Race_NA'), names_to = "race_type",
             values_to = "value")

p <- ggplot(race_groups_clean, aes(x= reorder(race_type, -value), y = value,
                                   label = value))
p + geom_bar(stat="identity") +
  xlab("")+
  ylab("Counts of Race")+
  labs(title="Yujie Ye: Counts of Asian Race in Data Set",
       subtitle = "Econ 145 Fall 2021") +
  scale_x_discrete(labels=c("Race: Asian", "Race: Non-asian", "NA")) +
  geom_text(nudge_y = 0.8)+
  theme_minimal()

#3
graph<- ibt_clean %>%
  select(Q65_Page.Submit,Q66_Page.Submit,race_ethnicity)%>%
  mutate(hard_test = as.numeric(Q65_Page.Submit)) %>%
  mutate(easy_test = as.numeric(Q66_Page.Submit)) %>%
  select(hard_test,easy_test,race_ethnicity) %>%
  pivot_longer(c('hard_test', 'easy_test'), names_to = "test_type",
             values_to = "value")
graph1 <- graph %>%
  mutate(Asia_or_not = ifelse(str_detect(graph$race_ethnicity,
                                         "^Chinese|^Korean|^Asia|^asia|^korean|^chinese",T),0,1))
graph1 <- graph1 %>%
  mutate(Asian = ifelse(Asia_or_not == "1","Asian","Not_asian")) %>%
  drop_na() %>%
  mutate(test_type_2 = ifelse(test_type == "hard_test", "Hard Test", "Easy Test"))

supp.labs <- c("Easy Test", "Hard Test")

 
p3 <-ggplot(aes(x=Asian,y= value), data = graph1)
p3 + geom_boxplot()+
  facet_grid(. ~ test_type_2)+
  ylim(25,190)+
  xlab("")+
  ylab("Time to Completion (in seconds)")+
  labs(title="Yujie Ye: Difference in Completion Times") +
  theme_minimal() +
  scale_x_discrete(labels=c("Asian Ethnicity", "Non-Asian Ethnicity"))


