#1
#a
fcc <- readr::read_csv("fcc_complaints_CA-2021.csv")
fcc<- janitor::clean_names(fcc)
#b
fcc_clean <- fcc %>%
  filter(zip != "00000") %>%
  filter(!is.na(method))

#c
fcc_clean <- fcc_clean %>%
  tidyr::extract(col = caller_id_number, into = "area_code",
                 regex = "(\\d\\d\\d)", remove = F)

#d
fcc_clean <- fcc_clean %>%
  mutate(month_of_issue = lubridate::month(date_of_issue),
         year_of_issue = lubridate::year(date_of_issue))

#e
fcc_clean <- fcc_clean %>%
  mutate(time_of_issue_clean = str_replace_all(fcc_clean$time_of_issue,
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
#f
call_distribution <- fcc_clean %>%
  filter(form == "Phone") %>%
  ggplot(aes(time_of_issue_clean, fill = method)) + ## edit the fill
  geom_histogram(alpha = 0.8) +
  theme_minimal() +
  scale_x_time(breaks = scales::date_breaks("3 hour"),
               labels = scales::time_format("%I:%M %p")) + ## edit the time_format
  theme(legend.position = "bottom") + ## edit this line
  labs(x = "", y = "Number of FCC Complaints", fill = "",
       title = "Distribution of Phone Complaints to the FCC",
       subtitle = "From January 2021 - July 2022",
       tag = "Figure 1") +
  ggthemes::scale_fill_fivethirtyeight()+
  theme(plot.margin = margin(t = 10, r = 10, b = 40, l = 10),
        plot.tag.position = c(0.5, -0.05)
  )
  
call_distribution

fcc_clean <- fcc_clean |>
  filter(form == "Phone")


#2

#a
acs <- readr::read_csv("acs_data.csv")

#b
acs <- acs %>%
  tidyr::extract(col = name, into = "zip",
                 regex = "(\\d\\d\\d\\d\\d)", remove = F)

#c
fcc_joined <- left_join(fcc_clean, acs , by = "zip")
fcc_joined <- fcc_joined %>%
  filter(total_pop != 0)

#d
fcc_joined <- fcc_joined %>%
  mutate(low_income_zip = ifelse(median_income < mean(fcc_joined$median_income,na.rm = T),
                                 "Below Average Median Income",
                                 "Above Average Median Income")) %>%
  mutate(high_age_zip = ifelse(median_age < mean(fcc_joined$median_age,na.rm = T),
                                 "Below Average Median Age",
                                 "Above Average Median Age"))

#e
aggregate <- fcc_joined %>%
  group_by(median_age, median_income, total_pop, zip, low_income_zip) %>%
  summarize(n_complaints = n())

#f
aggregate <- aggregate %>%
  mutate(complaints_per_1000 = (n_complaints / total_pop) *1000)

###
aggregate2 <- fcc_joined %>%
  mutate(city = tolower(city))|>
  group_by(city) %>%
  summarize(n_complaints = n())


a<-lm(data = aggregate, n_complaints ~ median_age+low_income_zip)
summary(a)

p <- ggplot(data = aggregate)
p + geom_col(mapping = aes(x = median_age, y = n_complaints,
                           fill = low_income_zip))+
  scale_y_log10()+
  theme(legend.position = "bottom")

p <- ggplot(data = aggregate)
p + geom_bar(mapping = aes(x =low_income_zip,
                           fill = low_income_zip))

c<-aggregate |>
  group_by(low_income_zip) |>
  summarise(count =n())
  
stargazer(a, title="Regression Results",
          dep.var.labels=c("Complaints Number"),
          covariate.labels=c("Median Age",
                             "Below Average Median Income"),
          omit.stat=c("LL","ser","f"), ci=TRUE, ci.level=0.90, single.row=TRUE)


