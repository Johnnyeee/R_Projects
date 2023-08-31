covid <- readr::read_csv("covid_concern_toplines.csv")

covid <- covid |>
  mutate(date = lubridate::mdy(modeldate))

covid1 <- covid |>
  pivot_longer(c('very_estimate',
                 'somewhat_estimate',
                 'not_very_estimate',
                 'not_at_all_estimate'), names_to = "concern",
               values_to = 'percentage')

label_name <- list(
  'not_at_all_estimate' = "Not at all Concern",
  'not_very_estimate' = "Not very Concern",
  'somewhat_estimate' = "Somewhat Concern",
  'very_estimate' = "Very Concern"
)
concern_labeller <- function(variable,value){
  return(label_name[value])
}

p<-ggplot(covid1, mapping = aes(x = date, y = percentage, color = subject))
p+geom_line(aes(group = subject))+
  facet_wrap(~ concern, labeller = concern_labeller)+
  labs(x = "", y="percentage",fill = "subject",
       title = "Covid-19: Concerning economy or infected in different levels",
       subtitle = "From 2020.02.15 to 2022.03.02",
       caption = "Source: FiveThirtyEight")+
  theme_bw()+
  theme(legend.title = element_blank())