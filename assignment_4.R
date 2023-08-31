#1
polls_adjusted <- readr::read_csv("covid_approval_polls_adjusted.csv")
#2
#a
library(dplyr)
polls_adjusted <- polls_adjusted %>%
  dplyr::group_by(subject) %>%
  dplyr::mutate(average_approval  = mean(approve_adjusted, na.rm=T)) %>%
  dplyr::ungroup()

#b
library(ggplot2)

approval_graph <- ggplot(data = polls_adjusted, aes(x = approve_adjusted)) +
  geom_histogram() +
  labs(title = "Adjusted Approval Ratings", x = "Approval",
       y = "Count") + 
  theme_minimal()
approval_graph

#c
approval_graph_facet <- polls_adjusted %>%
  filter(!is.na(approve_adjusted))%>%
  ggplot(aes(approve_adjusted, fill = subject)) +
  geom_histogram() +
  facet_wrap(~subject) +
  labs(x = "Approval", y = "Count", fill = "President",
       title = "American approval of Biden and Trump's response to coronavirus",
       subtitle = "From 2020-2022")+
  theme_minimal()
approval_graph_facet
#d
mean_line <- approval_graph_facet +
  geom_vline(aes(xintercept = average_approval),
             linetype='dashed')
mean_line

#e
approval_final <- polls_adjusted %>%
  ggplot(mapping = aes(approve_adjusted, fill = subject)) +
  geom_histogram() +
  facet_wrap(~subject) +
  labs(x = "Approval", y = "Count", fill = "President",
       title = "American approval of Biden and Trump’s response to coronavirus",
       subtitle = "From 2020-2022")+
  theme_minimal()+
  geom_vline(aes(xintercept = mean(polls_adjusted$approve_adjusted)),
             linetype="dashed")+
  scale_fill_manual(values = c("#008FD5","#FF2700"))+
  theme(legend.position = "bottom")
approval_final

#3
#a
polls_q3 <- polls_adjusted %>%
  mutate(end_date = lubridate::mdy(enddate)) %>%
  mutate(approve_fraction = approve_adjusted/100) %>%
  filter(party %in% c("D", "R", "I"))

#b
polls_q3 %>%
  ggplot(aes(x=end_date, y=approve_fraction, color=party))+
  geom_point(alpha = 0.1)+
  scale_color_manual(values = c("D" = "#008FD5", "R" = "#FF2700", 
                                "I" = "#77AB43")) +
  geom_smooth() +
  geom_vline(xintercept = as.numeric(lubridate::ymd("2021-01-20")),
             linetype="dashed") +
  scale_y_continuous(labels = scales::label_percent())+
  labs(x = NULL, y = NULL,
       title = "Yujie Ye: Approval of President’s Handling of Covid-19 Pandemic",
       subtitle = "From 2020-2022")+
  theme_minimal()

#4
#a
toplines <- readr::read_csv("covid_approval_toplines.csv")
#b
toplines_biden <- toplines %>%
  filter(subject == "Biden") %>%
  filter(party %in% c("D", "R", "I")) %>%
  mutate(model_date = lubridate::mdy(modeldate))
#c
toplines_biden <- toplines_biden %>%
  mutate(party_description = ifelse(party == "D", "Democrats",
                                    ifelse(party == "R", "Republicans","Independents"))) %>%
  mutate(approve_estimate_frac = approve_estimate/100)

#d
library(ggrepel)
library(scales)
library(lubridate)
final_graph <- toplines_biden %>%
  mutate(label = ifelse(model_date ==  max(model_date),
                        party_description, NA_character_)) %>% 
  ggplot(aes(x=model_date, y=approve_estimate_frac, group=party_description,
             color = party)) +
  geom_line() + 
  geom_text_repel(aes(label = label),
                  nudge_x = 10, na.rm = T,
                  xlim = as_date(c("2022-07-01", "2022-10-01"))) +
  geom_vline(aes(xintercept = as_date("2021-01-20")),
             linetype='dashed') +
  annotate("text", x = as_date("2021-01-20"), y = 0.05,
           label = "Biden sworn into office", size = 3,
           hjust = -0.1) +
  scale_color_manual(values = c("Democrats" = "#008FD5", "Republicans" = "#FF2700", 
                                 "Independents" = "#77AB43")) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(.1,1), clip = "off") +
  scale_x_date(limits = c(as_date("2020-12-01"), as_date("2022-10-01"))) +
  labs(x = "", y = "",
       title = "Do Americans approve of Biden response to the coronavirus crisis?",
       subtitle = "A calculation of the share of all Americans who approve of the handling of the coronavirus outbreak",
       ) +
  theme_minimal() +
  theme(legend.position = "none")
final_graph

  
