setwd("/Users/johnnys/Downloads/Econ 196AB/Thesis")
library(haven)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(foreign)
library(broom)
library(huxtable) 
core2021<- read_sav("2021core.sav")
core2019<- read_sav("2019core.sav")
core2018<- read_sav("2018core.sav")
bbuse_state <- read_csv("bbuse_state.csv")
bbuse_county <- read_csv("bbuse_county.csv")
media2020<- read_sav("ATP W74_2020.sav")
statename <- tibble(state = state.name, state_abb = state.abb) |>
  add_row(state = "District of Columbia", state_abb = "DC")

bbuse_state1 <- bbuse_state |>
  select(pctpopwbbacc, geography_desc) |>
  rename("state" = "geography_desc","pct" = "pctpopwbbacc")|>
  mutate(high = ifelse(pct >= 80, 1, 0))
bbuse_state2 <- merge(bbuse_state1, statename, by = "state")
  
  
bbuse_county <- bbuse_county |>
  select(-geom)


core2021fac <- core2021 |>
  as_factor()
core2019fac <- core2019 |>
  as_factor()
core2018fac <- core2018 |>
  as_factor()
media2020fac <- media2020 |>
  as_factor()


core2021fac <- merge(statename, core2021fac, by = "state")
core2019fac <- merge(statename, core2019fac, by = "state")
core2018fac <- merge(statename, core2018fac, by = "state")

int2021 <- merge(core2021fac, bbuse_state, by = "geography_desc")
int2019 <- merge(core2019fac, bbuse_state, by = "geography_desc")
int2018 <- merge(core2018fac, bbuse_state, by = "geography_desc")

c21 <- core2021fac |>
  select(sex, racecmb, income, age, educ2, bbhome1, intmob) |>
  mutate(access21 = ifelse(intmob == "Yes", 1, 0)) |>
  filter(income != "(VOL) Don't know/Refused")|>
  mutate(sex1 = ifelse(sex == "Female", 0, 1)) |>
  filter(racecmb != "Don't know/Refused (VOL.)") |>
  mutate(broadband = ifelse(bbhome1 == "Higher-speed", 1, 0)) |>
  filter(broadband == 1) |>
  group_by(state)|>
  summarise(count21 = n(), access_state21 = sum(access21),
            percent_2021 = access_state21/count21)

model21 = lm(access21 ~ sex1 + racecmb+ income+ age+ educ2, data = c21)
model21_2 = lm(access21 ~ sex1 +  age+ educ2, data = c21)
summary(model21)
anova(model21, model21_2)

c19 <- core2019fac |>
  select(sex, racecmb, inc, age, educ2, bbhome1, intmob) |>
  mutate(age = as.numeric(core2019fac$age))|>
  mutate(educ2 = as.numeric(core2019fac$educ2)) |>
  mutate(access21 = ifelse(intmob == "Yes", 1, 0)) |>
  filter(inc != "(VOL) Refused")|>
  filter(inc != "(VOL) Don't know")|>
  mutate(sex1 = ifelse(sex == "Female", 0, 1)) |>
  filter(racecmb != "Don't know/Refused (VOL.)") |>
  filter(educ2 != 9) 
  group_by(state)|>
  summarise(count19 = n(), access_state19 = sum(access19),
            percent_2019 = access_state19/count19)
model19 = lm(access21 ~ sex1 + racecmb+ inc+ age+ educ2, data = c19)
summary(model19)

c18 <- core2018fac |>
  select(sex, racecmb, inc, age, educ2, bbhome1, intmob) |>
  mutate(age = as.numeric(core2018fac$age))|>
  mutate(educ2 = as.numeric(core2018fac$educ2)) |>
  mutate(access21 = ifelse(intmob == "Yes", 1, 0)) |>
  filter(inc != "(VOL) Refused")|>
  filter(inc != "(VOL) Don't know")|>
  mutate(sex1 = ifelse(sex == "Female", 0, 1)) |>
  filter(racecmb != "Don't know/Refused (VOL.)") |>
  filter(educ2 != 9)  |>
  filter(educ2 != 10) 
  group_by(state)|>
  summarise(count18 = n(), access_state18 = sum(access18),
            percent_2018 = access_state18/count18)
model18 = lm(access21 ~ sex1 + racecmb+ inc+ age+ educ2, data = c18)
summary(model18)

c19_21 <- merge(c21, c19, by = "state")
c18_19_21 <- left_join(c19_21, c18 , by = "state")

c181921 <- c18_19_21 |>
  pivot_longer(c('percent_2021', 'percent_2019', 'percent_2018'),
               names_to = "year",
               values_to = "percent")


p <- ggplot(data = c181921,
            mapping = aes(x = year, y = percent))
p + geom_line(aes(group = state))+
  facet_wrap(~state)+
  scale_x_discrete(labels=c('2018','2019','2021'))+
  ylab("% of accessing the internet")+
  ggtitle( "Internet Access")

########################
c21_reg <- core2021fac |>
  select(cregion, state, usr, eminuse, intmob) |>
  mutate(access21 = ifelse(intmob == "Yes", 1, 0)) |>
  group_by(cregion)|>
  summarise(count21 = n(), access_reg21 = sum(access21),
            percent_2021 = access_reg21/count21)

c19_reg <- core2019fac |>
  select(cregion, state, usr, eminuse, intmob) |>
  mutate(access19 = ifelse(intmob == "Yes", 1, 0)) |>
  group_by(cregion)|>
  summarise(count19 = n(), access_reg19 = sum(access19),
            percent_2019 = access_reg19/count19)

c18_reg <- core2018fac |>
  select(cregion, state, usr, eminuse, intmob) |>
  mutate(access18 = ifelse(intmob == "Yes", 1, 0)) |>
  group_by(cregion)|>
  summarise(count18 = n(), access_reg18 = sum(access18),
            percent_2018 = access_reg18/count18)

c19_21_reg <- left_join(c21_reg, c19_reg, by = "cregion")
c18_19_21_reg <- left_join(c19_21_reg, c18_reg , by = "cregion")

c181921_reg <- c18_19_21_reg |>
  pivot_longer(c('percent_2021', 'percent_2019', 'percent_2018'),
               names_to = "year",
               values_to = "percent")
p <- ggplot(data = c181921_reg,
            mapping = aes(x = year, y = percent))
p + geom_line(aes(group = cregion))+
  facet_wrap(~cregion)+
  scale_x_discrete(labels=c('2018','2019','2021'))+
  ylab("% of accessing the internet")+
  ggtitle( "Internet Access")

##########
c21_usr <- core2021fac |>
  select(cregion, state, usr, eminuse, intmob) |>
  filter(usr != "")|>
  mutate(access21 = ifelse(eminuse == "Yes", 1, 0)) |>
  group_by(usr)|>
  summarise(count21 = n(), access_usr21 = sum(access21),
            percent_2021 = access_usr21/count21)

c19_usr <- core2019fac |>
  select(cregion, state, usr, eminuse, intmob) |>
  filter(usr != "")|>
  mutate(access19 = ifelse(eminuse == "Yes", 1, 0)) |>
  group_by(usr)|>
  summarise(count19 = n(), access_usr19 = sum(access19),
            percent_2019 = access_usr19/count19)

c18_usr <- core2018fac |>
  select(cregion, state, usr, eminuse, intmob) |>
  filter(usr != "")|>
  mutate(access18 = ifelse(eminuse == "Yes", 1, 0)) |>
  group_by(usr)|>
  summarise(count18 = n(), access_usr18 = sum(access18),
            percent_2018 = access_usr18/count18)

c19_21_usr <- left_join(c21_usr, c19_usr, by = "usr")
c18_19_21_usr <- left_join(c19_21_usr, c18_usr , by = "usr")

c181921_usr <- c18_19_21_usr |>
  pivot_longer(c('percent_2021', 'percent_2019', 'percent_2018'),
               names_to = "year",
               values_to = "percent")
p <- ggplot(data = c181921_usr,
            mapping = aes(x = year, y = percent))
p + geom_line(aes(group = usr))+
  facet_wrap(~usr)+
  scale_x_discrete(labels=c('2018','2019','2021'))+
  ylab("% of accessing the internet")+
  ggtitle(label = "Internet Usage", subtitle="R: rural; S: suburban; U: urban")

#################
demo21 <- core2021fac |>
  select(sex, racecmb, income, age, educ2,eminuse, intmob) |>
  filter(income != "(VOL) Don't know/Refused")|>
  mutate(access21 = ifelse(intmob == "Yes", 1, 0)) |>
  mutate(income = case_when(
    income == "Less than $10,000" ~ "under $30,000",
    income == "10 to under $20,000" ~ "under $30,000",
    income == "20 to under $30,000" ~ "under $30,000",
    income == "30 to under $40,000" ~ "$30,000 to $74,999",
    income == "40 to under $50,000" ~ "$30,000 to $74,999",
    income == "50 to under $75,000" ~ "$30,000 to $74,999",
    income == "75 to under $100,000" ~ "$75,000+",
    income == "100 to under $150,000" ~ "$75,000+",
    income == "$150,000 or more" ~ "$75,000+"))|>
  group_by(income)|>
  summarise(count21 = n(), access21 = sum(access21),
            percent_2021 = access21/count21)

demo19 <- core2019fac |>
  select(sex, racecmb, inc, age, educ2,eminuse, intmob) |>
  filter(inc != "(VOL) Refused")|>
  filter(inc != "(VOL) Don't know")|>
  mutate(access21 = ifelse(intmob == "Yes", 1, 0)) |>
  mutate(inc = case_when(
    inc == "Less than $10,000" ~ "under $30,000",
    inc == "10 to under $20,000" ~ "under $30,000",
    inc == "20 to under $30,000" ~ "under $30,000",
    inc == "30 to under $40,000" ~ "$30,000 to $74,999",
    inc == "40 to under $50,000" ~ "$30,000 to $74,999",
    inc == "50 to under $75,000" ~ "$30,000 to $74,999",
    inc == "75 to under $100,000" ~ "$75,000+",
    inc == "100 to under $150,000, OR" ~ "$75,000+",
    inc == "$150,000 or more?" ~ "$75,000+"))|>
  group_by(inc)|>
  summarise(count21 = n(), access21 = sum(access21),
            percent_2021 = access21/count21)

demo18 <- core2018fac |>
  select(sex, racecmb, inc, age, educ2,eminuse, intmob) |>
  filter(inc != "(VOL) Refused")|>
  filter(inc != "(VOL) Don't know")|>
  mutate(access21 = ifelse(intmob == "Yes", 1, 0)) |>
  mutate(inc = case_when(
    inc == "Less than $10,000" ~ "under $30,000",
    inc == "10 to under $20,000" ~ "under $30,000",
    inc == "20 to under $30,000" ~ "under $30,000",
    inc == "30 to under $40,000" ~ "$30,000 to $74,999",
    inc == "40 to under $50,000" ~ "$30,000 to $74,999",
    inc == "50 to under $75,000" ~ "$30,000 to $74,999",
    inc == "75 to under $100,000" ~ "$75,000+",
    inc == "100 to under $150,000, OR" ~ "$75,000+",
    inc == "$150,000 or more?" ~ "$75,000+"))|>
  group_by(inc)|>
  summarise(count21 = n(), access21 = sum(access21),
            percent_2021 = access21/count21)


#########
demo21 <- core2021fac |>
  select(sex, racecmb, income, age, educ2,eminuse, intmob) |>
  filter(age != 99)|>
  mutate(access21 = ifelse(intmob == "Yes", 1, 0)) |>
  mutate(age = case_when(
    age <= 29 ~ "18-29",
    age <= 49 ~ "30-49",
    age <= 64 ~ "50-64",
    age >= 65 ~ "65+"))|>
  group_by(age)|>
  summarise(count21 = n(), access21 = sum(access21),
            percent_2021 = access21/count21)

demo18$age<- as.numeric(as.character(demo18$age))

demo19 <- demo19 |>
  select(sex, racecmb, inc, age, educ2,eminuse, intmob) |>
  filter(age != "(VOL) Refused")|>
  filter(age != "(VOL) Don't know")|>
  mutate(access21 = ifelse(intmob == "Yes", 1, 0)) |>
  mutate(age = case_when(
    age <= 29 ~ "18-29",
    age <= 49 ~ "30-49",
    age <= 64 ~ "50-64",
    age >= 65 ~ "65+"))|>
  group_by(age)|>
  summarise(count21 = n(), access21 = sum(access21),
            percent_2021 = access21/count21)

demo18 <- demo18 |>
  select(sex, racecmb, inc, age, educ2,eminuse, intmob) |>
  filter(age != "(VOL) Refused")|>
  filter(age != "(VOL) Don't know")|>
  mutate(access21 = ifelse(intmob == "Yes", 1, 0)) |>
  mutate(age = case_when(
    age <= 29 ~ "18-29",
    age <= 49 ~ "30-49",
    age <= 64 ~ "50-64",
    age >= 65 ~ "65+"))|>
  group_by(age)|>
  summarise(count21 = n(), access21 = sum(access21),
            percent_2021 = access21/count21)


##########
c21_int <- core2021fac |>
  select(intmob) |>
  mutate(access21 = ifelse(intmob == "Yes", 1, 0)) |>
  summarise(count21 = n(), access_state21 = sum(access21),
            percent_2021 = access_state21/count21)

c19_int <- core2019fac |>
  select(intmob) |>
  mutate(access21 = ifelse(intmob == "Yes", 1, 0)) |>
  summarise(count21 = n(), access_state21 = sum(access21),
            percent_2021 = access_state21/count21)

c18_int <- core2018fac |>
  select(intmob) |>
  mutate(access21 = ifelse(intmob == "Yes", 1, 0)) |>
  summarise(count21 = n(), access_state21 = sum(access21),
            percent_2021 = access_state21/count21)

c19_21 <- merge(c21_int, c19_int, by = "state")
c18_19_21 <- left_join(c19_21, c18 , by = "state")

c181921 <- c18_19_21 |>
  pivot_longer(c('percent_2021', 'percent_2019', 'percent_2018'),
               names_to = "year",
               values_to = "percent")


p <- ggplot(data = c181921,
            mapping = aes(x = year, y = percent))
p + geom_line(aes(group = state))+
  facet_wrap(~state)+
  scale_x_discrete(labels=c('2018','2019','2021'))+
  ylab("% of accessing the internet")

############
library(arsenal)
table_one <- tableby(sex ~ ., data = core2021fac) 
summary(table_one, title = "Gapminder Data")
summary(core2021fac)

core_2021_demo <- core2021fac |>
  mutate(urs1 = as.factor(core2021fac$usr)) |>
  mutate(access21 = ifelse(intmob == "Yes", 1, 0))
core_2021_demo <- core_2021_demo |>
  select(lang, cregion, state, urs1, sex, income,racecmb,party, marital,educ2, access21)

summary(core_2021_demo)


############ DID
core2018fac1 <- core2018fac |>
  select(state, intmob) |>
  mutate(access = ifelse(intmob == "Yes", 1, 0)) |>
  group_by(state)|>
  summarise(int = sum(access), count = n(), )|>
  janitor::adorn_totals() |>
  mutate('2018' = int/count)

core2019fac1 <- core2019fac |>
  select(state, intmob) |>
  mutate(access = ifelse(intmob == "Yes", 1, 0)) |>
  group_by(state)|>
  summarise(int = sum(access), count = n(), )|>
  janitor::adorn_totals() |>
  mutate('2019' = int/count)

core2021fac1 <- core2021fac |>
  select(state, intmob) |>
  mutate(access = ifelse(intmob == "Yes", 1, 0)) |>
  group_by(state)|>
  summarise(int = sum(access), count = n(), )|>
  janitor::adorn_totals() |>
  mutate('2021' = int/count)

c19_21 <- merge(core2021fac1, core2019fac1, by = "state")
c18_19_21 <- left_join(c19_21, core2018fac1 , by = "state")


d <- c18_19_21 |>
  select('2018','2019','2021', state)|>
  rename("state_abb" = "state")
d <- merge(d, bbuse_state2, by = "state_abb")

d$'2018'[d$high == 0] <- 0
d$'2019'[d$high == 0] <- 0
d$'2021'[d$high == 0] <- 0

d_high <- d|>
  filter(high != 0)

mean(d_high$`2018`)
mean(d_high$`2019`)
mean(d_high$`2021`)

d_d <- c18_19_21 |>
  select('2018','2019','2021', state)|>
  rename("state_abb" = "state")
d_d <- merge(d_d, bbuse_state2, by = "state_abb")

d_d$'2018'[d_d$high == 1] <- 0
d_d$'2019'[d_d$high == 1] <- 0
d_d$'2021'[d_d$high == 1] <- 0

d_low <- d_d|>
  filter(high != 1)

mean(d_low$`2018`)
mean(d_low$`2019`)
mean(d_low$`2021`)


graph <- tribble(
  ~year,       ~mean,   ~type,
  2018,       0.810342, "high",
  2018,       0.8007473, "low",
  2019,       0.8658467,"high",
  2019,       0.8313438, "low",
  2021,       0.8750666,"high",
  2021,       0.8616122, "low",
)




data <- c18_19_21 |>
  select(state, '2018','2019','2021') |>
  pivot_longer(c('2018', '2019', '2021'),
               names_to = "year", values_to = "int")

data <- data |>
  mutate(year = as.numeric(data$year))|>
  rename("state_abb" = "state")

data1 <- merge(bbuse_state2, data, by="state_abb")

data2 <- data1 |>
  mutate(time = ifelse(year >= 2019, 1, 0)) |>
  mutate()

didreg <- lm( log(int) ~high*time, data = data2)
summary(didreg)

p <- ggplot(data = graph,
            mapping = aes(x = year, y = mean))
p + geom_line(aes(group = type))+
  scale_x_discrete(labels=c('2018','2019','2021'))+
  ylab("% of accessing the internet")

##根据15年的broadband percent然后分成high和low，然后取平均值，信息过少。


### DID 1.  access the internet on mobile handheld device at least occasionally.

#data wrangling
core2018fac1 <- core2018fac |>
  select(state, intmob, usr) |>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(access = ifelse(intmob == "Yes", 1, 0)) |>
  mutate(year = ifelse(check == "1", "2018",0))

core2019fac1 <- core2019fac |>
  select(state, intmob, usr) |>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(access = ifelse(intmob == "Yes", 1, 0)) |>
  mutate(year = ifelse(check == "1", "2019",0))

core2021fac1 <- core2021fac |>
  select(state, intmob, usr) |>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(access = ifelse(intmob == "Yes", 1, 0)) |>
  mutate(year = ifelse(check == "1", "2021",0))

#combine data
data <- rbind(core2018fac1,core2019fac1,core2021fac1)
#想着能不能把三个urban type变成numeric
data1 <- data |>
  rename("state_abb" = "state") |>
  mutate(state = as.numeric(factor(state_abb)))

data1 <- data1 |>
  filter(intmob != "(VOL) Don't know") |>
  filter(intmob != "(VOL) Refused")|>
  mutate(covid = ifelse(year == "2021",1,0))|>
  mutate(urban = ifelse(usr == "R",0,1))
#access = alpha + beta*year +gamma*Urban + Phi*year*urban
didreg1 <- lm(access ~ covid*urban, data = data1)
summary(didreg1)
huxreg(didreg1,didreg2)

##### 2. Do you currently subscribe to internet service at HOME?

core2018fac2 <- core2018fac |>
  select(state, home4nw, usr) |>
  filter(home4nw != "(VOL) Don't know") |>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(access = ifelse(home4nw == "Yes", 1, 0)) |>
  mutate(year = ifelse(check == "1", "2018",0))

core2019fac2 <- core2019fac |>
  select(state, home4nw, usr) |>
  filter(home4nw != "(VOL) Don't know") |>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(access = ifelse(home4nw == "Yes", 1, 0)) |>
  mutate(year = ifelse(check == "1", "2019",0))

core2021fac2 <- core2021fac |>
  select(state, home4nw, usr) |>
  filter(home4nw != "(VOL) Don't know") |>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(access = ifelse(home4nw == "Yes", 1, 0)) |>
  mutate(year = ifelse(check == "1", "2021",0))

data2 <- rbind(core2018fac2,core2019fac2,core2021fac2)|>
  mutate(covid = ifelse(year == "2021",1,0))|>
  filter(usr != "U")|>
  mutate(rural = ifelse(usr == "R",1,0)) |>
  mutate(suburban = ifelse(usr == "S",1,0))

didreg2 <- lm( access ~ covid*rural, data = data2)
summary(didreg2)

######### 3. access = Social Media

core2018fac3 <- core2018fac |>
  select( snsint2, usr) |>
  filter(snsint2 != "(VOL) Don't know") |>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(access = ifelse(snsint2 == "Yes", 1, 0)) |>
  mutate(year = ifelse(check == "1", "2018",0))|>
  select(-snsint2)

core2019fac3 <- core2019fac |>
  select( snsint2, usr) |>
  filter(snsint2 != "(VOL) Don't know") |>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(access = ifelse(snsint2 == "Yes", 1, 0)) |>
  mutate(year = ifelse(check == "1", "2019",0))|>
  select(-snsint2)


core2021fac3 <- core2021fac |>
  select( snsint2, usr) |>
  filter(snsint2 != "(VOL) Don't know") |>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(access = ifelse(snsint2 == "Yes", 1, 0)) |>
  mutate(year = ifelse(check == "1", "2021",0))|>
  select(-snsint2)

core2020fac3 <- media2020fac |>
  select( SNSUSE_W74, F_METRO) |>
  filter(SNSUSE_W74 != "(VOL) Don't know") |>
  mutate(check = ifelse(F_METRO == "Metropolitan"|
                          F_METRO == "Non-metropolitan",1,0))|>
  filter(check=="1")|>
  mutate(access = ifelse(SNSUSE_W74 == "Yes, I use social media sites", 1, 0)) |>
  mutate(year = ifelse(check == "1", "2020",0))|>
  mutate(usr = case_when(F_METRO =="Metropolitan" ~"U",
                         F_METRO == "Non-metropolitan" ~"R"))|>
  select(-SNSUSE_W74,-F_METRO)

data3 <- rbind(core2018fac3,core2019fac3,core2021fac3,core2020fac3) |>
  mutate(covid = case_when(year == "2020"~1,
                           year == "2021"~1,
                           TRUE ~ 0))   |>
  mutate(urban = ifelse(usr == "R",0,1))

didreg3 <- lm( access ~ covid*urban, data = data3)
summary(didreg3)

gdat3 = data3 |>
  group_by(urban, year) |>
  summarize(access = mean(access))|>
  mutate(urban = ifelse(urban==0,"Rural(control)","Urban(treat)"))


gg3 = ggplot(gdat3, aes(y=access,x=as.numeric(year), color = factor(urban))) +
  geom_point(aes(group = urban)) + 
  geom_line(aes(group = urban))+
  scale_fill_discrete(labels=c('High Program', 'Low Program'))+
  annotate(geom = "segment", x = 2019, xend = 2020,
           y = 0.709, yend = 0.823,
           linetype = "dashed", color = "blue")+
  annotate(geom = "segment", x = 2020, xend = 2021,
           y = 0.823, yend = 0.716,
           linetype = "dashed", color = "blue")+
  annotate(geom = "segment", x = 2020, xend = 2020,
           y = 0.74, yend = 0.823,
           linetype = "dotted", color = "blue")+
  geom_vline(xintercept=2019.98) +
  theme_bw() +
  ggtitle("Social Media Usage")+
  xlab('Year')+ylab('Social Media Use Percentage')+
  scale_color_manual(values=c('Red','Blue'))+
  theme(legend.title = element_blank())
gg3



######## 4. frequency of using the internet

core2018fac4 <- core2018fac |>
  select(state, intfreq, usr) |>
  mutate(freq = as.numeric(intfreq))|>
  filter(freq != "NA")|>
  filter(freq !="7")|>
  filter(freq !="6")|>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(year = ifelse(check == "1", "2018",0))

core2019fac4 <- core2019fac |>
  select(state, intfreq, usr) |>
  mutate(freq = as.numeric(intfreq))|>
  filter(freq != "NA")|>
  filter(freq !="7")|>
  filter(freq !="6")|>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(year = ifelse(check == "1", "2019",0))

core2021fac4 <- core2021fac |>
  select(state, intfreq, usr) |>
  mutate(freq = as.numeric(intfreq))|>
  filter(freq != "NA")|>
  filter(freq !="7")|>
  filter(freq !="6")|>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(year = ifelse(check == "1", "2021",0))

data4 <- rbind(core2018fac4,core2019fac4,core2021fac4) |>
  mutate(covid = ifelse(year == "2021",1,0))|>
  filter(usr != "U")|>
  mutate(rural = ifelse(usr == "R",1,0)) |>
  mutate(suburban = ifelse(usr == "S",1,0))

didreg4 <- lm( freq ~ covid*rural, data = data4)
summary(didreg4)

####### 5. High-speed or dial-up

core2018fac5 <- core2018fac |>
  select(state, bbhome1, usr) |>
  filter(bbhome1 != "(VOL) Don't know", bbhome1 != "(VOL) Refused") |>
  mutate(access = ifelse(bbhome1 %in% "Higher-speed",
                         1, 0)) |>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(year = ifelse(check == "1", "2018",0))

core2019fac5 <- core2019fac |>
  select(state, bbhome1, usr) |>
  filter(bbhome1 != "(VOL) Don't know", bbhome1 != "(VOL) Refused") |>
  mutate(access = ifelse(bbhome1 %in% "Higher-speed",
                         1, 0)) |>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(year = ifelse(check == "1", "2019",0))

core2021fac5 <- core2021fac |>
  select(state, bbhome1, usr) |>
  filter(bbhome1 != "(VOL) Don't know", bbhome1 != "(VOL) Refused") |>
  mutate(access = ifelse(bbhome1 %in% "Higher-speed",
                         1, 0)) |>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(year = ifelse(check == "1", "2021",0))

data5 <- rbind(core2018fac5,core2019fac5,core2021fac5) |>
  mutate(covid = ifelse(year == "2021",1,0))|>
  filter(usr != "U")|>
  mutate(rural = ifelse(usr == "R",1,0)) |>
  mutate(suburban = ifelse(usr == "S",1,0))

didreg5 <- lm(access ~ covid*rural, data = data5)
summary(didreg5)


#### 6. Use the internet or email at least occasionally

core2018fac6 <- core2018fac |>
  select(sex,inc,marital,state, eminuse, usr) |>
  filter(eminuse != "(VOL) Don't know", eminuse != "(VOL) Refused") |>
  mutate(access = ifelse(eminuse == "Yes",
                         1, 0)) |>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(year = ifelse(check == "1", "2018",0))


core2019fac6 <- core2019fac |>
  select(sex,inc,marital,state, eminuse, usr) |>
  filter(eminuse != "(VOL) Don't know", eminuse != "(VOL) Refused") |>
  mutate(access = ifelse(eminuse == "Yes",
                         1, 0)) |>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(year = ifelse(check == "1", "2019",0))

core2021fac6 <- core2021fac |>
  select(sex,income,marital,state, eminuse, usr) |>
  filter(eminuse != "(VOL) Don't know", eminuse != "(VOL) Refused") |>
  mutate(access = ifelse(eminuse == "Yes",
                         1, 0)) |>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(year = ifelse(check == "1", "2021",0))|>
  rename("inc"="income")
  

data6 <- rbind(core2018fac6,core2019fac6,core2021fac6) |>
  mutate(covid = ifelse(year == "2021",1,0)) |>
  mutate(urban = ifelse(usr == "R",0,1))

didreg6 <- lm(access ~ covid*urban, data = data6)
summary(didreg6)

didreg6_1<- lm(access ~ covid+urban+covid:urban, data = data6)
summary(didreg6_1)
anova(didreg6_1,didreg6)




didreg6_large <- lm(access ~ covid*urban+state, data = data6)
a<-tidy(didreg6)
b<-huxreg(didreg6, didreg6_large)


###Plot
gdat6 = data6 |>
  group_by(urban, year) |>
  summarize(access = mean(access))|>
  mutate(urban = ifelse(urban==0,"Rural(control)","Urban(treat)"))


gg6 = ggplot(gdat6, aes(y=access,x=as.numeric(year), color = factor(urban))) +
  geom_point(aes(group = urban)) + 
  geom_line(aes(group = urban))+
  scale_fill_discrete(labels=c('High Program', 'Low Program'))+
  annotate(geom = "segment", x = 2019, xend = 2021,
           y = 0.909, yend = 0.976,
           linetype = "dashed", color = "blue")+
  annotate(geom = "segment", x = 2021, xend = 2021,
           y = 0.936, yend = 0.976,
           linetype = "dotted", color = "black")+
  geom_vline(xintercept=2019.98) +
  theme_bw() +
  ggtitle("Do you use the internet or email, at least occasionally?")+
  xlab('Year')+ylab('Internet Use Percentage')+
  scale_color_manual(values=c('Red','Blue'))+
  theme(legend.title = element_blank())
gg6


###7. Book

core2018fac7 <- core2018fac |>
  select(state, books2a, books2c, usr) |>
  filter(books2a != "(VOL) Don't know", books2a != "(VOL) Refused") |>
  filter(books2c != "(VOL) Don't know", books2c != "(VOL) Refused") |>
  mutate(read_print = ifelse(books2a == "Yes",
                         1, 0)) |>
  mutate(read_e = ifelse(books2c == "Yes",
                             1, 0)) |>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(year = ifelse(check == "1", "2018",0))

core2019fac7 <- core2019fac |>
  select(state, books2a, books2c, usr) |>
  filter(books2a != "(VOL) Don't know", books2a != "(VOL) Refused") |>
  filter(books2c != "(VOL) Don't know", books2c != "(VOL) Refused") |>
  mutate(read_print = ifelse(books2a == "Yes",
                             1, 0)) |>
  mutate(read_e = ifelse(books2c == "Yes",
                         1, 0)) |>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(year = ifelse(check == "1", "2019",0))

core2021fac7 <- core2021fac |>
  select(state, books2a, books2c, usr) |>
  filter(books2a != "(VOL) Don't know", books2a != "(VOL) Refused") |>
  filter(books2c != "(VOL) Don't know", books2c != "(VOL) Refused") |>
  mutate(read_print = ifelse(books2a == "Yes",
                             1, 0)) |>
  mutate(read_e = ifelse(books2c == "Yes",
                         1, 0)) |>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(year = ifelse(check == "1", "2021",0))

data7 <- rbind(core2018fac7,core2019fac7,core2021fac7) |>
  mutate(covid = ifelse(year == "2021",1,0))|>
  filter(usr != "U")|>
  mutate(rural = ifelse(usr == "R",1,0)) |>
  mutate(suburban = ifelse(usr == "S",1,0))

didreg7 <- lm(read_e ~ covid*rural, data = data7)
summary(didreg7)

didreg8 <- lm(read_print ~ covid*rural, data = data7)
summary(didreg8)


######plot
gdat = data7 |>
  group_by(suburban, year) |>
  summarize(access = mean(read_e))


gg = ggplot(gdat, aes(y=access,x=as.numeric(year), color = factor(suburban))) +
  geom_point(aes(group = suburban)) + 
  geom_line(aes(group = suburban))+
  geom_vline(xintercept=2019.98) +
  theme_bw() +
  ggtitle("Did you read any e-book?")+
  xlab('Year')+ylab('Internet Use Percentage')+
  scale_color_manual(values=c('Red','Blue'))+
  annotate("text", x = 2020.3, y = 0.4, label = "COVID-19")+
  annotate("text", x = 2020.3, y = 0.515, label = "Urban", color = "Blue")+
  annotate("text", x = 2020.3, y = 0.455, label = "Rural", color = "Red")+
  theme(legend.position="none")
gg


dep<-c("twitter~","ins~","facebook~","snapchat~",
       "youtuybe~","whatsapp~","linkedin~") # list of unique dependent variables with ~ 
indep1<-c("covid")
indep2<-c("urban")# list of first unique independent variables 
myvar<-cbind(dep,indep1,indep2) # matrix of variables
myvar


################PART2:

core2021fac11_A <- W88fac |>
  select(COVINTESS_W88, UPGRADE_W88,F_AGECAT,F_GENDER,F_EDUCCAT,F_INC_TIER2,
         E_COVIDMOD_W88, F_METRO)|>
  filter_all(all_vars (. !="Refused"))|>
  mutate_if(is.factor, as.double)|>
  filter(F_GENDER != 3)|>
  mutate(E_COVIDMOD_W88 = case_when(E_COVIDMOD_W88 == 1~1,
                                    E_COVIDMOD_W88 == 2~1,
                                    TRUE ~ 0))|>
  mutate(F_GENDER = case_when(F_GENDER == 1~1,
                                    TRUE ~ 0))|>
  mutate(F_METRO = case_when(F_METRO == 1~1,
                              TRUE ~ 0))|>
  select(-UPGRADE_W88)

didreg11_FULL <- lm(COVINTESS_W88~F_AGECAT+F_GENDER+F_EDUCCAT
                    +F_INC_TIER2+E_COVIDMOD_W88+F_METRO,
               data = core2021fac11_A)
didreg11_1 <-lm(COVINTESS_W88~F_AGECAT+F_GENDER+F_EDUCCAT+E_COVIDMOD_W88+F_METRO,
                data = core2021fac11_A)
summary(didreg11_FULL)
summary(didreg11_1)
anova( didreg11_1,didreg11_FULL)
par(mfrow=c(2,2))
plot(didreg11_FULL)

mod0 <- lm(COVINTESS_W88~1, core2021fac11_A)
mod.upper <- lm(COVINTESS_W88 ~ F_AGECAT+F_GENDER+F_EDUCCAT+F_INC_TIER2
                +E_COVIDMOD_W88+F_METRO, core2021fac11_A)
step(mod0, scope = list(lower = mod0, upper = mod.upper),
     direction = "forward")

lmod <- lm(COVINTESS_W88 ~ ., core2021fac11_A)
step(lmod,
     direction = "backward")
####
core2021fac11_B <- W88fac |>
  select(COVINTESS_W88, UPGRADE_W88,F_AGECAT,F_GENDER,F_EDUCCAT,F_INC_TIER2,
         E_COVIDMOD_W88,F_METRO)|>
  filter_all(all_vars (. !="Refused"))|>
  mutate_if(is.factor, as.double)|>
  filter(F_GENDER != 3)|>
  mutate(UPGRADE_W88 = case_when(UPGRADE_W88 == 1~1,
                                    TRUE ~ 0))|>
  mutate(F_GENDER = case_when(F_GENDER == 1~1,
                              TRUE ~ 0))|>
  mutate(F_METRO = case_when(F_METRO == 1~1,
                             TRUE ~ 0))|>
  select(-COVINTESS_W88)

didreg11_B_FULL <- lm(UPGRADE_W88~F_AGECAT+F_GENDER+F_EDUCCAT+F_INC_TIER2
                      +E_COVIDMOD_W88+F_METRO,
                    data = core2021fac11_B)
didreg11_1_B <-lm(UPGRADE_W88~F_AGECAT+F_EDUCCAT+F_INC_TIER2,
                data = core2021fac11_B)
summary(didreg11_B_FULL)
summary(didreg11_1_B)
anova( didreg11_1_B,didreg11_B_FULL)

mod1 <- lm(UPGRADE_W88~1, core2021fac11_B)
mod.upper <- lm(UPGRADE_W88 ~ F_AGECAT+F_GENDER+F_EDUCCAT+F_INC_TIER2
                +E_COVIDMOD_W88+F_METRO, core2021fac11_B)
step(mod1, scope = list(lower = mod1, upper = mod.upper),
     direction = "forward")

par(mfrow=c(2,2))
plot(didreg11_1_B)



############ COVIDDIS:

core2021fac10 <- core2021fac|>
  select(coviddisa,coviddisb,coviddisc,coviddisd,coviddise, usr, sex,age,educ2,
         emplnw,income)|>
  filter_all(all_vars (. !="(VOL) Refused"))|>
  filter_all(all_vars (. !="(VOL) Don't know"))|>
  filter_all(all_vars (. !="(VOL) Don't know/Refused"))|>
  filter(age != 99)|>
  filter(educ2 != 99)|>
  mutate(check = ifelse(usr == "R"|usr == "S"|usr == "U",1,0))|>
  filter(check=="1")|>
  mutate(sex = case_when(sex == "Male"~1,
                              TRUE ~ 0))|>
  mutate(usr = case_when(usr == "R"~0,
                         TRUE ~ 1))|>
  mutate(work = case_when(emplnw == "Retired"~0,
                            emplnw== "(VOL) Disabled"~0,
                            emplnw=="(VOL) Other"~0,
                            TRUE ~1))|>
  mutate(income = case_when(income == "Less than $10,000"~1,
                             income== "10 to under $20,000"~1,
                             income=="20 to under $30,000"~1,
                            income == "30 to under $40,000"~2,
                            income == "40 to under $50,000"~2,
                            income == "50 to under $75,000"~2,
                            income == "75 to under $100,000"~2,
                            income == "100 to under $150,000"~3,
                            income == "$150,000 or more"~3,
                            TRUE ~0)) |>
  mutate_if(is.factor, as.double)
didreg10<-lm(cbind(coviddisa,coviddisb,coviddisc,coviddisd,coviddise)~
               usr+sex+age+educ2+work+income, data=core2021fac10)
summary(didreg10)

didreg10a<-lm(coviddisa~
                usr+sex+age+educ2+work+income, data=core2021fac10)
didreg10b<-lm(coviddisb~
                usr+sex+age+educ2+work+income, data=core2021fac10)
didreg10c<-lm(coviddisc~
                usr+sex+age+educ2+work+income, data=core2021fac10)
didreg10d<-lm(coviddisd~
                usr+sex+age+educ2+work+income, data=core2021fac10)
didreg10e<-lm(coviddise~
                usr+sex+age+educ2+work+income, data=core2021fac10)


tab_model(didreg10a,didreg10b,didreg10c,didreg10d,didreg10e, show.ci = FALSE,
          pred.labels = c("(Intercept)", "Urban", "Gender", "Age",
                          "Education", "Employment", 
                          "Income"),
          dv.labels = c("Getting latest information about COVID-19",
                        "Staying in contact with friends and family",
                        "Looking for jobs",
                        "Getting schoolwork done",
                        "Connecting with doctors or other medical professionals"))

d<-huxreg(didreg11_FULL,didreg11_1,didreg11_B_FULL,didreg11_1_B)
d

library(vtable)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

tab_model(didreg10a,didreg10b,didreg10c,didreg10d,didreg10e, show.ci = FALSE,
          pred.labels = c("(Intercept)", "Urban", "Gender", "Age",
                          "Education", "Employment", 
                          "Income"),
          dv.labels = c("Getting latest information about COVID-19",
                        "Staying in contact with friends and family",
                        "Looking for jobs",
                        "Getting schoolwork done",
                        "Connecting with doctors or other medical professionals"))

tab_model(didreg11_FULL,didreg11_1,didreg11_B_FULL,didreg11_1_B, show.ci = FALSE,
          pred.labels = c("(Intercept)", "Age", "Gender", "Education",
                          "Income", "Employment", 
                          "Urban"),
          dv.labels = c("Model1: The importance of the Internet to individuals",
                        "Model1_Reduced",
                        "Model2: Improve the Internet at home or not",
                        "Model2_Reduced"))
