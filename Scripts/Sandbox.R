library(tidyverse)
library(openintro)

covid <- read_csv("https://raw.githubusercontent.com/EricFriedlander/MATH221SP22/main/Datasets/simpsons_paradox_covid.csv")

glimpse(covid)

covid %>%
  count(age_group)

covid %>%
  count(outcome)

covid %>%
  count(outcome, age_group, vaccine_status)

covid %>%
  count(outcome, age_group) %>%
  mutate(prop = n/sum(n))

covid_sample <- covid %>%
  slice_sample(n=5)

covid_sample

covid_strat_sample <- covid %>%
  group_by(age_group) %>%
  slice_sample(n=5)

covid_strat_sample
