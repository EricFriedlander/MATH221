library(tidyverse)

covid <- read_csv("https://raw.githubusercontent.com/EricFriedlander/MATH221SP22/main/Datasets/simpsons_paradox_covid.csv") 

covid_under_50 <- covid %>%
  filter(age_group == "under 50") 

covid_50_plus <- covid %>%
  filter(age_group == "50 +")

covid_50_plus %>%
  specify(outcome~vaccine_status, success = "death") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("vaccinated", "unvaccinated")) %>%
  write_csv(file="Datasets/over_50_reps.csv")


covid_under_50 %>%
  specify(outcome~vaccine_status, success = "death") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("vaccinated", "unvaccinated")) %>%
  write_csv(file="Datasets/under_50_reps.csv")