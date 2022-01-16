library(tidyverse)
library(gssr)

gss2002_all <- gss_get_yr(2002)

gss2002 <- gss2002_all %>%
  mutate(
    race = as_factor(racecen1),
    age = cut_number(age, n=3),
    degree = as_factor(degree),
    region = as_factor(region)) %>%
  select(c(age, race, degree, pres00)) %>%
  filter(pres00 == 1 | pres00== 2 | pres00 == 3) %>%
  mutate(pres00 = as_factor(pres00),
         race = fct_collapse(race,
                             "asian or pacific islander" = c("asian indian",
                                                             "chinese",
                                                             "filipino",
                                                             "japanese",
                                                             "korean",
                                                             "other asian",
                                                             "guamanian or chamorro",
                                                             "vietnamese",
                                                             "samoan",
                                                             "other pacific islander",
                                                             "native hawaiian"),
                             "other" = c("some other race", "NA", "DK", "IAP")
                             )
  ) %>%
  drop_na()

set.seed(1234)
gss2002 <-slice_sample(gss2002, n=100000, replace=TRUE)
cell <- c("AT&T", "Verizon", "Sprint", "Comcast", "Time Warner")
gss2002$provider <- as_factor(cell[sample(1:5, size=100000, replace=TRUE)])
gss2002 <- droplevels(gss2002)

write_csv(gss2002, file="Datasets/gss2002.csv")

