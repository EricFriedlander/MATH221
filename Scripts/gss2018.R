gss2018_all <- gss_get_yr(2018)

gss2018 <- gss2018_all %>%
  mutate(
    race = as_factor(racecen1),
    age = cut(age,
              breaks=c(0, 30, 50, 65, 1000000), 
              labels=c("<= 30", "30-50", "50-65", ">65"), 
              include.lowest=TRUE,),
    degree = as_factor(degree),
    region = as_factor(region)) %>%
  select(c(age, race, degree, pres16)) %>%
  filter(pres16 == 1 | pres16 == 2 | pres16 == 3) %>%
  mutate(pres16 = as_factor(pres16),
         race = fct_collapse(race,
                             "asian or pacific islander" = c("asian indian",
                                                             "chinese",
                                                             "filipino",
                                                             "japanese",
                                                             "korean",
                                                             "other asian",
                                                             "guamanian or chamorro"),
                             "other" = c("some other race", "NA", "DK")
                             )
  ) %>%
  drop_na()

gss2018 <-slice_sample(gss2018, n=100000, replace=TRUE)
cell <- c("AT&T", "Verizon", "T-Mobile", "U.S. Cellular", "Dish Wireless")
gss2018$provider <- cell[sample(1:5, size=100000, replace=TRUE)]

write_csv(gss2018, file="Datasets/gss2018.csv")

