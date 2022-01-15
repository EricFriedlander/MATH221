gss2002_all <- gss_get_yr(2002)

gss2002 <- gss2002_all %>%
  mutate(
    race = as_factor(racecen1),
    age = cut(age,
              breaks=c(0, 30, 50, 65, 1000000), 
              labels=c("<= 30", "30-50", "50-65", ">65"), 
              include.lowest=TRUE,),
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
                                                             "other pacific islander"),
                             "other" = c("some other race", "NA", "DK")
                             )
  ) %>%
  drop_na()

gss2002 <-slice_sample(gss2002, n=100000, replace=TRUE)
cell <- c("AT&T", "Verizon", "Sprint", "Comcast", "Time Warner")
gss2002$provider <- cell[sample(1:5, size=100000, replace=TRUE)]

write_csv(gss2002, file="Datasets/gss2012.csv")

