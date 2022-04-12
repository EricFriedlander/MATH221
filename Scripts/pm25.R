library(tidyverse)

setwd(dir = "Datasets/")

cty2015 <- read_csv("ctyfactbook2015.csv")
cty2015 <- cty2015 %>% 
  rename(County_FIPS_Code = names(cty2015)[3],
                 PM2.5 =  names(cty2015)[12]) 
cty2015 <- cty2015 %>%   
  dplyr::select(State, County, County_FIPS_Code, PM2.5)%>% 
  mutate(PM2.5 = as.numeric(PM2.5)) %>% 
  drop_na()
          


cty2020 <- read_csv("ctyfactbook2020.csv")
cty2020 <- cty2020 %>% 
  rename(County_FIPS_Code = names(cty2020)[3],
         PM2.5 =  names(cty2020)[12]) 

cty2020 <- cty2020 %>%   
  dplyr::select(State, County, County_FIPS_Code, PM2.5) %>% 
  mutate(PM2.5 = as.numeric(PM2.5)) %>% 
  drop_na()

cty <- inner_join(cty2015, cty2020, by = c("State" = "State", "County" = "County", "County_FIPS_Code" = "County_FIPS_Code"), suffix = c("_2015", "_2020"))

cty %>% write_csv(file="ctyfactbook2015_2020.csv")


