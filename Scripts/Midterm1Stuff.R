library(tidyverse)
library(openintro)
library(broom)

glimpse(smoking)

smoking %>%
  count(gender, smoke) %>% 
  pivot_wider(names_from = smoke, values_from = n)

smoking %>%
  group_by(gender) %>% 
  count(gender, smoke) %>% 
  mutate(prop = n/sum(n)) %>% 
  pivot_wider(id_cols = c(smoke, gender, prop), names_from = smoke, values_from = prop)

ggplot(smoking, aes(x = amt_weekdays)) +
  geom_histogram(binwidth = 5)

ggplot(smoking, aes(x = amt_weekdays, y=amt_weekends)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(smoking, aes(x = amt_weekdays, y=amt_weekends)) +
  geom_point()

smoking_mod <- lm(amt_weekends ~ amt_weekdays, data = smoking) 
summary(smoking_mod)

smoking_mod %>% 
  augment() %>%
  ggplot(aes(x=amt_weekdays, y=.resid)) +
  geom_point()

smoking_clean <- smoking %>% 
  filter(amt_weekends >10 | amt_weekdays < 40)

ggplot(smoking_clean, aes(x = amt_weekdays, y=amt_weekends)) +
  geom_point() +
  geom_smooth(method = "lm")

smoking_mod_clean <- lm(amt_weekends ~ amt_weekdays, data = smoking_clean) 
summary(smoking_mod)

smoking_mod_clean %>% 
  augment() %>%
  ggplot(aes(x=amt_weekdays, y=.resid)) +
  geom_point()

smoking_mod <- lm(amt_weekends ~ amt_weekdays + gender, data = smoking_clean) 
summary(smoking_mod)

smoking %>% 
  mutate(gross_income = as.factor(gross_income))

smoking %>% 
  ggplot(aes(x = amt_weekdays, y=gender)) +
  geom_boxplot()
