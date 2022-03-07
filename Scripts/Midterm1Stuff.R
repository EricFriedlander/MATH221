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



######################


glimpse(mammals)

mammals %>% 
  mutate(exposure_group = ifelse(exposure < 3, "Low", "High"),
         danger_group = ifelse(danger < 3, "Low", "High")) %>% 
  count(exposure_group, danger_group) %>%
  pivot_wider(names_from = danger_group, values_from = n, values_fill = 0)
mammals %>% 
  mutate(exposure_group = ifelse(exposure < 3, "Low", "High"),
         danger_group = ifelse(danger < 3, "Low", "High")) %>%
ggplot(aes(y=factor(danger_group), x=total_sleep)) +
  geom_boxplot()

mammals %>% 
  ggplot(aes(x=total_sleep)) +
  geom_histogram(bins=7)


mammals %>% 
  ggplot(aes( x=brain_wt)) +
  geom_boxplot()


mammals %>%
  summarize(mean(brain_wt),
            median(brain_wt),
            sd(brain_wt),
            IQR(brain_wt))

mammals_clean <- mammals %>% 
  mutate(
  log_brain_wt = log(brain_wt)
)
mammals %>%
  ggplot(aes(x=log(brain_wt), y=total_sleep)) +
  geom_point()

mammals %>%
  arrange(desc(brain_wt)) %>%
  select(species)

mammals %>% 
  drop_na() %>% 
  summarize(
    mean(log(brain_wt)),
    var(log(brain_wt)),
    mean(total_sleep),
    var(total_sleep))
  )

mammals_mod1 <- lm(total_sleep ~ log_brain_wt, data = mammals_clean)
summary(mammals_mod1)
mammals_mod1 %>%
  augment() %>% 
  ggplot(aes(x=log_brain_wt, y=.resid)) + 
  geom_point()



mammals <- mammals %>% 
  mutate(
    danger = factor(danger),
    exposure = factor(exposure),
    predation = factor(predation)
  )


mammals_mod1 <- lm(total_sleep ~ log_brain_wt + factor(exposure), data = mammals_clean)
summary(mammals_mod1)

mammals_mod2 <- lm(total_sleep ~ log_brain_wt + factor(predation), data = mammals_clean)
summary(mammals_mod2)

mammals_mod3 <- lm(total_sleep ~ log_brain_wt+ factor(danger), data = mammals_clean)
summary(mammals_mod3)

mammals_mod4 <- lm(total_sleep ~ log_brain_wt+ factor(danger) + factor(predation) + factor(exposure), data = mammals_clean)
summary(mammals_mod4)

mammals_mod4 %>%
  augment() %>%
  ggplot(aes(x=.fitted, y=.std.resid)) +
  geom_point()

library(leaps) 


mammals_selected <- mammals_clean %>% 
  select(-species, -brain_wt, -non_dreaming, -dreaming) %>% 
  mutate(
    danger = factor(danger),
    exposure = factor(exposure),
    predation = factor(predation)
  )

forward <- regsubsets(total_sleep ~ ., data=mammals_selected, method = "forward", nvmax=12)
with(summary(forward), data.frame(adjr2, outmat))

mammals %>% 
  mutate(new = gestation %% 1 == 0) %>% 
  count(new)
e
