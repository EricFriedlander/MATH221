library(tidyverse)
library(openintro)
library(tidymodels)
library(leaps)
library(palmerpenguins)

glimpse(births14)

?births14

births14_clean <- births14 %>%
  drop_na() %>%
  select(-lowbirthweight)

births14_clean %>% count(habit)

ggplot(births14_clean, aes(x=habit, y=weight)) +
  geom_bar(stat="summary", fun="mean")

births14_clean %>% 
  group_by(habit) %>%
  summarize(mean(weight))


habit_mod <- lm(weight ~ factor(habit), data = births14_clean)

summary(habit_mod)


ggplot(births14_clean, aes(x=weeks, y=weight, color=habit)) +
  geom_point()


births_mod <- lm(weight ~ habit + weeks, data = births14_clean)

summary(birth_mod)

augmented_mod <- augment(births_mod)

ggplot(augmented_mod, aes(x=weeks, y=weight, color=habit)) +
  geom_point() +
  geom_line(aes(y = .fitted))


set.seed(1988)
#Create training set
train <- births14_clean %>% sample_frac(.80)
#Create test set
test  <- anti_join(births14_clean, train)

train %>% count(habit)
test %>% count(habit)

# Fit model
births_mod <- lm(weight ~ habit + weeks + mature + mage, data = train)

# Predict test
test <- test %>%
  mutate(predictions = predict(births_mod, newdata=test))

# Compute sum of squared residuals
test %>%
  summarize(sse = sum((predictions - weight)^2))

# Summary
summary(births_mod)


# Fit model
births_mod <- lm(weight ~ fage+mage+mature+weeks+premie+visits+gained+sex+habit+marital+whitemom, data = births14_clean)

# Summary
summary(births_mod)

births_mod_full <- lm(weight ~ ., data = births14_clean)
b_mod1 <- stepAIC(births_mod_full)


backward <- regsubsets(weight ~ ., data=births14_clean, method = "backward", nvmax=12)
with(summary(backward), data.frame(adjr2, bic, cp, outmat))


forward <- regsubsets(weight ~ ., data=births14_clean, method = "forward", nvmax=12)
with(summary(forward), data.frame(adjr2, bic, cp, outmat))


backward <- regsubsets(weight ~ ., data=births14_clean, method = "exhaustive", nvmax=12)
with(summary(backward), data.frame(adjr2, bic, cp, outmat))

glimpse(penguins)
