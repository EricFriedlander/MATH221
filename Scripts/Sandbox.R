library(tidyverse)
library(openintro)

ggplot(data = corr_match, aes(x = x, y = y1)) +
  geom_point()

ggplot(data = corr_match, aes(x = x, y = y2)) +
  geom_point()

ggplot(data = corr_match, aes(x = x, y = y3)) +
  geom_point()

ggplot(data = corr_match, aes(x = x, y = y4)) +
  geom_point()

ggplot(data = corr_match, aes(x = x, y = y5)) +
  geom_point()

ggplot(data = corr_match, aes(x = x, y = y6)) +
  geom_point()

ggplot(data = corr_match, aes(x = x, y = y7)) +
  geom_point()

ggplot(data = corr_match, aes(x = x, y = y8)) +
  geom_point()

corr_match %>%
  summarize(cor(x, y1))

corr_match %>%
  summarize(cor(x, y2))

corr_match %>%
  summarize(cor(x, y3))

corr_match %>%
  summarize(cor(x, y4))

corr_match %>%
  summarize(cor(x, y5))

corr_match %>%
  summarize(cor(x, y6))

corr_match %>%
  summarize(cor(x, y7))

corr_match %>%
  summarize(cor(x, y8))

ggplot(bdims, aes(x=hgt, y=che_di)) +
  geom_point()

m <- .5
b <- -57



ggplot(bdims, aes(x=hgt, y=che_di)) +
  geom_point() +
  geom_abline(intercept = b, slope = m, color="red")

bdims <- bdims %>%
  mutate(
    y_pred = m * hgt + b,
    residuals = che_di - y_pred
  )

ggplot(bdims, aes(x=hgt, y=residuals)) + 
  geom_point()
