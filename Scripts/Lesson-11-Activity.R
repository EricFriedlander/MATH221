library(tidyverse)
library(infer)

# Define data
da_majors_first <- 8
nonda_majors_first <- 2
da_majors_not <- 5
nonda_majors_not <- 5

da_majors <- c(
  rep("Data Analytics", da_majors_first+da_majors_not),
  rep("Other", nonda_majors_first+nonda_majors_not)
)

first_born <- c(
  rep("First Born", da_majors_first),
  rep("Not First", da_majors_not),
  rep("First Born", nonda_majors_first),
  rep("Not First", nonda_majors_not)
)

data <- tibble(major = da_majors, first_born = first_born)
glimpse(data)


# Create Contingency Table
data %>%
  count(major, first_born) %>%
  pivot_wider(names_from=major, values_from = n)

# We want to know if First Born students are more likely to be DA majors
# Let's first write down our null and alternative hypotheses


data %>%
  group_by(first_born) %>%
  count(major, first_born) %>%
  mutate(proportion = n/sum(n)) %>%
  pivot_wider(id_cols=c(major, first_born, proportion), names_from=major, values_from = proportion)

# Point estimate of phat_fb - phat_nf
point_estimate <- .5 - .5
point_estimate


# Let's use R to permute for us
data %>%
  mutate(major_perm = sample(major)) %>%
  group_by(first_born) %>%
  count(major_perm, first_born) %>%
  mutate(proportion = n/sum(n)) %>%
  pivot_wider(id_cols=c(major_perm, first_born, proportion), names_from=major_perm, values_from = proportion)
 
data %>%
  rep_sample_n(size = nrow(data), reps=10000, replace = FALSE) %>%
  mutate(major_perm = sample(major)) %>%
  group_by(replicate, first_born) %>%
  summarize(prop_da = mean(major_perm == "Data Analytics")) %>%
  summarize(diff_da = diff(prop_da)) %>%
  ggplot(aes(x = diff_da)) +
  geom_histogram(binwidth=.1)


# Lets figure out our p-value

library(infer)

data %>%
  specify(major ~ first_born, success = "Data Analytics") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 5, type = "permute") %>%
  calculate(stat = "diff in props", order = c("First Born", "Not First"))


data %>%
  specify(major ~ first_born, success = "Data Analytics") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 10000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("First Born", "Not First")) %>%
  summarize(q.025 = quantile(stat, probs = 0.025), 
            q.05 = quantile(stat, probs = 0.05), 
            q.95 = quantile(stat, probs = 0.95), 
            q.975 = quantile(stat, probs = 0.975))
  

data %>%
  rep_sample_n(size = nrow(data), reps=10000, replace = FALSE) %>%
  mutate(major_perm = sample(major)) %>%
  group_by(replicate, first_born) %>%
  summarize(prop_da = mean(major_perm == "Data Analytics")) %>%
  summarize(diff_da = diff(prop_da)) %>%
  ggplot(aes(x = diff_da)) +
  geom_histogram(binwidth=.1) +
  geom_vline(aes(xintercept=-0.3), color="red")


point_estimate <- data %>%
  specify(major ~ first_born, success = "Data Analytics") %>%
  calculate(stat = "diff in props", order = c("First Born", "Not First"))
  

data %>%
  specify(major ~ first_born, success = "Data Analytics") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 10000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("First Born", "Not First")) %>%
  ggplot(aes(x = stat)) +
  geom_histogram(binwidth=.1) +
  geom_vline(aes(xintercept=-0.3), color="red")
  