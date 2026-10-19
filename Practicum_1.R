# Title: Practicum_1.R
# Author: Fauzia Waqar
# Purpose: Replicate Figures 1 and 2 from Bartels (2000), "Partisanship and Voting Behavior," using the Cumulative CCES data, 2006-2024
# Requires: data/cumulative_2006-2024.rds in the working directory; packages tidyverse and haven installed
# Output: Plots to plot window; nothing to disk
library(tidyverse)
library(haven)
cc <- read_rds("data/cumulative_2006-2024.rds")
# Table for Figure 1
fig_1 <- cc |>
  select(year, pid7, weight) |>
  filter(year %in% 2006:2024) |>
  mutate(pid7 = as_factor(pid7)) |>
  mutate(pid7 = case_when(
    pid7 %in% c("Strong Democrat", "Strong Republican") ~ "Strong",
    pid7 %in% c("Not Very Strong Democrat", "Not Very Strong Republican") ~ "Weak",
    pid7 %in% c("Lean Democrat", "Lean Republican") ~ "Leaner",
    pid7 == "Independent" ~ "Pure independent"
  )) |>
  filter(!is.na(pid7)) |>
  group_by(year, pid7) |>
  summarize(total = sum(weight, na.rm = TRUE), .groups = "drop") |>
  group_by(year) |>
  mutate(proportion = total / sum(total)) |>
  ungroup()

# Figure 1
ggplot(fig_1, aes(x = year, y = proportion, linetype = pid7)) +
  geom_line() +
  scale_linetype_manual(values = c("Strong" = "solid", "Weak" = "dashed",
                                   "Pure independent" = "dotted", "Leaner" = "dotdash")) +
  labs(title = "The Distribution of Party Identification, 2006-2024")

fig_2<-cc|>
  select(year,pid7,weight,voted_turnout_self) |>
  filter(year %in% c(2008,2012,2016,2020,2024)) |>
  mutate(pid7 = as_factor(pid7),
         voted_turnout_self = as_factor(voted_turnout_self)) |>
     mutate(identifier = case_when(
      pid7 %in% c("Strong Democrat", "Not Very Strong Democrat",
                  "Not Very Strong Republican", "Strong Republican") ~ 1,
      pid7 %in% c("Lean Democrat", "Independent", "Lean Republican") ~ 0
    )) |>
  mutate(group = case_when(
    voted_turnout_self == "Yes" ~ "Voters",
    voted_turnout_self == "No" ~ "Nonvoters",
    is.na(voted_turnout_self) ~ NA
  )) |>
  filter(!is.na(identifier), !is.na(group)) |>
  group_by(year,group)|>
  summarize(proportion = sum(identifier * weight) / sum(weight))
ggplot(fig_2, aes(x = year, y = proportion, linetype = group)) +
  geom_line() +
  geom_point() +
  scale_linetype_manual(values = c("Voters" = "solid", "Nonvoters" = "dashed")) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Party Identification Among Presidential Voters and Nonvoters, 2008-2024")
  
  
  
  



