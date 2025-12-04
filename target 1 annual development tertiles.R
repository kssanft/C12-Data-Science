library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)

gdp_per_capita <- read.csv("gdp-per-capita-worldbank.csv")
continents <- read.csv("continents-according-to-our-world-in-data.csv")

View(gdp_per_capita)
View(continents)

gdp_w_continents <- gdp_per_capita |>
  rename(GDP = GDP.per.capita..PPP..constant.2017.international... ) |>
  left_join (continents |> select (Code, Continent), by = "Code") |>
  drop_na()

View(gdp_w_continents)

library(dplyr)

## 2. Group the data set by the years and categorize the countries as high, med, and low depending on their respective global position that year 

gdp_annual_dev_tertiles <- gdp_w_continents |>
  filter(!is.na(GDP)) |>
  group_by(Year) |>
  mutate(
    Development = case_when(
      ntile(GDP, 3) == 1 ~ "Low",
      ntile(GDP, 3) == 2 ~ "Medium",
      TRUE               ~ "High"
    )
  ) |>
  ungroup() |>
  mutate(
    Development = factor(Development,
                         levels = c("Low", "Medium", "High"))
  )

View(gdp_annual_dev_tertiles)