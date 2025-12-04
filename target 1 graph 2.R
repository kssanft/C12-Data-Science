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

## 1. For each country, find the year with GDP closest to 2000

closest_to_2000 <- gdp_w_continents |>
  filter(!is.na(GDP)) |>
  mutate(dist_to_2000 = abs(Year - 2000)) |>
  group_by(Code) |>
  slice_min(dist_to_2000, with_ties = FALSE) |>
  ungroup()

# optional: check what year was used per country
# View(closest_to_2000 |> select(Country, Year, GDP))

## 2. Use those closest-year GDP values to split countries into thirds

gdp_in_thirds <- closest_to_2000 |>
  arrange(GDP) |>
  mutate(
    Development = case_when(
      ntile(GDP, 3) == 1 ~ "Low",
      ntile(GDP, 3) == 2 ~ "Medium",
      TRUE               ~ "High"
    )
  )

## 3. Create a simple lookup table: Country -> Development

dev_lookup <- gdp_in_thirds |>
  select(Code, Development) |>
  distinct()

## 4. Join this lookup back onto your original gdp_per_capita data

gdp_per_capita_dev <- gdp_per_capita |>
  left_join(dev_lookup, by = "Code") |>
  mutate(
    Development = factor(Development,
                         levels = c("Low", "Medium", "High"))
  )

## 5. Inspect result

View(gdp_per_capita_dev)