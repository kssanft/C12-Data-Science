
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)

gdp_per_capita <- read.csv("gdp-per-capita-worldbank.csv", stringsAsFactors = F)
continents <- read.csv("continents-according-to-our-world-in-data.csv", stringsAsFactors = F)
population_data <- read.csv("Population.csv", stringsAsFactors = F, skip = 3)

View(gdp_per_capita)
View(continents)
View(population_data)

colnames(population_data) <- gsub("^X", "", colnames(population_data))
population_data <-  population_data[,-70]
#View(population_data)
population_long <- population_data %>%
  gather (key = "Year", value = "Population", `1960`:`2024`) %>%
  mutate(
    Year = as.numeric(Year),
    Population = as.numeric(Population)
  ) %>%
  rename (
    Entity = Country.Name,
    Code = Country.Code
  ) %>%
  drop_na()

View(population_long)


gdp_per_capita2 <- gdp_per_capita %>%
  rename(GDP_capita = GDP.per.capita..PPP..constant.2017.international... ) %>%
  left_join (continents %>% select (Code, Continent), by = "Code") %>%
  left_join (population_long %>% select (Code, Year, Population), by = c("Code", "Year")) %>%
  drop_na() %>%
  group_by(Year) %>%
  mutate(
    Development = case_when(
      ntile(GDP_capita, 3) == 1 ~ "Low",
      ntile(GDP_capita, 3) == 2 ~ "Medium",
      TRUE               ~ "High"
    )
  ) %>%
  ungroup() %>%
  mutate(
    Development = factor(Development,
                         levels = c("Low", "Medium", "High"))
  )


tertile_growth <- gdp_per_capita2 %>%
  group_by (Continent, Development, Year) %>%
  summarise( 
    weighted_gdp = weighted.mean (GDP_capita, w = Population, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by (Continent, Development) %>%
  arrange(Year) %>%
  mutate(
    weighted_gdp_growth = (weighted_gdp - lag(weighted_gdp)) /
      lag(weighted_gdp) * 100
  ) %>%
  drop_na()


plot_continent <- function(continent_name) {
  tertile_growth <- tertile_growth %>%
    filter (Continent == continent_name)
  
  
  ggplot(tertile_growth, aes(x = Year, y = weighted_gdp_growth, color = Development, group = Development)) +
    geom_line(linewidth = 1.25) +
    geom_hline(yintercept=7, linetype="dashed", color = "black", linewidth = 1) +
    annotate("text", x = max(tertile_growth$Year), y = 7, label = "7%", 
             color = "black",vjust = -0.5, hjust = 1.1, size = 4) +
    labs(
      title = paste ("Weighted GDP Growth Rate in", continent_name),
      x = "Year",
      y = "Weighted GDP per Capita Growth Rate (%)"
    ) +
    scale_colour_manual(values = c(
      "Low" = "#008F39",
      "Medium" = "#B90A07",
      "High" = "#015294"
    )) +
    theme_economist_white() +
    theme(
      text = element_text(size = 11),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      axis.title.y = element_text(margin = margin(r = 10))
    )
}


plot_continent("Europe")
plot_continent("Asia")
plot_continent("Africa")
plot_continent("North America")
plot_continent("South America")
plot_continent("Oceania")


low_growth <- tertile_growth %>%
  filter(Development == "Low")


ggplot(low_growth, aes(x = Year, y = weighted_gdp_growth, color = Continent, group = Continent)) +
  geom_line(linewidth = 1.25) +
  geom_hline(yintercept = 7, linetype = "dashed", color = "black", linewidth = 1) +
  annotate("text", x = max(low_growth$Year), y = 7, label = "7%", 
           color = "black", vjust = -0.5, hjust = 1.1, size = 4) +
  labs(
    title = "Weighted GDP Growth Rate - Low Development",
    x = "Year",
    y = "Weighted GDP per Capita Growth Rate (%)"
  ) +
  scale_colour_manual(values = c("Africa" = "#F5A623",
                                  "Asia" = "#B90A07",
                                  "Europe" = "#008F39",
                                  "North America" = "#015294",
                                  "Oceania" = "#7B1FA2",
                                  "South America" = "#E91E63"
  )) +
  theme_economist_white() +
  theme(
    text = element_text(size = 11),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.title.y = element_text(margin = margin(r = 10))
  )

#Version without Europe - more zoomed in 
low_growth2 <- tertile_growth %>%
  filter(Development == "Low" & Continent != "Europe")


ggplot(low_growth2, aes(x = Year, y = weighted_gdp_growth, color = Continent, group = Continent)) +
  geom_line(linewidth = 1.25) +
  geom_hline(yintercept = 7, linetype = "dashed", color = "black", linewidth = 1) +
  annotate("text", x = max(low_growth2$Year), y = 7, label = "7%", 
           color = "black", vjust = -0.5, hjust = 1.1, size = 4) +
  labs(
    title = "Weighted GDP Growth Rate - Low Development",
    x = "Year",
    y = "Weighted GDP per Capita Growth Rate (%)"
  ) +
  scale_colour_manual(values = c("Africa" = "#F5A623",
                                 "Asia" = "#B90A07",
                                 "North America" = "#015294",
                                 "Oceania" = "#7B1FA2",
                                 "South America" = "#E91E63"
  )) +
  theme_economist_white() +
  theme(
    text = element_text(size = 11),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.title.y = element_text(margin = margin(r = 10))
  )

