library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(plotly)
library(tidyr)

gdp_per_capita <- read.csv("gdp-per-capita-worldbank.csv", stringsAsFactors = F)
continents <- read.csv("continents-according-to-our-world-in-data.csv", stringsAsFactors = F)
population_data <- read.csv("Population.csv", stringsAsFactors = F, skip = 3)
M49 <- read.csv("M49.csv", stringsAsFactors = FALSE)

#View(gdp_per_capita)
#View(continents)
#View(population_data)
#View(M49)


M49 <- M49 %>%
  select( Code = ISO.alpha3.Code,
          LDC = Least.Developed.Countries..LDC.,
          LLDC = Land.Locked.Developing.Countries..LLDC.,
          SIDS = Small.Island.Developing.States..SIDS.
  ) %>%
  mutate( LDC  = ifelse (LDC == "x", "LDC", "Non-LDC"),
          LLDC = ifelse (LLDC == "x", "LLDC", "Non-LLDC"),
          SIDS = ifelse (SIDS == "x", "SIDS", "Non-SIDS")
  ) %>%
  ungroup()

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

#View(population_long)


gdp_per_capita2 <- gdp_per_capita %>%
  rename(GDP_capita = GDP.per.capita..PPP..constant.2017.international... ) %>%
  left_join (continents %>% select (Code, Continent), by = "Code") %>%
  left_join (population_long %>% select (Code, Year, Population), by = c("Code", "Year")) %>%
  left_join(M49, by = "Code") %>%
  drop_na() 


LDC_continent_growth_line <- gdp_per_capita2 %>%
  group_by(Continent, LDC, Year) %>%
  summarise(weighted_gdp = weighted.mean(GDP_capita, w = Population, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(Continent, LDC) %>%
  arrange(Year) %>%
  mutate(
         weighted_gdp_growth = (weighted_gdp - lag(weighted_gdp)) / lag(weighted_gdp) * 100
  ) %>%
  drop_na()



plot_LDC_continent_line <- function(continent_name) {
  LDC_continent_growth_line <- LDC_continent_growth_line %>% 
    filter(Continent == continent_name,Year >= 1991)
  
  ggplot() +
    geom_line(data = LDC_continent_growth_line, aes(x = Year, y = weighted_gdp_growth, color = LDC, group = LDC),
      linewidth = 1.25
    ) +
    geom_hline(yintercept = 7, linetype = "dashed", color = "black", linewidth = 1) +
    annotate("text", x = max(LDC_continent_growth_line$Year), y = 7, label = "7%", 
             color = "black",vjust = 1.3, hjust = 1.1, size = 4) +
    labs(title = paste("Weighted GDP Growth in", continent_name, "- LDCs"),
         x = "Year",
         y = "Weighted GDP per Capita Growth Rate (%)"
          ) +
    scale_colour_manual(values = c("LDC" = "#B90A07", "Non-LDC" = "#015294")) +
    theme_economist_white() +
    theme(
      text = element_text(size = 11),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      axis.title.y = element_text(margin = margin(r = 10))
    )
}


plot_LDC_continent_line("Africa")
plot_LDC_continent_line("Asia")
plot_LDC_continent_line("North America")
plot_LDC_continent_line("Oceania")


#Bar chart

LDC_continent_growth_bar <- gdp_per_capita2 %>%
  filter(LDC == "LDC") %>%
  group_by(Continent, Year) %>%
  summarise( weighted_gdp = weighted.mean(GDP_capita, w = Population, na.rm = TRUE)
  ) %>%
  arrange(Continent, Year) %>%
  group_by(Continent) %>%
  mutate(
          weighted_gdp_growth = (weighted_gdp - lag(weighted_gdp)) / lag(weighted_gdp) * 100
  ) %>%
  ungroup()


plot_LDC_continent_bar <- LDC_continent_growth_bar %>%
  filter(!is.na(weighted_gdp_growth)) %>%
  plot_ly(
  x = ~Continent,
  y = ~weighted_gdp_growth,
  frame = ~Year,
  type = "bar",
  text = ~ paste(round(weighted_gdp_growth, 2), "%"),
  textposition = "outside",
  marker = list(color = "#B90A07"),
  showlegend = FALSE
  ) %>%
  layout(title = list(text = "Weighted GDP Growth of LDCs by Continent Over Time",
                      y = 0.97),
    xaxis = list(title = "Continent"),
    yaxis = list(title = "Weighted GDP Growth Rate (%)", range = c(-15,15)),
    shapes = list(
      list(type = "line",
           x0 = 0,
           x1 = 1,
           xref = "paper",
           y0 = 7,
           y1 = 7,
           line = list(color = "grey", width = 2, dash = "dash")
      )
    ),
    annotations = list(
      list(
        x = 1,
        xref = "paper",
        y = 8.5,
        text = "7%",
        showarrow = FALSE,
        font = list(color = "grey", size = 12)
      )))


plot_LDC_continent_bar
