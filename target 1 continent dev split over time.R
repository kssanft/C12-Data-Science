# animated?? bar chart showing the percentage split of the continents (low, med, high) over time
library(dplyr)
install.packages("plotly")
library(plotly)
View(gdp_annual_dev_tertiles)

#using this data set, we can change it so its organized by percentage split
# i want a data set that is basically continent | year | % high | % med | % low 

annual_dev_distribution <- gdp_annual_dev_tertiles |>
  group_by(Year, Continent) |>
  summarize(
    percent_low = (sum(Development == "Low") / n_distinct(Code)) * 100,
    percent_med = (sum(Development == "Medium") / n_distinct(Code)) *100,
    percent_high = (sum(Development == "High") / n_distinct(Code)) *100
  ) |>
  ungroup() |>
  arrange(Year, Continent)


View(annual_dev_distribution)

dev_plot <- annual_dev_distribution |>
  plot_ly(
    x     = ~Continent,
    frame = ~Year         # this creates the slider / animation over time
  ) |>
  add_bars(
    y    = ~percent_low,
    name = "Low"
  ) |>
  add_bars(
    y    = ~percent_med,
    name = "Medium"
  ) |>
  add_bars(
    y    = ~percent_high,
    name = "High"
  ) |>
  layout(
    barmode = "stack",
    title   = "Share of countries by development level and continent over time",
    xaxis   = list(title = "Continent"),
    yaxis   = list(title = "Share of countries (%)")
  )

dev_plot