library(dplyr)
library(purrr)
library(ggplot2)
library(highcharter)

options(box.path = here::here())

box::use(
  scripts / get_stock_data[get_stock_data],
  scripts / read_portfolio[read_latest_portfolio, read_portfolio_history]
)

portfolio <- read_latest_portfolio()

stock_series <- portfolio |>
  pull(ticker) |>
  set_names() |>
  purrr::map(get_stock_data) |>
  list_rbind(names_to = "ticker")

# TODO: Define the logic to get the oldest purchase price for the current holdings 
# of each asset. Make sure to handle edge cases, such as when an asset was sold 
# and later bought again. Consider the period from the first time the asset appeared 
# in the portfolio up to the current date.

stocks <- stock_series |>
  filter(date >= "2025-01-01") |> 
  left_join(select(portfolio, ticker, quantity), by = "ticker") |>
  mutate(value = close * quantity) 

# Stock value
stocks |>
  summarise(assets = sum(value), .by = date) |>
  ggplot(aes(x = date, y = assets)) +
  geom_line() +
  theme_minimal() +
  labs(x = NULL, y = "US$") +
  scale_y_continuous(labels = scales::comma)

#Stock volatility
stocks |>
  summarise(
    assets = round(sum(value), 2),
    .by = date
  ) |>
  mutate(daily_returns = (assets / lag(assets) - 1) * 100) |> 
  hchart("line", hcaes(x = date, y = daily_returns), name = "Daily returns")