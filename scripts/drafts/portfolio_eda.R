library(dplyr)
library(purrr)

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

portfolio
  

