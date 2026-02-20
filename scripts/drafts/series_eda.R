library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

options(box.path = here::here())

box::use(
  scripts / get_stock_data[get_stock_data],
)

serie <- get_stock_data("ARKK")

return_lags <- tibble::tribble(
  ~period, ~lag,
  "5d",    5,
  "1m",    21,
  "6m",    126,
  "1y",    252
)

serie |>
  select(date, close) |> 
  mutate(
    lag_5 = lag(close, 5),
    lag_30 = lag(close, 30),
    lag_180 = lag(date, 180)
  ) |>
  tail()