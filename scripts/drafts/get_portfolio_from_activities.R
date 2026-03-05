library(readxl)
library(dplyr)

logs <- read_excel("data/portfolio_activity.xlsx") |>
  mutate(date = as.Date(date))

logs |>
  mutate(
    signed_qty = if_else(type == "buy", quantity, -quantity)
  ) |>
  group_by(symbol) |>
  summarise(
    quantity = sum(signed_qty),
    invested = sum(if_else(type == "buy", amount, 0)),
    avg_price = invested / sum(if_else(type == "buy", quantity, 0)),
    .groups = "drop"
  ) |>
  filter(round(quantity, 1) > 0) |>
  clipr::write_clip(
    
  )


read_portfolio_from_clipboard() |> clipr::write_clip()
