library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(reactable)

options(box.path = here::here())

box::use(
  scripts / get_stock_data[
    get_stock_data,
    update_portafolio_series,
    load_portfolio_series,
    read_portfolio_activity
  ],
)

logs <- read_portfolio_activity() |>
  filter_out(symbol == "AIRE")

# To update series, use this one instead
# prices <- update_portafolio_series()

prices <- load_portfolio_series()


positions_evolution <- function(portfolio_activity) {
  portfolio_activity |>
    dplyr::mutate(
      qty_signed = if_else(type == "buy", quantity, -quantity) |>
        round(4)
    ) |>
    dplyr::select(date, symbol, qty_signed) |>
    dplyr::arrange(symbol, date) |>
    dplyr::mutate(
      position = pmax(cumsum(qty_signed), 0),
      .by = symbol
    ) |>
    dplyr::select(-qty_signed)
}

expanded_positions_evolution <- function(portfolio_activity) {
  portfolio_activity |>
    positions_evolution() |>
    dplyr::group_by(symbol) |>
    tidyr::complete(
      date = seq(min(date), Sys.Date(), by = "day")
    ) |>
    dplyr::arrange(symbol, date) |>
    tidyr::fill(position, .direction = "down") |>
    tidyr::replace_na(list(position = 0)) |>
    dplyr::ungroup()
}

current_positions <- function(portfolio_activity) {
  portfolio_activity |>
    dplyr::mutate(
      signed_qty = dplyr::if_else(type == "buy", quantity, -quantity)
    ) |>
    dplyr::group_by(symbol) |>
    dplyr::summarise(
      quantity = sum(signed_qty),
      invested = sum(if_else(type == "buy", amount, 0)),
      .groups = "drop"
    ) |>
    dplyr::filter(round(quantity) > 0) |>
    dplyr::arrange(dplyr::desc(invested))
}


positions <- logs |> 
  positions_evolution()

daily_positions <- logs |>
  expanded_positions_evolution()

portfolio_daily <- daily_positions |>
  inner_join(
    prices |> select(symbol, date, price = adjusted),
    by = c("symbol", "date")
  ) |>
  mutate(value = position * price) |>
  filter(value > 0)

portfolio_evolution <- portfolio_daily |>
  summarise(portfolio_value = sum(value), .by = date)

cash_flows <- logs |>
  mutate(
    cash_flow = if_else(type == "buy", amount, -amount)
  ) |>
  summarise(net_flow = sum(cash_flow), .by = date)

portfolio_returns <- portfolio_evolution |>
  left_join(cash_flows, by = "date") |>
  mutate(net_flow = coalesce(net_flow, 0)) |>
  arrange(date) |>
  mutate(
    lag_value = lag(portfolio_value),
    return = (portfolio_value - lag_value - net_flow) / lag_value
  ) |>
  filter(!is.na(return)) |>
  mutate(
    cumulative_return = cumprod(1 + return) - 1
  )

return_lags <- tibble::tribble(
  ~period, ~lag,
  "ytd", 84,
  "5d", 5,
  "1m", 21,
  "6m", 126,
  "1y", 252
)

returns <- return_lags |>
  mutate(
    data = map(lag, \(l) {
      prices |>
        group_by(symbol) |>
        arrange(date) |>
        mutate(ret = adjusted / lag(adjusted, l) - 1) |>
        slice_tail(n = 1) |>
        select(symbol, ret)
    })
  ) |>
  select(period, data) |>
  unnest(data) |>
  pivot_wider(names_from = period, values_from = ret)


returns |>
  reactable(
    columns = list(
      symbol = colDef(),  # leave symbol as text
      `5d`  = colDef(cell = \(x) scales::percent(x, accuracy = 0.01)),
      `1m`  = colDef(cell = \(x) scales::percent(x, accuracy = 0.01)),
      `ytd`  = colDef(cell = \(x) scales::percent(x, accuracy = 0.01)),
      `6m`  = colDef(cell = \(x) scales::percent(x, accuracy = 0.01)),
      `1y`  = colDef(cell = \(x) scales::percent(x, accuracy = 0.01))
    ),
    sortable = TRUE,
    pagination = FALSE,
    highlight = TRUE
  )


library(ggplot2)

portfolio_returns |>
  ggplot(aes(x = date, y = cumulative_return)) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)

portfolio_evolution |> 
  ggplot(aes(date, portfolio_value)) +
    geom_line() +
    labs(
      title = "Portfolio Value",
      x = NULL,
      y = "USD"
    )

current_portfolio <- logs |>
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
  filter(round(quantity) > 0) |>
  arrange(desc(invested))

