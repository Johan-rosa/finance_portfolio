recv <- readr::read_csv("data/Hapi_2026-08-27.csv")
send <- readr::read_csv("data/tradingView.csv")

good_symbols <- recv$Symbol |> stringr::str_remove(".+:") |>
  unique()

readxl::read_excel("data/portfolio_activity.xlsx") |>
  dplyr::filter(ticker %in% c("EQX", "ORCL", "NOW")) |>
  setNames(names(send)) |>
  readr::write_csv("data/moretransacionts.csv")

send |>
  dplyr::filter(Symbol == "EQX")
send |>
  janitor::clean_names() |>
  dplyr::filter(!symbol %in% good_symbols) |>
  View()