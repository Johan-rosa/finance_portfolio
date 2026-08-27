recv <- readr::read_csv("data/Hapi_2026-08-27.csv")
send <- readr::read_csv("data/tradingView.csv")

good_symbols <- recv$Symbol |> stringr::str_remove(".+:") |>
  unique()

send |>
  janitor::clean_names() |>
  dplyr::filter(!symbol %in% good_symbols) |>
  View()