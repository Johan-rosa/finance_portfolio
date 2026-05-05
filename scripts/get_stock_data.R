#' Get ticker / symbol img
#' @export
img_url <- function(ticker) {
  glue::glue("https://hapi-ticker-images.s3.us-east-1.amazonaws.com/{ticker}.png")
}

#' Get stock historical data
#' @export
get_stock_data <- function(symbol, src = "yahoo", ...) {
  quantmod::getSymbols(symbol, src=src, auto.assign = FALSE, ...) |>
    as.data.frame() |>
    tibble::rownames_to_column("date") |>
    purrr::set_names(c("date", "open", "high", "low", "close", "volumen", "adjusted")) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      date = lubridate::ymd(date)
    )
}

#' @export
read_portfolio_activity <- function() {
  readxl::read_excel("data/portfolio_activity.xlsx") |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter_out(symbol == "SOLUSD")
}

#' @export
update_portafolio_series <- function() {
  logs <- read_portfolio_activity()
 
  tickers <- unique(logs$symbol)
  start_date <- min(logs$date)

  prices <- tickers |>
    purrr::set_names() |> 
    purrr::map(get_stock_data, .progress = TRUE) |>
    purrr::list_rbind(names_to = "symbol")

  saveRDS(prices, "data/series.rds")
  return(princes)
}

#' @export
load_portfolio_series <- function() {
  readRDS("data/series.rds")
}

