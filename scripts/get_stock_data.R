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
    setNames(c("date", "open", "high", "low", "close", "volumen", "adjusted")) |>
    tibble::as_tibble()
}
