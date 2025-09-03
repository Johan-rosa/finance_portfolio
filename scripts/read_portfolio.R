#' Read portfolio from clipboard
#' 
#' @export
read_portfolio_from_clipboard <- function(text = clipr::read_clip_tbl(header = FALSE)) {
  columns <- data.frame(
    col_id = c(1, 2, 3, 0),
    col_name = c("ticker", "quantity", "value", "recent_change")
  )
  
  text |> 
    tibble::rowid_to_column() |> 
    dplyr::mutate(
      col_id = rowid %% 4,
      row = cumsum(col_id == 1)
    ) |>
    dplyr::left_join(columns) |>
    dplyr::select(row, col_name, value = V1) |>
    dplyr::filter(col_name != "recent_change") |> 
    tidyr::pivot_wider(names_from = col_name, values_from = value) |>
    dplyr::mutate(
      quantity = readr::parse_number(quantity),
      value = readr::parse_number(value),
      current_price = value / quantity,
      date = Sys.Date()
    ) |>
    dplyr::relocate(date, .after = row)
}

read_portfolio_from_clipboard() |>
  saveRDS(file.path("data/portfolio", paste(Sys.Date(), "rds", sep = ".")))

#' Read saved portfolios
#' 
#' @export
read_recent_portfolio <- function() {
  portfolios <- list.files("data/portfolio")
  
  file.path("data/portfolio", portfolios) |>
    purrr::set_names(fs::path_ext_remove(portfolios)) |> 
    purrr::map(readr::read_rds)
}
