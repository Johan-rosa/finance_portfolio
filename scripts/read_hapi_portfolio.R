#' Read portfolio from clipboard
#' 
#' @export
#' @example 
#' read_portfolio_from_clipboard() |> 
#'    saveRDS(file.path("data/portfolio", paste(Sys.Date(), "rds", sep = ".")))
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
    tidyr::pivot_wider(names_from = col_name, values_from = value) |>
    tidyr::separate(recent_change, into = c("change", "pchange"), sep = "\\s") |> 
    dplyr::mutate(
      quantity = readr::parse_number(quantity),
      value = readr::parse_number(value),
      current_price = value / quantity,
      change = readr::parse_number(stringr::str_remove(change, "\\$")),
      pchange = readr::parse_number(pchange) / 100,
      date = Sys.Date(),
    ) |>
    dplyr::relocate(date, .after = row)
}

#' Read saved portfolios
#' 
#' @export
read_portfolio_history <- function() {
  portfolios <- list.files("data/portfolio")
  
  file.path("data/portfolio", portfolios) |>
    purrr::set_names(fs::path_ext_remove(portfolios)) |> 
    purrr::map(readr::read_rds) |>
    purrr::list_rbind()
}


#' @export
read_latest_portfolio <- function() {
  list.files("data/portfolio/", full.names = TRUE) |>
    base::max() |>
    readr::read_rds()
}
