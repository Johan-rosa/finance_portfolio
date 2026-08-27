read_buy <- function(as_json = FALSE) {
  text <- clipr::read_clip() |> 
    stringr::str_subset(".")
  
  rows_with_values <- which(text |> stringr::str_detect("\\d|Buy|Sell"))
  
  type <- stringr::str_extract(text[1], "Buy|Sell")

  result <- dplyr::tibble(
    label = text[pmax(rows_with_values - 1, 1)],
    value = text[rows_with_values]
  ) |>
    dplyr::mutate(
      label = dplyr::case_when(stringr::str_detect(label, "Buy|Sell") ~ "ticker", .default = label),
      value = stringr::str_remove(value, "Buy |Sell ")
    )
  
  tidy_result <- tidyr::pivot_wider(result, names_from = label, values_from = value) |>
    janitor::clean_names() |> 
    dplyr::mutate(
      type = type,
      dplyr::across(
        dplyr::any_of(c("date_of_purchase", "date_of_sale")),
        lubridate::dmy_hm
      ),
      shares = readr::parse_number(shares),
      amount = readr::parse_number(amount),
      execution_price = readr::parse_number(execution_price)
    ) |>
    dplyr::select(
      date_time = dplyr::contains("date"),
      ticker,
      price = execution_price,
      quantity = shares,
      type
    )
  
  if (as_json) return(jsonlite::toJSON(tidy_result))
  tidy_result
}

(transactions <- read_buy())

(transactions <- dplyr::bind_rows(transactions, read_buy()))

clipr::write_clip(transactions)