library(piggyback)
library(nflreadr)
library(ggplot2)

current_urls <- piggyback::pb_download_url(
  repo = "dynastyprocess/apps-calculator",
  tag = "trade_data")

all_trades <- current_urls |>
  purrr::map_dfr(nflreadr::parquet_from_url) |>
  dplyr::mutate(week = lubridate::week(timestamp))

all_trades |>
  dplyr::group_split(week) |>
  purrr::map(function(.x){

    file_year <- lubridate::year(.x$timestamp[[1]])
    file_week <- .x$week[[1]]

    .x <- .x |> dplyr::select(-week)

    arrow::write_parquet(
      x = .x,
      sink = file.path(tempdir(check = TRUE), glue::glue("trades_{file_year}_{file_week}.parquet"))
    )
    invisible()
  })

list.files(tempdir(),pattern = "trades_",full.names = TRUE) |>
  piggyback::pb_upload("dynastyprocess/apps-calculator", tag = "weekly_trades")

current_weeks <- current_urls |>
  basename() |>
  gsub(pattern = ".+_([0-9]+\\.[0-9]+).+",replacement = "\\1") |>
  as.numeric() |>
  lubridate::as_datetime() |>
  lubridate::week()

basename(current_urls[current_weeks != lubridate::week(Sys.Date())]) |>
  piggyback::pb_delete("dynastyprocess/apps-calculator", tag = "trade_data")
