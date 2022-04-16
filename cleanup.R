suppressPackageStartupMessages({
  library(arrow)
  library(tidyverse)
  library(fs)
  library(httr)
  library(glue)
  library(piggyback)
  setwd(here::here())
})

cleanup <- function(){
  on.exit({
    POST("https://hc-ping.com/9345d7c2-eddc-4b99-b995-0717fc007e6a/fail",
         body = "Failed to cleanup trades from server!")
  })
  list_trades <- list.files("storage", recursive = TRUE, full.names = TRUE)

  map_dfr(list_trades, arrow::read_parquet) %>%
    write_parquet(glue::glue("storage/trades_{Sys.Date()}.parquet"))

  piggyback::pb_upload(glue::glue("storage/trades_{Sys.time()}.parquet"),
                       repo = "dynastyprocess/apps-calculator",
                       tag = "trade_data")

  list.files("storage", recursive = TRUE, full.names = TRUE) %>%
    fs::file_delete()

  on.exit(NULL)

  POST("https://hc-ping.com/9345d7c2-eddc-4b99-b995-0717fc007e6a",
       body = glue("Successfully cleaned out local data at {Sys.time()}"))

}
