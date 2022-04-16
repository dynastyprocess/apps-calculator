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
    POST("https://hc-ping.com/639523c7-5cfb-4503-b2eb-427959e039b1/fail",
         body = "Failed to cleanup trades from server!")
  })

  list_trades <- list.files("storage", recursive = TRUE, full.names = TRUE)

  upload_path <- glue::glue("storage/trades_{as.numeric(Sys.time())}.parquet")

  map_dfr(list_trades, arrow::read_parquet) %>%
    write_parquet(upload_path)

  piggyback::pb_upload(upload_path,
                       repo = "dynastyprocess/apps-calculator",
                       tag = "trade_data")

  list_trades %>%
    fs::file_delete()

  fs::file_delete(upload_path)

  on.exit(NULL)

  POST("https://hc-ping.com/639523c7-5cfb-4503-b2eb-427959e039b1",
       body = glue("Successfully cleaned out local data at {Sys.time()}"))
}

cleanup()
