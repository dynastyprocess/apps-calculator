suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(fs)
  library(httr)
  library(glue)
  library(piggyback)
  setwd(here::here())
})

cleanup <- function(){
  on.exit({
    httr::POST("https://hc-ping.com/639523c7-5cfb-4503-b2eb-427959e039b1/fail",
         body = "Failed to cleanup trades from server!")
  })

  list_trades <- list.files("storage", recursive = TRUE, full.names = TRUE)

  upload_path <- glue::glue("storage/trades_{as.numeric(Sys.time())}.rds")

  arrow::write_parquet(
    data.table::rbindlist(lapply(list_trades, readRDS)),
    upload_path)

  piggyback::pb_upload(upload_path,
                       repo = "dynastyprocess/apps-calculator",
                       tag = "trade_data")

  fs::file_delete(list_trades)
  fs::file_delete(upload_path)

  on.exit(NULL)

  httr::POST("https://hc-ping.com/639523c7-5cfb-4503-b2eb-427959e039b1",
       body = glue::glue("Successfully cleaned out local data at {Sys.time()}"))
}

# cleanup()

suppressPackageStartupMessages({
  library(data.table)
  library(nflreadr)
  library(aws.s3)
  pkgload::load_all()
  setwd(here::here())
})

dpbucket <- aws.s3::get_bucket(bucket = 'dpcalc', region = "")
