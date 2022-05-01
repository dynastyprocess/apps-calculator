suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(httr)
  library(glue)
  library(nflreadr)
  pkgload::load_all()
  setwd(here::here())
})

update_local_values <- function(){

  on.exit({
    httr::POST("https://hc-ping.com/9345d7c2-eddc-4b99-b995-0717fc007e6a/fail",
               body = "Failed to update calculator_values!")
  })

  players <- data.table::fread("https://raw.githubusercontent.com/dynastyprocess/data/master/files/values-players.csv")

  players <- players[,player := paste0(nflreadr::clean_player_names(player),", ",pos," ",team)]

  arrow::write_parquet(players, 'data/player_raw.parquet')

  picks <- data.table::fread("https://raw.githubusercontent.com/dynastyprocess/data/master/files/values-picks.csv")

  picks <- picks[!is.na(pick),]

  arrow::write_parquet(picks, 'data/picks_raw.parquet')

  prefill <- gen_df_values(players_raw, picks_raw)

  arrow::write_parquet(prefill,"data/prefill.parquet")

  on.exit(NULL)
  arrow::POST("https://hc-ping.com/9345d7c2-eddc-4b99-b995-0717fc007e6a",
       body = glue::glue("Successfully updated calculator values at {Sys.time()}"))
}

update_local_values()
