suppressPackageStartupMessages({
  library(data.table)
  library(cli)
  library(nflreadr)
  library(aws.s3)
  pkgload::load_all()
  setwd(here::here())
})

update_bucket_values <- function(){

  dpbucket <- aws.s3::get_bucket(bucket = 'dpcalc', region = "")

  players <- data.table::fread("https://raw.githubusercontent.com/dynastyprocess/data/master/files/values-players.csv")
  players <- players[,player := paste0(nflreadr::clean_player_names(player),", ",pos," ",team)]

  picks <- data.table::fread("https://raw.githubusercontent.com/dynastyprocess/data/master/files/values-picks.csv")
  picks <- picks[!is.na(pick),]
  prefill <- values_generate(players, picks)

  aws.s3::s3saveRDS(players, "values/players.rds", bucket = dpbucket, region = "")
  aws.s3::s3saveRDS(picks,   "values/picks.rds",   bucket = dpbucket, region = "")
  aws.s3::s3saveRDS(prefill, "values/prefill.rds", bucket = dpbucket, region = "")
}

update_bucket_values()
