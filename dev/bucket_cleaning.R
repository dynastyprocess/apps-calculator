library(aws.s3)
library(dplyr)
library(furrr)

max <- 1e6
bucket <- "dpcalc"
marker <-  NULL
delimiter <- NULL
parse_response <- TRUE
region <- ""
prefix <- "trades"

query <- list(prefix = prefix, delimiter = delimiter, `max-keys` = as.integer(pmin(1000, max)), marker = marker)

r <- s3HTTP(verb = "GET", bucket = bucket, query = query, parse_response = parse_response, region = region)

i <- 1
out <- vector("list",1e3)
out[[i]] <- r[names(r)=="Contents"]
next_marker <- r$NextMarker

while (r[["IsTruncated"]] == "true" && !is.null(max) && as.integer(r[["MaxKeys"]]) < max) {
  query <- list(
    prefix = prefix,
    delimiter = delimiter,
    `max-keys` = as.integer(pmin(max - as.integer(r[["MaxKeys"]]), 1000)),
    marker = next_marker
  )
  i <- i+1
  if(i %% 10 == 0) cli::cli_alert_info(i)

  extra <- s3HTTP(verb = "GET", bucket = bucket, query = query, parse_response = parse_response, region = region)
  out[[i]] <- extra[names(extra)=="Contents"]

  r[["MaxKeys"]] <- as.character(as.integer(r[["MaxKeys"]]) + as.integer(extra[["MaxKeys"]]))
  r[["IsTruncated"]] <- extra[["IsTruncated"]]
  next_marker <- extra$NextMarker
}

keys <- out |>
  unlist(recursive = FALSE) |>
  purrr::map("Key") |>
  unlist() |>
  unname() |>
  tail(-1)

plan(multicore)
downloc <- file.path(tempdir(),"trades")
dir.create(downloc)
cli::cli_progress_bar("Download files",total = length(keys))

purrr::walk(keys, ~{
  aws.s3::save_object(.x,"dpcalc",file.path(downloc,basename(.x)),region = "")
  cli::cli_progress_update()
  })
)
