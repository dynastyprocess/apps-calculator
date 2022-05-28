suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(here)
  library(lubridate)
  setwd(here::here())
})

scale_app <- function(n_containers){
  stopifnot(
    is.numeric(n_containers),
    length(n_containers) == 1,
    n_containers <= 5,
    n_containers >= 1)

  app_spec <- jsonlite::read_json("dev/app_spec.json")
  app_spec$services[[1]]$instance_count <- n_containers

  do_baseurl <- "https://api.digitalocean.com/v2/"
  do_auth <- function() httr::add_headers(Authorization = paste("Bearer",Sys.getenv("DO_PAT")))
  dpcalc_id <- "39852dd6-dd0b-433f-ab2b-e70be1933cda"

  json_spec <- list(spec = app_spec) |>
    jsonlite::toJSON(auto_unbox = TRUE)

  update_req <- httr::PUT(paste0(do_baseurl,"apps/",dpcalc_id),
                          do_auth(),
                          body = json_spec)

  httr::stop_for_status(update_req)

  force_redeploy <- httr::POST(paste0(do_baseurl,"apps/",dpcalc_id,"/deployments"),
                               do_auth(),
                               body = list(force_build = TRUE) |>
                                      jsonlite::toJSON(auto_unbox = TRUE)
                               )

  httr::stop_for_status(force_redeploy)

  invisible(NULL)
}

run_hour <- lubridate::hour(lubridate::with_tz(Sys.time(),tzone = "America/Toronto"))

# if it's after 9 am and before 10pm, three containers else one container
n_containers <- ifelse(run_hour >= 9 & run_hour < 22, 2, 1)

scale_app(n_containers)

