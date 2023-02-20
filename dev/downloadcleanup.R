future::plan(future::multisession)
downloc <- file.path(tempdir(),"trades")
dir.create(downloc,recursive = TRUE,showWarnings = FALSE)
progressr::with_progress({
  progressr::progressor(steps = length(keys))
  furrr::future_walk(
    keys,
    function(key){
      aws.s3::save_object(key,"dpcalc",file.path(downloc,basename(key)),region = "")
      p()
    }
  )
  })
