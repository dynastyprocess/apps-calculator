get_month <- function(x) as.numeric(substr(as.character(x),6,7))

get_year <- function(x) as.numeric(substr(as.character(x),1,4))
