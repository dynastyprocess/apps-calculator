#' Calculates Trade Values
# pkgload::load_all()
.datatable.aware <- TRUE

values_generate <- function(df_players,
                            df_picks,
                            qb_type = "1QB",
                            league_size = "12 teams",
                            value_factor = 235,
                            rookie_optimism = 75,
                            draft_type = "Normal",
                            future_factor = 80){

  ## minor preprocessing
  .league_size <- .parse_number(league_size)

  pick_values <- df_picks |>
    .calc_currentpicks( rookie_optimism, qb_type) |>
    .label_currentpicks(.league_size) |>
    .calculate_value(value_factor) |>
    .add_futurepicks(future_factor, .league_size) |>
    .filter_rookiepicks()

  player_values <- df_players |>
    .select_qbtype(qb_type) |>
    .calculate_value(value_factor)

  combined_values <- rbindlist(list(player_values,pick_values), fill = TRUE, use.names = TRUE) |>
    .label_draft_type(draft_type, .league_size)

  combined_values[order(-value),list(Player = player, Age = age, Value = value)]
}

## Value Subfunctions ----
.parse_number <- function(vec){
  as.numeric(gsub(pattern = "([0-9]+).+", replacement = "\\1",x = vec))
}

.str_pad <- function(vec) substr(paste0("0",vec),nchar(vec),nchar(vec)+1)

.calc_currentpicks <- function(df_picks, rookie_opt, qb_type){

  high_model <-  if(qb_type == "1QB") df_picks$ecr_high_1qb else df_picks$ecr_high_2qb
  low_model <- if(qb_type == "1QB") df_picks$ecr_low_1qb else df_picks$ecr_low_2qb
  high_factor <-  rookie_opt/100
  low_factor <-  1 - (rookie_opt/100)
  ecr <- high_factor * high_model + low_factor*low_model

  pick_values <- df_picks[, ecr := ecr]

  return(pick_values)
}

.label_currentpicks <- function(pick_values,leaguesize) {
  .league_size <- leaguesize + 0.001

  pick_values <- pick_values[
    ,`:=`(season = get_year(scrape_date),
          rookie_round = (pick %/% .league_size) + 1,
          round_pick = round(pick %% .league_size,0))
  ][, pick_label := paste0(season," Pick ",as.character(rookie_round),".",.str_pad(round_pick))
  ]
  return(pick_values)
}

.select_qbtype <- function(df,qb_type){
  v_ecr <- if(qb_type == "1QB") df$ecr_1qb else df$ecr_2qb

  df_q <- df[,ecr := v_ecr]
  return(df_q)
}

.calculate_value <- function(df,value_factor){
  .value_factor <- value_factor / 10000

  df_v <- df[,value := round(10500 * exp(-.value_factor * ecr))]
  return(df_v)
}


.add_futurepicks <- function(pick_values, future_factor, .league_size){
  .future_factor <- future_factor / 100
  .league_size <- .league_size + 0.001

  today_month <- get_month(Sys.Date())

  n1_factor <- data.table::fcase(
    today_month < 9, .future_factor,
    today_month <=10 , (1-.future_factor)*0.25 + .future_factor,
    today_month <=11 , (1-.future_factor)*0.50 + .future_factor,
    today_month <=12 , (1-.future_factor)*0.75 + .future_factor)

  n2_factor <- .future_factor

  n1 <- copy(pick_values)

  n1[
    ,`:=`(
      season = season + 1,
      rookie_round = data.table::fcase(rookie_round == 1, '1st',
                                       rookie_round == 2,  '2nd',
                                       rookie_round == 3,  '3rd',
                                       rookie_round >= 4,  paste0(rookie_round,'th')),
      eml = data.table::fcase(round_pick <= .league_size/3, 'Early',
                              round_pick <= .league_size*2/3, 'Mid',
                              default =  'Late')
    )
  ]


  n1_eml <- n1[
    ,.(value = mean(value) * n1_factor)
    ,keyby = .(season, rookie_round, eml)
  ][,pick_label := paste(season,eml,rookie_round)]

  n1_summary <- n1[
    ,list(value = mean(value) * n1_factor)
    ,by = .(season, rookie_round)
  ][,pick_label := paste(season,rookie_round)
  ]

  n2_summary <- copy(n1_summary)

  n2_summary[
    ,':='(season = season + 1,
          value = value * n2_factor)
  ][, pick_label := paste(season, rookie_round)
  ]

  n3_summary <- copy(n2_summary)

  n3_summary[
    ,':='(season = season + 1,
          value = value * n2_factor)
  ][,pick_label := paste(season, rookie_round)]

  df_picks <- data.table::rbindlist(list(pick_values,n1_eml,n1_summary,n2_summary,n3_summary),
                        fill = TRUE,
                        use.names = TRUE)

  df_picks[
    ,`:=`(
      player = pick_label,
      position = "PICK",
      value = round(value)
    )
  ][order(-value)]
}

.filter_rookiepicks <- function(df,today_date = Sys.Date()){
  today_month <- get_month(today_date)

  today_year <- get_year(today_date)

  if(today_month > 8){
    df <- df[season != today_year,]
  }

  df[, list(player, position, value)]

  return(df)
}

.label_draft_type <- function(df,draft_type, .league_size){
  .league_size <- .league_size + 0.001

  if(draft_type=='Normal') return(df)

  startup_picks <- copy(df)

  filter_vec <- switch(
    draft_type,
    'Startup (Players Only)' = startup_picks$pos != "PICK",
    'Startup (Players & Picks)' = (startup_picks$pos!='PICK' & startup_picks$draft_year != year(Sys.Date())) |
      grepl(x = startup_picks$player,pattern = year(Sys.Date()))
  )

  startup_picks <- startup_picks[
    filter_vec
  ][order(-value)
  ][, pick := seq_len(.N)
  ][,`:=`(startup_round = (pick  %/% .league_size)+1,
          startup_pick = round(pick %% .league_size,0),
          age = NA)
  ][,player := paste0("Startup Pick ",startup_round,".",.str_pad(startup_pick))
  ]

  rbindlist(list(startup_picks,df),fill = TRUE)[
    order(-value,player)
  ]
}
