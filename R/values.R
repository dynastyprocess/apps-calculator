#' Calculates Trade Values

qb_type = "1QB"
league_size = "12 teams"
value_factor = 235
rookie_optimism = 75
draft_type = "Normal"
future_factor = 80
.datatable.aware=TRUE
values_generate <- function(players_raw,
                            picks_raw,
                            qb_type = "1QB",
                            league_size = "12 teams",
                            value_factor = 235,
                            rookie_optimism = 75,
                            draft_type = "Normal",
                            future_factor = 80){

  ## minor preprocessing
  .league_size <- .parse_number(league_size)

  pick_values <- picks_raw |>
    .calc_currentpicks( rookie_optimism, qb_type) |>
    .label_currentpicks(.league_size) |>
    .calculate_value(value_factor) |>
    .add_futurepicks(future_factor, .league_size) |>
    .filter_rookiepicks()

  # pick_values <- .calculate_value(pick_values, value_factor)
  # pick_values <-
  #   .label_currentpicks(parse_number(league_size)) %>%
  #   .calculate_value(value_factor) %>%
  #   .add_futurepicks(future_factor,parse_number(league_size)) %>%
  #   .filter_rookiepicks() %>%
  #   select(player = pick_label,value)

  players_raw %>%
    .select_qbtype(qb_type) %>%
    .calculate_value(value_factor) %>%
    bind_rows(pick_values) %>%
    .label_displaymode(draft_type,league_size) %>%
    arrange(desc(value)) %>%
    rename(Player = player,Age = age,Value = value) %>%
    select(Player,Age,Value)
}

## Value Subfunctions ----
.parse_number <- function(vec){
  as.numeric(gsub(pattern = "([0-9]+).+", replacement = "\\1",x = vec))
}

.calc_currentpicks <- function(picks_raw, rookie_opt, qb_type){

  cp <- picks_raw[
    ,
    c("high_factor","low_factor","high_model","low_model") :=
      list(rookie_opt/100,
           1- (rookie_opt / 100),
           ifelse(qb_type == "1QB", ecr_high_1qb, ecr_high_2qb),
           ifelse(qb_type == "1QB", ecr_low_1qb, ecr_low_2qb)
    )
  ][,ecr:= high_factor * high_model + low_factor*low_model]

  return(cp)
}

.label_currentpicks <- function(cp,leaguesize) {
  l_s <- leaguesize + 0.001

  cp <- cp[
    ,`:=`(season = lubridate::year(lubridate::as_date(scrape_date)),
          rookie_round = (pick %/% l_s) + 1,
          round_pick = round(pick %% l_s,0),
          pick_label = paste0(season," Pick ",as.character(rookie_round),".",str_pad(round_pick,2,'left',0))
          )
  ]
  return(cp)
}

.select_qbtype <- function(df,qb_type){
  df_q <- df[,ecr := ifelse(qb_type == "1QB", ecr_1qb, ecr_2qb)]
  return(df_q)
}

.calculate_value <- function(df,value_factor){
  .value_factor <- value_factor / 10000

  df_v <- df[,value := round(10500 * exp(.value_factor * ecr))]
  return(df_v)
}


.add_futurepicks <- function(pick_values, future_factor, league_size){
  .future_factor <- future_factor / 100
  .league_size <- .parse_number(league_size) + 0.001

  today_month <- lubridate::month(Sys.Date())

  n1_factor <- data.table::fcase(
    today_month < 9, .future_factor,
    today_month <=10 , (1-.future_factor)*0.25 + .future_factor,
    today_month <=11 , (1-.future_factor)*0.50 + .future_factor,
    today_month <=12 , (1-.future_factor)*0.75 + .future_factor)

  n2_factor <- .future_factor

  n1 <- df[
    ,`:=`(
      season = season + 1,
      rookie_round = data.table::fcase(rookie_round == 1, '1st',
                                       rookie_round == 2,  '2nd',
                                       rookie_round == 3,  '3rd',
                                       rookie_round >= 4,  paste0(rookie_round,'th')),
      eml = data.table::fcase(round_pick <= l_s/3, 'Early',
                              round_pick <= l_s*2/3, 'Mid',
                              default =  'Late')
    )
  ]

  n1_eml <- copy(n1)

  n1_eml[
    ,value = mean(value) * n1_factor
  ]

  n1_eml <- n1 %>%
    group_by(season,rookie_round,eml) %>%
    summarise(value = mean(value)*n1_factor) %>%
    ungroup() %>%
    mutate(pick_label = paste(season,eml,rookie_round))

  n1_summary <- n1 %>%
    group_by(season,rookie_round) %>%
    summarise(value = mean(value)*n1_factor) %>%
    ungroup() %>%
    mutate(pick_label = paste(season,rookie_round))

  n2_summary <- n1_summary %>%
    mutate(season = season + 1,
           value = value*n2_factor,
           pick_label = paste(season,rookie_round))

  n3_summary <- n2_summary %>%
    mutate(season = season + 1,
           value = value * n2_factor,
           pick_label = paste(season, rookie_round))

  df %>%
    mutate(rookie_round = as.character(rookie_round)) %>%
    bind_rows(n1_eml,n1_summary,n2_summary, n3_summary) %>%
    mutate(position = "PICK",
           value = round(value)) %>%
    arrange(desc(value))
}

.filter_rookiepicks <- function(df,today_date = Sys.Date()){
  today_month <- month(today_date)

  today_year <- year(today_date) %>% as.character()

  if(today_month > 8){
    df <- df %>%
      filter(str_detect(pick_label,today_year,negate = TRUE))
  }

  return(df)
}

.label_displaymode <- function(df,displaymode,leaguesize){
  l_s <- parse_number(leaguesize) + 0.001

  if(displaymode=='Normal'){return(df)}

  df %>%
    filter(case_when(displaymode == 'Startup (Players Only)'~ pos!='PICK',
                     displaymode == 'Startup (Players & Picks)'~(
                       (pos!='PICK' & draft_year != year(Sys.Date())) |
                         grepl(year(Sys.Date()),player)
                     ))) %>%
    arrange(desc(value)) %>%
    rowid_to_column(var='pick') %>%
    mutate(startup_round = (pick %/% l_s)+1,
           startup_pick = round(pick %% l_s,0),
           player = paste0("Startup Pick ",startup_round,".",str_pad(startup_pick,2,'left',0)),
           age = NA) %>%
    bind_rows(df) %>%
    arrange(desc(value),player)
}
