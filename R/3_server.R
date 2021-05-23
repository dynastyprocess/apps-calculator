# Calculator server (sub)functions
#
# Abstracting pieces of the calculator so that they can be used in mobile and desktop versions

library(dplyr)
library(stringr)
library(sever)
library(shiny)
library(joker)

# Generate Calculator Values Table ----
## Main Function (... is passed to the final select function)----

gen_df_values <- function(players_raw, picks_raw,
                          qb_type, league_size, value_factor, rookie_optimism,
                          draft_type, future_factor, ...){

  pick_values <- picks_raw %>%
    .calc_currentpicks(rookie_optimism,qb_type) %>%
    .label_currentpicks(parse_number(league_size)) %>%
    .calculate_value(value_factor) %>%
    .add_futurepicks(future_factor,parse_number(league_size)) %>%
    .filter_rookiepicks() %>%
    select(player = pick_label,value)

  players_raw %>%
    .select_qbtype(qb_type) %>%
    .calculate_value(value_factor) %>%
    bind_rows(pick_values) %>%
    .label_displaymode(draft_type,league_size) %>%
    arrange(desc(value)) %>%
    rename(Player = player,Age = age,Value = value) %>%
    select(...)

}

## Value Subfunctions ----
.select_qbtype <- function(df,qb_type){
  df %>%
    mutate(ecr = case_when(qb_type == '1QB' ~ecr_1qb,
                           TRUE ~ ecr_2qb))
}

.calculate_value <- function(df,value_factor){
  v_f <- value_factor/10000

  df %>%
    mutate(
      value = 10500 * exp(-v_f * ecr),
      value = round(value))
}

.label_currentpicks <- function(df,leaguesize) {
  l_s <- leaguesize + 0.001

  df %>%
    mutate(
      season = year(as_date(scrape_date)),
      rookie_round = (pick %/% l_s)+1,
      round_pick = round(pick %% l_s,0),
      pick_label = paste0(season," Pick ",as.character(rookie_round),".",str_pad(round_pick,2,'left',0))
    )
}

.calc_currentpicks <- function(df,rookie_opt,qb_type){
  # r_o <- rookie_opt/100
  df %>%
    mutate(
      high_model = case_when(qb_type == '1QB' ~ ecr_high_1qb,
                             TRUE ~ ecr_high_2qb),
      low_model = case_when(qb_type == '1QB' ~ ecr_low_1qb,
                            TRUE ~ ecr_low_2qb),
      high_factor = rookie_opt/100,
      low_factor = 1-high_factor,
      ecr = high_factor*high_model + low_factor*low_model)
}

.add_futurepicks <- function(df,futurerookie_factor,leaguesize){
  fr_f <- futurerookie_factor/100

  today_month <- month(Sys.Date())

  n1_factor <- case_when(today_month < 9 ~ fr_f,
                         today_month <=10 ~ (1-fr_f)*0.25 + fr_f,
                         today_month <=11 ~ (1-fr_f)*0.50 + fr_f,
                         today_month <=12 ~ (1-fr_f)*0.75 + fr_f)

  n2_factor <- fr_f

  l_s <- leaguesize + 0.001

  n1 <- df %>%
    mutate(season = season + 1,
           rookie_round = case_when(rookie_round == 1 ~ '1st',
                                    rookie_round == 2 ~ '2nd',
                                    rookie_round == 3 ~ '3rd',
                                    rookie_round >= 4 ~ paste0(rookie_round,'th')),
           eml = case_when(round_pick <= l_s/3 ~ 'Early',
                           round_pick <= l_s*2/3 ~ 'Mid',
                           TRUE ~ 'Late'))

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
                     displaymode == 'Startup (Players & Picks)'~ (pos!='PICK'|grepl(year(Sys.Date()),player)))) %>%
    arrange(desc(value)) %>%
    rowid_to_column(var='pick') %>%
    mutate(startup_round = (pick %/% l_s)+1,
           startup_pick = round(pick %% l_s,0),
           player = paste0("Startup Pick ",startup_round,".",str_pad(startup_pick,2,'left',0))) %>%
    bind_rows(df) %>%
    arrange(desc(value),player)
}

sever_joke <- function(){

  sever::sever(
    shiny::tagList(
      shiny::h1("Disconnected"),
      shiny::p(shiny::em(try(joker::randomjoke()))),
      shiny::tags$button(
        "Reload",
        style = "color:#000;background-color:#fff;",
        class = "button button-raised",
        onClick = "location.reload();"
      )
    ),
    bg_color = "#000"
  )
}
