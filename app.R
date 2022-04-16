options(shiny.reactlog = TRUE)
players_raw <- read_parquet('data/player_raw.parquet')
picks_raw <- read_parquet('data/picks_raw.parquet')

ui <- ui_mainpage(
  f7TabLayout(
    use_sever(),
    shinyjs::useShinyjs(),
    meta() %>%
      meta_social(
        title = "Trade Calculator - DynastyProcess.com",
        description = "A dynasty trade calculator that you can customize for your strategy and league!",
        url = "https://apps.dynastyprocess.com/calculator",
        image = "header_small.png",
        image_alt = "DynastyProcess Calculator logo",
        twitter_creator = "@_TanHo",
        twitter_card_type = "summary",
        twitter_site = "@DynastyProcess"
      ),
    includeCSS("www/dp.css"),
    # addcss_transparentDT(),
    navbar = ui_header(),
    panels = ui_sidebar(),
    appbar = NULL,
    f7Tabs(
      # Main tabs ----
      id = 'tabs',
      f7Tab( # input tab ----
             tabName = "Inputs",
             icon = f7Icon('wand_stars'),
             active = TRUE,
             # h1("Main Inputs", style = "text-align:center"),
             uiOutput('team_inputs'),
             br(),
             dpcalc_inputs(),
             ui_spacer()
      ),
      f7Tab( # analysis tab ----
             tabName = "Analysis",
             icon = f7Icon('graph_circle_fill'),
             h1("Trade Analysis", style = 'text-align:center;'),
             div(
               id = "analysis_placeholder",
               f7Card("Press Calculate to load the analysis!")
             ),
             shinyjs::hidden(
               div(
                 style = "text-align:center;",
                 id = "gauge_div",
                 f7Card(
                   inset = TRUE,
                   f7Gauge(id = 'score',
                           type = 'semicircle',
                           value = 50,
                           borderBgColor = '#1b7837',
                           borderColor = '#762a83',
                           labelFontSize = '18'
                   )
                 )
               )
             ),
             uiOutput('results_tab'),
             ui_spacer()
      ),
      f7Tab(tabName = "Values", # values tab ----
            icon = f7Icon('square_favorites_fill'),
            h1('Values - Quick Reference', style = 'text-align:center;'),
            f7Card(
              f7Text("value_search",label = NULL,placeholder = "Search:"),
              inset = TRUE),
            uiOutput('values'),
            ui_spacer()
      ),
      f7Tab(tabName = "About", # about tab ----
            icon = f7Icon('info_circle_fill'),
            br(),
            div(img(src = 'https://github.com/dynastyprocess/graphics/raw/main/.dynastyprocess/dp_hex.svg',style = 'max-width: 128px'), style = 'text-align:center;'),
            br(),
            f7Card(title = "About",
                   includeMarkdown('www/about.md')),
            br(),
            dp_donations(),
            br(),
            f7Card(glue(
              "ECR last updated: {players_raw$scrape_date[[1]]}"
            )),
            br(),
            f7Card(
              title = "More by DynastyProcess:",
              f7List(
                inset = TRUE,
                f7ListItem(
                  title = "Twitter",
                  media = f7Icon('logo_twitter'),
                  url = "https://www.twitter.com/dynastyprocess"
                ),
                f7ListItem(
                  title = "Data Repository",
                  media = f7Icon('archivebox_fill'),
                  url = "https://www.github.com/dynastyprocess/data"
                ),
                f7ListItem(
                  title = "Main Site",
                  media = f7Icon('waveform_circle_fill'),
                  url =
                    "https://dynastyprocess.com"
                )
              )
            ),
            ui_spacer()
            # f7Card(title = "Popular Players")
      )
    )
  )
)
server <- function(input, output, session) {

  sever_joke() # cleans up disconnect screen

  # Input Updates ----

  observeEvent(input$teams,{
    if(parse_number(input$teams) >= 20 & input$qb_type != "2QB/SF"){
      updateF7SmartSelect(inputId = "qb_type", selected = "2QB/SF")

      f7Toast(
        text = "Whoa, big league! Switching QB type to SF.",
        position = "bottom",
        closeTimeout = 3000
      )
    }
  })

  # Calculate Actual Values ----

  values <- reactive({
    gen_df_values(players_raw,picks_raw,
                  input$qb_type,input$teams,input$value_factor,
                  input$rookie_optimism,input$draft_type,input$future_factor,
                  c('Player','Age','Value'))
  })

  # Render input fields ----

  output$teamAinput <- renderUI({
    f7AutoComplete('players_teamA',
                   label = "Add Players to Team A",
                   multiple = TRUE,
                   expandInput = TRUE,
                   typeahead = FALSE,
                   choices = values()$Player,
                   # value = character()
                   value = values()$Player[sample(1:32,1)]
    )})

  output$teamBinput <- renderUI({
    f7AutoComplete('players_teamB',
                   label = "Add Players to Team B",
                   multiple = TRUE,
                   expandInput = TRUE,
                   typeahead = FALSE,
                   choices = values()$Player,
                   # value = character()
                   value = values()$Player[sample(1:32,1)]
    )})

  output$teamA_list <- renderUI({

    req(input$players_teamA)

    map(input$players_teamA, f7ListItem) %>%
      f7List(inset = TRUE)
  })

  output$teamB_list <- renderUI({

    req(input$players_teamB)

    map(input$players_teamB, f7ListItem) %>%
      f7List(inset = TRUE)
  })

  output$team_inputs <- renderUI({

    div(
      uiOutput('teamAinput'),
      uiOutput('teamA_list'),
      uiOutput('teamBinput'),
      uiOutput('teamB_list'),
      f7Button('calculate',"Calculate!",
               shadow = TRUE)
    )
  })

  observeEvent(input$toggle_inputhelp, {
    updateF7Panel(inputId = "panel_left", session = session)
  })

  observeEvent(values(),{

    holdA <- input$players_teamA
    holdB <- input$players_teamB

    updateF7AutoComplete('players_teamA', value = holdA)
    updateF7AutoComplete('players_teamB', value = holdB)
  },
  ignoreInit = TRUE)

  # Results tab ----

  teamA_values <- eventReactive(input$calculate,{
    values() %>%
      filter(Player %in% input$players_teamA) %>%
      arrange(desc(Value))
  })

  teamB_values <- eventReactive(input$calculate,{
    values() %>%
      filter(Player %in% input$players_teamB) %>%
      arrange(desc(Value))
  })

  teamA_total <- reactive({
    teamA_values() %>%
      summarise(Total = sum(Value)) %>%
      pull(Total)
  })
  teamB_total <- reactive({
    teamB_values() %>%
      summarise(Total = sum(Value)) %>%
      pull(Total)
  })

  percent_diff <- reactive({if (teamA_total() > teamB_total())
  {round(100*((teamA_total() - teamB_total())/teamB_total()))}
    else if (teamA_total() < teamB_total())
    {round(100*((teamB_total() - teamA_total())/teamA_total()))}
    else
    {0}
  })

  observeEvent(input$calculate,{
    shinyjs::hide(id = "analysis_placeholder")
    shinyjs::show(id = "gauge_div")

    gauge_value <- if(teamA_total() > teamB_total()){50+percent_diff()/2} else {50-percent_diff()/2}

    updateF7Gauge(
      id = "score",
      value = gauge_value,
      valueText = paste0(percent_diff(),'%'),
      valueTextColor = case_when(teamA_total() == teamB_total() ~ '#ffffff',
                                 percent_diff() <=5 ~ '#ffffff',
                                 teamA_total() > teamB_total() ~ '#762a83',
                                 teamA_total() < teamB_total() ~ '#1b7837',),
      labelText = case_when(teamA_total() == teamB_total() ~ 'Trade is equal!',
                            percent_diff() <=5 ~ 'Trade is ~ fair!',
                            teamA_total() > teamB_total() ~ 'in favour of Team A',
                            teamA_total() < teamB_total() ~ 'in favour of Team B'),
    )

  })

  output$teamA_total <- renderText({ paste("Team A total:",format(teamA_total(),big.mark = ',')) })
  output$teamB_total <- renderText({ paste("Team B total:",format(teamB_total(),big.mark = ',')) })

  output$trade_plot <- render_mobile({

    tibble(Team = c('Team A','Team B'),
           Players = list(teamA_values(),teamB_values())) %>%
      unnest(Players) %>%
      mobile(aes(x = Team, y = Value, color = Player, adjust = stack)) %>%
      mobile_bar() %>%
      mobile_legend(position = 'bottom')

  })

  output$tradewizard_table <- renderDT({

    trade_diff <- abs(teamA_total()-teamB_total())

    tradebalancer_table <- values() %>%
      filter(Value<=(trade_diff*1.05),Value>=(trade_diff*0.95)) %>%
      datatable(class = "valuetable compact row-border",
                container = value_container,
                selection = 'none',
                options = list(searching = FALSE,
                               scrollX = TRUE,
                               columnDefs = list(list(className = 'dt-left', targets = 0),
                                                 list(className = 'dt-right', targets = -1)),
                               ordering = FALSE,
                               paging = FALSE,
                               info = FALSE),
                rownames = FALSE)
  })

  output$tradewizard <- renderUI({

    req(percent_diff()>5 | is.infinite(percent_diff()))

    trade_diff <- abs(teamA_total()-teamB_total())

    tradebalancer_table <- values() %>%
      filter(Value<=(trade_diff*1.05),Value>=(trade_diff*0.95))

    tagList(
      f7Card(title = "Trade Wizard", inset = TRUE,
             "These players might help balance out the trade!"),
      f7Table(tradebalancer_table,card = TRUE)
    )
  })

output$results_tab <- renderUI({

  req(input$calculate)

  div(
    f7Card(div(textOutput('teamA_total'),style = "font-size:larger;font-weight:700;"), inset = TRUE),
    f7Table(teamA_values(),card = TRUE),
    f7Card(div(textOutput('teamB_total'),style = "font-size:larger;font-weight:700;"), inset = TRUE),
    f7Table(teamB_values(),card = TRUE),
    dp_donations(),
    f7Card(title = "Trade Plot", inset = TRUE, mobileOutput('trade_plot')),
    uiOutput('tradewizard'),
    ui_spacer()
  )
})

observeEvent(input$calculate,{
  updateF7Tabs(session, id = 'tabs', selected = 'Analysis')
})

# values tab ----

value_display <- reactive({
  if(is.null(input$value_search)) return(values())
  values() %>%
    dplyr::filter(str_detect(tolower(Player),tolower(input$value_search)))
})

output$values <- renderUI({
  div(
    f7Table(value_display(),card = TRUE),
    ui_spacer()
  )
})

# show last updated ----

f7Toast(
  session = session,
  text = glue("ECR last updated {players_raw$scrape_date[[1]]}"),
  position = "center",
  closeTimeout = 1000)

# Save data to a sqlite file on server ----

  sessionID <- UUIDgenerate(1)

  observeEvent(input$calculate, {

    req(teamA_values(),teamB_values())

    saved_data <- tibble(
      trade_id = UUIDgenerate(1),
      session_id = sessionID,
      timestamp = Sys.time(),
      input_calctype = input$calc_type,
      input_drafttype = input$draft_type,
      input_qb = input$qb_type,
      input_teams = input$teams,
      input_valuefactor = input$value_factor,
      input_rookieoptimism = input$rookie_optimism,
      input_futurefactor = input$future_factor,
      teamA_players = paste(input$players_teamA, sep = "", collapse = " | "),
      teamA_values = paste0(teamA_values()$Value, sep = "", collapse = " | "),
      teamA_total = teamA_total(),
      teamB_players = paste(input$players_teamB, sep = "", collapse = " | "),
      teamB_values = paste0(teamB_values()$Value, sep = "", collapse = " | "),
      teamB_total = teamB_total()
    ) %>%
      mutate(
        year_month = format(timestamp,format = "%Y_%m")
      )

    try({
      arrow::write_dataset(
        dataset = saved_data,
        path = "storage",
        format = "parquet",
        partitioning = c("year_month","session_id")
      )
    })
  })

} # end of server segment ----

shinyApp(ui, server) # run app ----
