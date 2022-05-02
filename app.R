players_raw <- as.data.table(read_parquet('data/player_raw.parquet'))
picks_raw <- as.data.table(read_parquet('data/picks_raw.parquet'))

prefill <- values_generate(players_raw,picks_raw)

ui <- ui_mainpage(
  f7TabLayout(
    useSever(),
    use_glouton(),
    shinyjs::useShinyjs(),
    tags$head(tags$script(src = "dpcalc.js")),
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
    navbar = ui_header(),
    panels = ui_sidebar(),
    appbar = NULL,
    f7Tabs(
      id = 'tabs',
      f7Tab(
        tabName = "Inputs",
        icon = f7Icon('wand_stars'),
        active = TRUE,
        f7SmartSelect(
          inputId = "players_teamA",
          label = "Add Players to Team A",
          multiple = TRUE,
          choices = prefill$Player,
          openIn = "popup",
          searchbar = TRUE,
          virtualList = TRUE
        ),
        uiOutput('teamA_list'),
        f7SmartSelect(
          inputId = "players_teamB",
          label = "Add Players to Team B",
          multiple = TRUE,
          openIn = "popup",
          choices = prefill$Player,
          searchbar = TRUE,
          virtualList = TRUE
        ),
        uiOutput('teamB_list'),
        f7Button('calculate', "Calculate!", shadow = TRUE),
        br(),
        dp_inputs(),
        ui_spacer()
      ),
      f7Tab(
        tabName = "Analysis",
        icon = f7Icon('graph_circle_fill'),
        h1("Trade Analysis", style = 'text-align:center;'),
        div(
          id = "analysis_placeholder",
          f7Card("Press Calculate to load the analysis!")
        ),
        shinyjs::hidden(
          div(
            id = "result_div",
            div(
              style = "text-align:center;",
              f7Card(
                inset = TRUE,
                f7Gauge(id = 'score',
                        type = 'semicircle',
                        value = 50,
                        borderBgColor = '#1b7837',
                        borderColor = '#762a83',
                        labelFontSize = '18')
              )),
            f7Card(div(textOutput('teamA_total'),style = "font-size:larger;font-weight:700;"), inset = TRUE),
            uiOutput('result_table_teamA'),
            f7Card(div(textOutput('teamB_total'),style = "font-size:larger;font-weight:700;"), inset = TRUE),
            uiOutput('result_table_teamB'),
            dp_donations(),
            f7Card(title = "Trade Plot", inset = TRUE, mobileOutput('trade_plot')),
            uiOutput('tradewizard'),
            ui_spacer()
          )
        ),
        uiOutput('results_tab'),
        ui_spacer()
      ),
      f7Tab(
        tabName = "Values",
        icon = f7Icon('square_favorites_fill'),
        h1('Values - Quick Reference', style = 'text-align:center;'),
        f7Card(
          f7Text("value_search",label = NULL,placeholder = "Search:"),
          inset = TRUE),
        uiOutput('values'),
        ui_spacer()
      ),
      f7Tab(
        tabName = "About",
        icon = f7Icon('info_circle_fill'),
        br(),
        div(
          img(src = 'https://github.com/dynastyprocess/graphics/raw/main/.dynastyprocess/dp_hex.svg',
              style = 'max-width: 128px;'),
          style = 'text-align:center;'),
        br(),
        f7Card(
          title = "About",
          includeMarkdown('www/about.md')),
        br(),
        dp_donations(),
        br(),
        f7Card(
          glue("ECR last updated: {players_raw$scrape_date[[1]]}")
        ),
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
  session$allowReconnect(TRUE)

  # Input Updates ----

  observeEvent(input$teams,{
    if(.parse_number(input$teams) >= 20 & input$qb_type != "2QB/SF"){
      updateF7SmartSelect(inputId = "qb_type", selected = "2QB/SF")
      f7Toast(
        text = "Whoa, big league! Switching QB type to SF.",
        position = "bottom",
        closeTimeout = 3000
      )
    }
  })

  # Calculate Actual Values ----

  rv <- reactiveValues()

  rv$values <- prefill

  observeEvent({
    paste(input$qb_type,
          input$teams,
          input$value_factor,
          input$rookie_optimism,
          input$draft_type,
          input$future_factor)
  },{
    shinyMobile::showF7Preloader()
    rv$values <- values_generate(players_raw = players_raw,
                                 picks_raw = picks_raw,
                                 qb_type = input$qb_type,
                                 league_size = input$teams,
                                 value_factor = input$value_factor,
                                 rookie_optimism = input$rookie_optimism,
                                 draft_type = input$draft_type,
                                 future_factor = input$future_factor
    )
    updateF7SmartSelect("players_teamA", choices = rv$values$Player)
    updateF7SmartSelect("players_teamB", choices = rv$values$Player)

    Sys.sleep(1)

    shinyMobile::hideF7Preloader()
  }, ignoreInit = FALSE)

  # Update input fields ----

  output$teamA_list <- renderUI({
    req(input$players_teamA)
    lapply(input$players_teamA, f7ListItem) |> f7List(inset = TRUE)
  })

  output$teamB_list <- renderUI({
    req(input$players_teamB)
    lapply(input$players_teamB, f7ListItem) |> f7List(inset = TRUE)
  })

  observeEvent(input$toggle_inputhelp, {
    updateF7Panel(id = "panel_left", session = session)
  })

  # Results tab ----

  teamA_values <- eventReactive(input$calculate,{
    v <- rv$values
    setDT(v)
    v[Player %in% input$players_teamA][order(-Value)]
  })

  teamB_values <- eventReactive(input$calculate,{
    v <- rv$values
    setDT(v)
    v[Player %in% input$players_teamB][order(-Value)]
  })

  teamA_total <- reactive({
    vA <- teamA_values()
    setDT(vA)
    vA[,.(Total = sum(Value))][["Total"]]
  })

  teamB_total <- reactive({
    vB <- teamB_values()
    setDT(vB)
    vB[,.(Total = sum(Value))][["Total"]]
  })

  percent_diff <- reactive({
    if (teamA_total() > teamB_total()) {round(100*((teamA_total() - teamB_total())/teamB_total()))}
    else if (teamA_total() < teamB_total()) {round(100*((teamB_total() - teamA_total())/teamA_total()))}
    else {0}
  })

  observeEvent(input$calculate,{
    shinyjs::hide(id = "analysis_placeholder")
    shinyjs::show(id = "result_div")

    glouton::add_cookie("dp_qb_type", input$qb_type,options = cookie_options(expire = 1))
    glouton::add_cookie("dp_teams", input$teams,options = cookie_options(expire = 1))
    glouton::add_cookie("dp_value_factor", input$value_factor,options = cookie_options(expire = 1))
    glouton::add_cookie("dp_rookie_optimism", input$rookie_optimism,options = cookie_options(expire = 1))
    glouton::add_cookie("dp_draft_type", input$draft_type,options = cookie_options(expire = 1))
    glouton::add_cookie("dp_future_factor", input$future_factor,options = cookie_options(expire = 1))

    gauge_value <- if(teamA_total() > teamB_total()) 50+percent_diff()/2 else 50-percent_diff()/2

    updateF7Gauge(
      id = "score",
      value = gauge_value,
      valueText = paste0(percent_diff(),'%'),
      valueTextColor = data.table::fcase(teamA_total() == teamB_total(), '#ffffff',
                                         percent_diff() <=5, '#ffffff',
                                         teamA_total() > teamB_total(), '#762a83',
                                         teamA_total() < teamB_total(), '#1b7837'),
      labelText = data.table::fcase(teamA_total() == teamB_total(), 'Trade is equal!',
                                    percent_diff() <=5, 'Trade is ~ fair!',
                                    teamA_total() > teamB_total(), 'in favour of Team A',
                                    teamA_total() < teamB_total(), 'in favour of Team B')
    )

    updateF7Tabs(session, id = 'tabs', selected = 'Analysis')
  })

  output$teamA_total <- renderText( paste("Team A total:",format(teamA_total(),big.mark = ',')) )
  output$teamB_total <- renderText( paste("Team B total:",format(teamB_total(),big.mark = ',')) )

  output$trade_plot <- render_mobile({

    vA <- copy(teamA_values())
    setDT(vA)
    vA[,Team := "Team A"]

    vB <- copy(teamB_values())
    setDT(vB)
    vB[,Team := "Team B"]

    data.table::rbindlist(list(vA,vB)) |>
      mobile(aes(x = Team, y = Value, color = Player, adjust = stack)) |>
      mobile_bar() |>
      mobile_legend(position = 'bottom')

  })

  output$tradewizard <- renderUI({

    req(percent_diff() > 5 | is.infinite(percent_diff()))

    v <- rv$values
    setDT(v)

    trade_diff <- abs(teamA_total() - teamB_total())

    tradebalancer_table <- v[Value<=(trade_diff*1.05),Value>=(trade_diff*0.95),]

    tagList(
      f7Card(title = "Trade Wizard", inset = TRUE,
             "These players might help balance out the trade!"),
      f7Table(tradebalancer_table,card = TRUE)
    )
  })

  output$result_table_teamA <- renderUI({req(input$calculate); f7Table(teamA_values(),card = TRUE)})
  output$result_table_teamB <- renderUI({req(input$calculate); f7Table(teamB_values(),card = TRUE)})

  # values tab ----

  value_display <- reactive({
    if(!isTruthy(input$value_search)) return(rv$values)
    v <- rv$values
    v[str_detect(tolower(Player),fixed(tolower(input$value_search)))]
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
    position = "bottom",
    closeTimeout = 3000)

  # Save data to a sqlite file on server ----

  sessionID <- UUIDgenerate(1,use.time = TRUE)

  observeEvent(input$calculate, {

    req(teamA_values(),teamB_values())

    try({

      tradeID <- UUIDgenerate(1,use.time = TRUE)

      saved_data <- tibble::tibble(
        trade_id = tradeID,
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
        teamB_total = teamB_total(),
        stringsAsFactors = FALSE
      )

      arrow::write_parquet(saved_data,file.path("storage",paste0(tradeID,".parquet")))
    })
  })

  observeEvent(
    eventExpr = TRUE,{

      all_cookies <- fetch_cookies()

      if(!is.null(all_cookies[["qb_type"]])) updateF7SmartSelect("qb_type",selected = all_cookies[["qb_type"]])
      if(!is.null(all_cookies[["teams"]])) updateF7SmartSelect("teams",selected = all_cookies[["teams"]])
      if(!is.null(all_cookies[["draft_type"]])) updateF7SmartSelect("draft_type",selected = all_cookies[["draft_type"]])
      if(!is.null(all_cookies[["value_factor"]])) updateF7Slider("value_factor", value = as.numeric(all_cookies[["value_factor"]]))
      if(!is.null(all_cookies[["rookie_optimism"]])) updateF7Slider("rookie_optimism", value = as.numeric(all_cookies[["rookie_optimism"]]))
      if(!is.null(all_cookies[["future_factor"]])) updateF7Slider("future_factor", value = as.numeric(all_cookies[["future_factor"]]))

    }, ignoreInit = FALSE, ignoreNULL = FALSE, once = TRUE)

} # end of server segment ----

shinyApp(ui, server) # run app ----
