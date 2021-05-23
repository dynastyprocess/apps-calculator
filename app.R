options(shiny.reactlog = TRUE)
players_raw <- read_parquet('data/player_raw.parquet')
picks_raw <- read_parquet('data/picks_raw.parquet')

ui <- ui_mainpage(
  f7TabLayout(
    use_sever(),
    includeCSS("www/dp.css"),
    addcss_transparentDT(),
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
             br(),
             h1("Main Inputs", style = "text-align:center"),
             uiOutput('team_inputs'),
             f7Card(title = 'Customize Value Settings',
                    f7Row(
                      f7SmartSelect(
                        'qb_type',
                        label = 'QB Type',
                        openIn = "sheet",
                        choices = c('1QB', '2QB/SF')
                      ),
                      f7SmartSelect(
                        'teams',
                        label = 'Teams',
                        openIn = "sheet",
                        choices = glue("{seq(6,24,2)} teams"),
                        selected = "12 teams"
                      ),
                      f7SmartSelect(
                        'draft_type',
                        label = "Display Mode",
                        openIn = "sheet",
                        choices = c('Normal',
                                    'Startup (Players & Picks)',
                                    'Startup (Players Only)')
                      )
                    ),
                    f7Slider(
                      'value_factor',
                      "Valuation Factor",
                      min = 210,
                      max = 260,
                      value = 235,
                      step = 5,
                      labels = tagList(
                        f7Icon("square_stack_3d_up_fill"),
                        f7Icon("star_circle_fill")
                      )
                    ),
                    f7Slider(
                      'rookie_optimism',
                      'Rookie Optimism',
                      min = 0,
                      max = 100,
                      value = 80,
                      step = 5,
                      labels = tagList(
                        f7Icon("bolt_slash_fill"),
                        f7Icon("bolt_fill")
                      )
                    ),
                    f7Slider(
                      'future_factor',
                      'Future Pick Value',
                      min = 65,
                      max = 95,
                      value = 80,
                      step = 5,
                      labels = tagList(
                        f7Icon("play_fill"),
                        f7Icon("forward_fill")
                      )
                    ),
                    br(),
                    f7Button(
                      'toggle_inputhelp',
                      label = "Help",
                      shadow = TRUE,
                      size = 'small',
                      rounded = TRUE
                    )
             ),
             ui_spacer()
      ),
      f7Tab( # analysis tab ----
             tabName = "Analysis",
             icon = f7Icon('graph_circle_fill'),
             h1("Trade Analysis", style = 'text-align:center;'),
             uiOutput('results_tab'),
             ui_spacer()
      ),
      f7Tab(tabName = "Values", # values tab ----
            icon = f7Icon('square_favorites_fill'),
            h1('Values - Quick Reference', style = 'text-align:center;'),
            uiOutput('values'),
            ui_spacer()
      ),
      f7Tab(tabName = "About", # about tab ----
            icon = f7Icon('info_circle_fill'),
            br(),
            div(img(src = 'icons/128x128.png'), style = 'text-align:center;'),
            br(),
            f7Card(title = "About",
                   includeMarkdown('www/about.md')),
            br(),
            f7Card(glue(
              "ECR last updated: {players_raw$scrape_date[[1]]}"
            )),
            br(),
            f7Card(
              title = "More by DynastyProcess:",
              f7List(
                inset = TRUE,
                # f7ListItem(title = "Desktop Version",
                #            url = "https://apps.dynastyprocess.com/calculator",
                #            media = f7Icon('number_circle_fill',old = FALSE)),
                f7ListItem(
                  title = "Crystal Ball",
                  media = f7Icon('moon_circle_fill'),
                  url = "https://apps.dynastyprocess.com/crystalball"
                ),
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
                   value = character()
                   # value = values()$Player[sample(1:32,1)]
    )})

  output$teamBinput <- renderUI({
    f7AutoComplete('players_teamB',
                   label = "Add Players to Team B",
                   multiple = TRUE,
                   expandInput = TRUE,
                   typeahead = FALSE,
                   choices = values()$Player,
                   value = character()
                   # value = values()$Player[sample(1:32,1)]
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

  output$trade_gauge <- renderUI({

    gauge_value <- if(teamA_total() > teamB_total()){50+percent_diff()/2} else {50-percent_diff()/2}

    x <- f7Gauge('score',type = 'semicircle', value = gauge_value,
            borderBgColor = '#1b7837',
            borderColor = '#762a83',
            valueText = paste0(percent_diff(),'%'),
            valueTextColor = case_when(teamA_total() == teamB_total() ~ '#ffffff',
                                       percent_diff() <=5 ~ '#ffffff',
                                       teamA_total() > teamB_total() ~ '#762a83',
                                       teamA_total() < teamB_total() ~ '#1b7837',),
            labelText = case_when(teamA_total() == teamB_total() ~ 'Trade is equal!',
                                  percent_diff() <=5 ~ 'Trade is ~ fair!',
                                  teamA_total() > teamB_total() ~ 'in favour of Team A',
                                  teamA_total() < teamB_total() ~ 'in favour of Team B'),
            labelFontSize = '18'
    )

    return(x)
  })

  output$teamA_total <- renderText({ paste("Team A total:",format(teamA_total(),big.mark = ',')) })
  output$teamB_total <- renderText({ paste("Team B total:",format(teamB_total(),big.mark = ',')) })

  value_container <- withTags(
    table(class = "compact row-border",
          thead(tr(
            th(style="text-align:left;","Player"),
            th(style="text-align:right;padding-right:3px;","Age"),
            th(style='text-align:right;padding-right:3px;',"Value")
          ))
    )
  )

  output$teamA_valuetable <- renderDT({
    teamA_values() %>%
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
  output$teamB_valuetable <- renderDT({
    teamB_values() %>%
      datatable(class = "valuetable compact row-border",
                container = value_container,
                selection = 'none',
                options = list(searching = FALSE,
                               scrollX = TRUE,
                               columnDefs = list(list(className = 'dt-left', targets = 0),
                                                 list(`text-align` = 'right', targets = c(1,2))),
                               ordering = FALSE,
                               paging = FALSE,
                               info = FALSE),
                rownames = FALSE)
  })

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

    req(percent_diff()>5|is.infinite(percent_diff()))

    f7Card(title = "Trade Wizard", inset = TRUE,
           "These players might help balance out the trade!",
           DTOutput('tradewizard_table')
    )
  })

  output$results_tab <- renderUI({

    validate(need(input$calculate,message = "Please press Calculate to load the analysis!"))

    div(
      f7Card(
        div(uiOutput('trade_gauge',height = '250px'),style = 'text-align:center;'),inset= TRUE),

      f7Card(title = textOutput('teamA_total'), inset = TRUE,
             DTOutput('teamA_valuetable')
      ),
      f7Card(title = textOutput('teamB_total'), inset = TRUE,
             DTOutput('teamB_valuetable')
      ),
      f7Card(title = "Trade Plot",inset = TRUE,
             mobileOutput('trade_plot')
      ),
      uiOutput('tradewizard'),
      br(),
      br(),
      br(),
      br(),
      br()
    )
  })

  observeEvent(input$calculate,{
    updateF7Tabs(session, id = 'tabs', selected = 'Analysis')
  })

  # values tab ----
  # output$values_table <- renderDT({
  #   values() %>%
  #     datatable(class = "valuetable compact row-border",
  #               container = value_container,
  #               selection = 'none',
  #               # filter = "top",
  #               options = list(searching = FALSE,
  #                              ordering = FALSE,
  #                              # columnDefs = list()
  #                              paging = FALSE,
  #                              info = FALSE),
  #               rownames = FALSE)
  # })


  output$values <- renderUI({

    # validate(need(input$calculate,message = "Please press Calculate to load the Values table!"))

    div(
      # f7Card(DTOutput('values_table')),
      f7Table(values(),card = TRUE),
      br(),
      br(),
      br(),
      br(),
      br()
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
