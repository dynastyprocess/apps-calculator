#' @inheritParams f7Page
ui_mainpage <- function(...){
  f7Page(
    title = "DynastyProcess Trade Calculator",
    # manifest = "manifest.json",
    favicon = "favicon.ico",
    icon = '128x128.png',
    allowPWA = TRUE,
    # preloader = TRUE,
    # loading_duration = 3,
    options = list(
      dark = TRUE,
      theme = 'md',
      color = '#d7283c',#ed122b
      tapHold = FALSE,
      navbar = list()
    ),
    ...
  )
}

ui_header <- function(){
  f7Navbar(# Navbar ----
           title = div(img(src = 'icons/header_small.png',style = "max-width:90%;max-height:50px;display:flex;align-items:center;"),
                       style = "text-align:center;"),
           # subtitle = "Trade Calculator",
           transparent = TRUE,
           hairline = TRUE)
}

ui_sidebar <- function(){
  tagList(f7Panel( # Sidebar panels (currently a help panel) ----
                   id = "panel_left",
                   title = "Trade Calculator Help",
                   side = "left",
                   theme = "dark",
                   effect = "cover",
                   h3("Inputs",style = "text-align:center;"),
                   .ui_help(),
                   h3("Hints", style ="text-align:center;"),
                   .ui_hints()
  ))
}

.ui_help <- function() {
  f7Accordion(
    f7AccordionItem(
      title = "QB Type",
      f7Block(
        inset = TRUE,
        "Toggles between the base FantasyPros 1QB values and our algorithm-generated 2QB values.")),
    f7AccordionItem(
      title = "League Size",
      f7Block("Renames rookie and startup selections. Does not adjust values, you'll need to tweak the Valuation Factor for that."),
      inset = TRUE),
    f7AccordionItem(
      title = "Display Mode",
      f7Block(
        inset = TRUE,
        "Adds labeling so that you can assess the value of startup pick trades, and can also include placeholder picks for a separate rookie draft.")),
    f7AccordionItem(
      title = "Valuation Factor",
      f7Block(
        inset = TRUE,
        "Tunes how star players are valued relative to bench players, so that you can tweak values for league size/scoring, market preferences, and personal strategy.")),
    f7AccordionItem(
      title = "Rookie Pick Optimism",
      f7Block(
        inset = TRUE,
        "Adjusts between our Perfect Knowledge and Hit Rate algorithms for valuing rookie picks."
      )
    ),
    f7AccordionItem(
      title = "Future Pick Factor",
      f7Block(inset = TRUE, "Adjusts value of future rookie picks.")
    )
  )
}

.ui_hints <- function() {
  f7Accordion(f7AccordionItem(
    title = "Faster searching",
    f7Block(
      inset = TRUE,
      "You can pull up a list of all players by entering a ' , ' and all picks by entering a ' . ' - this is helpful for quickly scrolling through the list! \n"
    )
  ))
}

ui_spacer <- function(){
  div(br(),br(),br(),br(),br())
}

dp_inputs <- function(){
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
             choices = glue("{seq(6,32,2)} teams"),
             selected = "12 teams"
           ),
           f7SmartSelect(
             'draft_type',
             label = "Startup Mode",
             openIn = "sheet",
             choices = c('Normal',
                         'Startup (Players & Picks)',
                         'Startup (Players Only)')
           )
         ),
         f7Slider(
           'value_factor',
           "Valuation Factor",
           min = 185,
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
           value = 60,
           step = 5,
           labels = tagList(
             f7Icon("bolt_slash_fill"),
             f7Icon("bolt_fill")
           )
         ),
         f7Slider(
           'future_factor',
           'Future Pick Value',
           min = 50,
           max = 100,
           value = 90,
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
  )
}

dp_donations <- function(){
  div(style = "text-align:center;",f7Link(label = img(src="https://cdn.ko-fi.com/cdn/kofi4.png?v=3",style = "border:0px;height:50px;"), href = "https://ko-fi.com/M4M83XE0K"))
}
