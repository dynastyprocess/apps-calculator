
addcss_transparentDT <- function(){
  tags$head(tags$style(
    HTML(
      "
      table td, table tr, table th{background: transparent !important;}
      .valuetable {color: #ffffff}
      "
    )
  ))
}

ui_mainpage <- function(...){
  f7Page( # f7Page setup and Init Options ----
          title = "DynastyProcess Trade Calculator",
          manifest = "manifest.json",
          favicon = "favicon.ico",
          icon = '128x128.png',
          allowPWA = TRUE,
          options = list(
            dark = TRUE,
            theme = 'md',
            color = '#d31027',
            tapHold = FALSE,
            navbar = list()
          ),
          ...
  )
}

ui_header <- function(){
  f7Navbar(# Navbar ----
           title = div(img(src = 'icons/header_small.png',style = "max-width:90%;max-height:50px;"),
                       style = "text-align:center;"),
           # subtitle = "Trade Calculator",
           transparent = TRUE,
           hairline = TRUE)
}

ui_sidebar <- function(){
  tagList(f7Panel( # Sidebar panels (currently a help panel) ----
                   # inputId = "panel_left",
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
