# Calculator server (sub)functions
#
# Abstracting pieces of the calculator so that they can be used in mobile and desktop versions

# Generate Calculator Values Table ----
## Main Function (... is passed to the final select function)----

sever_joke <- function(){

  sever::sever(
    shiny::div(
      id = "sever_screen",
      shiny::h1("Disconnected"),
      shiny::p(shiny::em(try(joker::randomjoke()))),
      shiny::tags$button(
        "Reload",
        style = "color:#000;background-color:#fff;",
        class = "button button-raised",
        onClick = "Shiny.shinyapp.reconnect();" # for future allowReconnect ish things?
        # onClick = "location.reload()"
      )
    ),
    bg_color = "#000"
  )
}
