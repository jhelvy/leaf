library(surveydown)

# sd_db_config()
db <- sd_db_connect()

server <- function(input, output, session) {

  sd_server(
    db = db,
    use_cookies = TRUE
  )

}

shiny::shinyApp(ui = sd_ui(), server = server)
