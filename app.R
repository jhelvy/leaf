# Install required packages:
# install.packages("pak")
# pak::pak('surveydown-dev/surveydown') # Development version from github
# pak::pak('tidyverse')
# pak::pak('arrow')

# Load packages
library(surveydown)
library(tidyverse)

# Set up car make-model-trim data frame

cars <- arrow::open_dataset('data.parquet') %>%
  filter(powertrain == 'bev') %>%
  select(make, model, trim) %>%
  collect() %>%
  distinct(make, model, trim)

makes <- unique(cars$make)
names(makes) <- makes

# Set up database configuration

# sd_db_config()
db <- sd_db_connect()

# Server setup
server <- function(input, output, session) {

  # Create reactive values to store filtered data frames
  filtered_data <- reactiveValues(
    make_selected_df = NULL,
    model_selected_df = NULL
  )

  sd_question(
    type   = "select",
    id     = "make",
    label  = "Make:",
    option = makes
  )

  observe({
    # Store the filtered data frame in the reactiveValues
    filtered_data$make_selected_df <- cars[which(input$make == cars$make),]

    models <- filtered_data$make_selected_df$model
    names(models) <- models

    sd_question(
      type   = "select",
      id     = "model",
      label  = "Model:",
      option = models
    )
  })

  observe({

    # Filter based on both make and model
    filtered_data$model_selected_df <- filtered_data$make_selected_df[
      which(input$model == filtered_data$make_selected_df$model),]

    trims <- filtered_data$model_selected_df$trim
    names(trims) <- trims

    sd_question(
      type   = "select",
      id     = "trim",
      label  = "Trim:",
      option = trims
    )
  })

  # Database designation and other settings
  sd_server(
    db = db,
    all_questions_required = FALSE
  )
}

# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
