library(shiny)
library(shinymanager)

# UI
# ui <- secure_app(
#   # Choose a new theme
#   theme = shinythemes::shinytheme("flatly"),
#   
#   ### EDIT: Add an image ### 
#   tag_img = tags$img(
#     src = "https://www.r-project.org/logo/Rlogo.png", width = 100
#   ),
#   
#   # Classic UI
#   fluidPage(
#     tags$h1("My app")
#   )
# )


# SERVER
server <- function(input, output, session) {
  
  result_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
}

shinyApp(ui = ui, server = server)