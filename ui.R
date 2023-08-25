fluidPage(
  
  auth_ui(
    id = "auth",
    tags_bottom = tags$div(
      tags$p("Username: happy - Password: happy123", style ="text-align:center")),
    lan = use_language("en"),
    status = "danger"
  ), 
  
  useSever(),
  
  uiOutput("app")
)
