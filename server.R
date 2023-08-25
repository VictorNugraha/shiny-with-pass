server <- function(input, output, session) {
  res_auth <- callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials(credentials)
  )
  
  sever(bg_color = "black")
  
  # Output render image Home---
  output$image <- renderImage({
    list(src = "www/logo_2.png",
         style = "display: block; margin-left: auto; margin-right: auto;",
         height = "75%")
    
  }, deleteFile = F)
  
  # Output action button 1 to go to second tab---
  observeEvent(input$jumpToStats, {
    updateTabsetPanel(session = session,
                      inputId = "inTabset",
                      selected = "stats")
  })
  
  # Output action button 2 to go to second tab---
  observeEvent(input$jumpToVis, {
    updateTabsetPanel(session = session,
                      inputId = "inTabset",
                      selected = "vis")
  })
  
  output$text <- renderUI({
    div(style = "text-align:center",
        HTML(
          paste(
            "<center> <h1> <font color =\"#94368f\">What You'll Find Here</h1> </center></font color =\"#94368f\"><br>
            <h5>Interactive dashboard and visualization that design to perform descriptive information and also
            predictive analytics about World Happiness Report..</h5>
            "
          )
        ))
    
  })
  
  output$text2 <- renderUI({
    div(style = "text-align:center",
        HTML(
          paste(
            "<center> <h1><font color =\"#94368f\">How It Can Help You</h1> </center></font color =\"#94368f\"><br>
            <h5>The features in this dashboard, will help you to understand better about several factors that may affect
              happiness levels in every country. Moreover, you can predict the happiness scores based on the criteria like income,
              social support from other country, human rights, etc.</h5>
            "
          )
        ))
    
  })
  
  output$text3 <- renderUI({
    div(style = "text-align:center",
        HTML(
          paste(
            "<center> <h1><font color =\"#94368f\">Ready To Get Started?</h1></font color =\"#94368f\"> </center><br>
            <h5>This dashboard has two main menus, Statistics and Visualization. Statistics menu will help you to perform predictive
            analytics result about happiness score, whilst Visualization menu will perform details of descriptive information
            about the data.</h5>
            "
          )
        ))
    
  })
  
  output$text4 <- renderUI({
    div(style = "text-align:center",
        HTML(
          paste(
            "<center> <h2> <font color =\"#94368f\">Please Fill Machine Learning Input</h2> </center></font color =\"#94368f\"><br>
            "
          )
        ))
    
  })
  
  output$text5 <- renderUI({
    div(style = "text-align:center",
        HTML(
          paste(
            "<center> <h2> <font color =\"#94368f\">Please Select Desired Visualization</h2> </center></font color =\"#94368f\"><br>
            "
          )
        ))
  })
  
  output$output_ml <- renderUI({
    fluidRow(
      column(
        width = 12,
        box(
          numericInput(
            inputId = "value",
            label = "GDP Per Capita (0~15)",
            min = 0,
            max = 15,
            value = 5,
            step = 1
          ),
          width = 3
        ),
        box(
          numericInput(
            inputId = "value2",
            label = "Social Support (0~1)",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.1
          ),
          width = 3
        ),
        box(
          numericInput(
            inputId = "value3",
            label = "Life Expectancy (0~100)",
            min = 0,
            max = 100,
            value = 50,
            step = 10
          ),
          width = 3
        ),
        box(
          numericInput(
            inputId = "value4",
            label = "Freedom of Choices(0~1)",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.1
          ),
          width = 3
        ),
        box(
          numericInput(
            inputId = "value5",
            label = "Generosity (-1~1)",
            min = -10,
            max = 20,
            value = 0.5,
            step = 0.1
          ),
          width = 3
        ),
        box(
          numericInput(
            inputId = "value6",
            label = "Perception of Corruption (0~1)",
            min = -10,
            max = 20,
            value = 0.5,
            step = 0.1
          ),
          width = 3
        ),
        box(
          numericInput(
            inputId = "value7",
            label = "Positive Affect (0~1)",
            min = -10,
            max = 20,
            value = 0.5,
            ,
            step = 0.1
          ),
          width = 3
        ),
        box(
          numericInput(
            inputId = "value8",
            label = "Negative Affect (0~1)",
            min = -10,
            max = 20,
            value = 0.5,
            step = 0.1
          ),
          width = 3
        )
      ),
      column(5),
      column(
        width = 2,
        actionButton(
          'submitButton',
          label = HTML("<span style='font-size:1.25em;'>Submit</span>"),
          style = "color: white;
                                 background-color: #c192bf;
                                 border-color: #c192bf;
                                 width: 250px;"
        )
      ),
      column(5)
    )
  })
  
  df <- reactive({
    data_input <- data.frame(
      Log.GDP.Per.Capita = input$value,
      Social.Support = input$value2,
      Healthy.Life.Expectancy.At.Birth = input$value3,
      Freedom.To.Make.Life.Choices = input$value4,
      Generosity = input$value5,
      Perceptions.Of.Corruption = input$value6,
      Positive.Affect = input$value7,
      Negative.Affect = input$value8
    )
    
    data_predict <-
      data_input %>% mutate_at(colnames(data_input), as.numeric)
    
  })
  
  prediction <- reactive({
    pred <- predict(object = rm_model, newdata = df())
  })
  
  observeEvent(input$submitButton, {
    output$Pred <- renderText({
      prediction()
    })
    
    
    output$gauge <- renderPlotly({
      fig <- plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = prediction(),
        type = "indicator",
        mode = "gauge+number",
        gauge = list(
          axis = list(
            range = list(NULL, 10),
            tickwidth = 1,
            tickcolor = "black"
          ),
          bar = list(color = "white"),
          bgcolor = "white",
          borderwidth = 2,
          bordercolor = "gray",
          steps = list(list(
            range = c(0, 10), color = "#c192bf"
          ))
        )
      )
      
      fig <- fig %>%
        layout(
          margin = list(l = 0, r = 0),
          font = list(color = "#94368f", family = "Arial"),
          title = list(
            text = "Happiness Prediction Result",
            x = 0.49,
            y = 0.4,
            size = 15
          )
        )
      
      fig
    })
    
  })
  
  output$plot_int <- renderPlotly({
    happiness_2022 <- happiness %>%
      filter(Year == input$year,
             Regional.Indicator %in% input$region)
    
    happiness_SS_2022 <- happiness_2022 %>%
      group_by(Regional.Indicator) %>%
      summarise(Social_support = mean(Social.Support)) %>%
      arrange(desc(Social_support))
    
    plot_statis <- ggplot(data = happiness_SS_2022,
                          mapping = aes(
                            x = Social_support,
                            y = reorder(Regional.Indicator, Social_support),
                            text = glue("{Regional.Indicator}
                                                      Score: {Social_support}")
                          )) + # Parameter text dan fungsi glue ditambahkan pada bagian ini
      geom_col(mapping = aes(fill = Social_support)) +
      scale_fill_gradient(low = "#c192bf", high = "#94368f") +
      labs(title = "Social Support Ranking by Region",
           x = "Social Support Score",
           y = "Regional Indicator") +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(
          family = '',
          face = 'bold',
          colour = '#94368f',
          hjust = 0.5
        )
      )
    
    ggplotly(plot_statis, tooltip = "text")
    
  })
  
  observeEvent(
    input$interactive,
    {
      output$output_viz <- renderUI({
        fluidRow(
          box(
            width = 6,
            tags$head(tags$script(picker_in)),
            pickerInput(
              inputId = "region",
              label = HTML(paste("<b>Region</b>")),
              choices = c(
                "South Asia ",
                "Central and Eastern Europe",
                "Middle East and North Africa",
                "Latin America and Caribbean",
                "Commonwealth of Independent States",
                "North America and ANZ",
                "Western Europe",
                "Sub-Saharan Africa",
                "Southeast Asia",
                "East Asia"
              ),
              selected = unique(happiness$Regional.Indicator),
              multiple = TRUE,
              options = list(
                "actions-box" = TRUE,
                "selected-text-format" = "count"
              ),
              inline = TRUE,
              width = "540px"
            )
          ),
          
          box(
            width = 6,
            pickerInput(
              inputId = "year",
              label = HTML(paste("<b>Year</b>")),
              choices = c(2018, 2019, 2020, 2021, 2022),
              selected = 2018,
              multiple = FALSE,
              options = list(
                "actions-box" = TRUE,
                "selected-text-format" = "count"
              ),
              inline = TRUE,
              width = "540px"
            ),
          ),
          
          fluidRow(column(width = 12, plotlyOutput("plot_int")))
        )
      })
      
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE,
    once = F
  )
  
  observeEvent(input$animation, {
    output$output_viz <- renderUI({
      fluidRow(column(width = 6, img(src = "ts.gif", align = "center")),
               column(width = 6, img(src = "bar.gif", align = "center")))
    })
    
  })
  
  
  output$app <- renderUI({
    fluidPage(
      # Add menu to the right
      list(tags$head(
        HTML('<link rel="icon", href="MyIcon.png", type="image/png"/>')
      )),
      div(
        style = "padding: 1px 0px; width: '100%'",
        titlePanel(title = "", windowTitle = "World Hapiness Dashboard")
      ),
      
      introjsUI(),
      useShinyjs(),
      
      tags$head(tags$style(
        HTML("
          .navbar .navbar-nav {float: right}
        ")
      )),
      
      # Navbar page title
      navbarPage(
        #Title---
        title = div(
          img(
            src = "title_logo2.png",
            height = '17px',
            width = '17px'
          ),
          "World Happiness Report",
          style = "color: white"
        ),
        #Theme bootswatch---
        theme = bs_theme(
          bootswatch = "litera",
          bg = "white",
          fg = "black",
          base_font = font_google("Prompt"),
          code_font = font_google("JetBrains Mono")
          
        ),
        
        tags$head(tags$style(
          HTML(
            '.navbar-static-top {background-color: #94368f;}',
            '.navbar-default .navbar-nav>.active>a {background-color: #94368f;}'
          )
        )),
        
        #ID NavBar---
        id = "inTabset",
        
        header = tagList(useShinydashboard()),
        
        # Navbar menu
        tabPanel(
          title = div("Home", style = "color: white"),
          imageOutput("image"),
          
          # WHAT
          fluidRow(column(3),
                   column(6, htmlOutput("text")),
                   column(3)),
          
          # PAGE BREAK
          fluidRow(column(2), column(
            8,
            tags$hr(style = "border-top: 1px solid #000000; border-color: #c192bf;")
          ), column(2)),
          
          # HOW
          fluidRow(column(3),
                   column(6, htmlOutput("text2")),
                   column(3)),
          
          # PAGE BREAK
          
          fluidRow(column(2), column(
            8,
            tags$hr(style = "border-top: 1px solid #000000;  border-color: #c192bf;")
          ), column(2)),
          
          # SOLUTION
          fluidRow(
            align = "center",
            column(3),
            column(
              6,
              htmlOutput("text3"),
              
              splitLayout(
                cellWidths = c("40%", "40%"),
                actionButton(
                  inputId = 'jumpToStats',
                  icon = icon("book", style = "font-size: 17.5px"),
                  label = HTML("<span style='font-size:1.75em;'>Statistics</span>"),
                  style = "color: white;
                           background-color: #c192bf;
                           border-color: #c192bf;
                           width: 200px;"
                ),
                actionButton(
                  inputId = 'jumpToVis',
                  icon("chart-line", style = "font-size: 17.5px"),
                  label = HTML("<span style='font-size:1.75em'>Visualization</span>"),
                  style = "color: white;
                           background-color: #c192bf;
                           border-color: #c192bf;
                           width: 200px;"
                )
              )
            ),
            column(3)
          )
        ),
        
        tabPanel(
          title = div("Machine Learning", style = "color: white"),
          value = "stats",
          
          fluidPage(fluidRow(
            column(width = 12, htmlOutput("text4")),
            column(width = 12,
                   uiOutput("output_ml"))
          ),
          
          fluidRow(
            column(width = 1),
            column(width = 11, plotlyOutput('gauge'))
          ))
          
        ),
        
        tabPanel(
          title = div("Report Visualization", style = "color: white"),
          value = "vis",
          
          fluidRow(
            column(
              width = 12,
              
              br(),
              
              htmlOutput("text5"),
              
              actionButton(
                inputId = "interactive",
                label = strong("Interactive Visualization"),
                style = "color: white;
                                                  background-color: #c192bf;
                                                  border-color: #c192bf;
                                                  width: 200px;"
              ),
              
              actionButton(
                inputId = "animation",
                label = strong("Animation Visualization"),
                style = "color: white;
                                                  background-color: #c192bf;
                                                  border-color: #c192bf;
                                                  width: 200px;"
              ),
              
              align = "center",
              style = "margin-bottom: 10px;",
              style = "margin-top: -10px;",
              br(),
              br()
              
            )
          ),
          
          fluidPage(fluidRow(column(
            width = 12,
            uiOutput("output_viz")
          )))
        ),
        
        #FOOTER---
        div(class = "footer",
            includeHTML("html/footer2.Rhtml"))
      )
    )
    
  })
  
}
