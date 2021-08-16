#
# Contact center estimation wait time calculation model
#
# (C) 2017 Bright Pattern
#
# Author: Sergey Yanenko
#
library(shiny)
library("highcharter")

#
# Constants
#

min_L  = 0
max_L  = 100
step_L = 1

min_m  = 0
max_m  = 100
step_m = 1

min_Taht  = 0
max_Taht  = 100
step_Taht = 1

min_Taat  = 0
max_Taat  = 100
step_Taat = 1

min_Lambda  = 0
max_Lambda  = 100
step_Lambda = 1

min_K     = 0
max_K     = 1
step_K    = 0.01
scale_x_K = 100

#
# UI
#
ui <- fluidPage(

  #
  # Styles 
  #

  # Title
    tags$head(
    tags$style(HTML("
      h2 {
        font-size: 22px;
        font-style: italic;
        font-weight: 500;
        margin-top: 10px;
        margin-bottom: 0px;
      }
    "))
  ),

  # Sliders panel
  tags$style(HTML(".well {margin-top: 10px; margin-bottom: 30px}}")),
  
  # Sliders
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: purple}")),
  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: DarkOrange}")),
  tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: green}")),
  tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: darkblue}}")),
  tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: DarkTurquoise}}")),
  tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: black}}")),

  title = "EWT",

  # Input
  sidebarLayout(
    position = c("right"),

    sidebarPanel(
    
      sliderInput("L",
                  "L (queue length)",
                  min = min_L,
                  max = max_L,
                  step = step_L,
                  animate = T,
                  value = step_L),
      
      sliderInput("m",
                  "m (total number of agents)",
                  min = min_m,
                  max = max_m,
                  step = step_m,
                  animate = T,
                  value = step_m),
      
      sliderInput("Taht",
                  "Taht (average handling time)",
                  min = min_Taht,
                  max = max_Taht,
                  step = step_Taht,
                  animate = T,
                  value = step_Taht),
      
      sliderInput("Taat",
                  "Taat (average abandonment time)",
                  min = min_Taat,
                  max = max_Taat,
                  step = step_Taat,
                  animate = T,
                  value = step_Taat),
      
      sliderInput("Lambda",
                  "Lambda (inbound calls rate with higher prority than current)",
                  min = min_Lambda,
                  max = max_Lambda,
                  step = step_Lambda,
                  animate = T,
                  value = step_Lambda),
      
      sliderInput("K",
                  "K (level confidence)",
                  min = min_K,
                  max = max_K,
                  step = step_K,
                  animate = T,
                  value = step_K),
                  
      img(src="ewt.bmp", align = "center", width=560 , style="margin:5px")
    ),

    # Plot
    mainPanel(
      # Title
      titlePanel(
        fluidRow(
          column(1, img(src="bp_logo.png", align = "left", width=40)),
          column(10, align="center", "Estimation Wait Time")
        )        
      ),

      #Plot
      fluidRow(
          column(6, highchartOutput("plot1", height = "300")),
          column(6, highchartOutput("plot2", height = "300"))
        ),
      fluidRow(
          column(6, highchartOutput("plot3", height = "300")),
          column(6, highchartOutput("plot4", height = "300"))
        ),
      fluidRow(
          column(6, highchartOutput("plot5", height = "300")),
          column(6, highchartOutput("plot6", height = "300"))
        )
   )    
  )
)


#
# Create plot
#
createPlot <- function(line_x, line_y, x_label, point_x, point_y, line_color, hc_theme)
{
   line_label = sprintf("EWT(%s)", x_label)
   
   p <- highchart()                                                                       %>%
        hc_chart(type = "line")                                                           %>%
        hc_xAxis(categories = line_x)                                                     %>%
        hc_legend(enabled = FALSE)                                                        %>%
        hc_add_series(type = "line",  name = line_label, data=line_y)                     %>%
        hc_add_series(type="scatter", name="Present", data=list(c(point_x, point_y)))     %>%
        hc_add_theme(hc_theme)                                                            %>%
        hc_tooltip(crosshairs = list(TRUE, TRUE),
#                  backgroundColor = "#FCFFC5",
                   shared = TRUE,
                   borderWidth = 2,
                   headerFormat = sprintf("%s: {point.x:,.2f}<br>", x_label),
                   pointFormat = "EWT: {point.y:,.2f}")                                   %>% 
        hc_plotOptions(scatter = list(  marker = list(  radius = 5,
                                                        lineColor = '#FF0000',
                                                        fillColor = '#00FF00',
                                                        lineWidth = 2),
                                        color = "#FF0000"),
                                                        
                       line = list( color = line_color )
        )
}

#
# Server
#
server <- function(input, output) {

  # HC theme
  hc_theme = hc_theme_gridlight()

  EWT <- function(L, m, Taht, Taat, Lambda, K) { (L + K)  / (( m / Taht) + (L / Taat) - Lambda) }
  
  # EWT(L)    
  output$plot1 <- renderHighchart({
        
        current_ewt = EWT(input$L, input$m, input$Taht, input$Taat, input$Lambda, input$K)
        L = seq(min_L, max_L, step_L)

        return(createPlot(line_x      = L,
                          line_y      = EWT(L, input$m, input$Taht, input$Taat, input$Lambda, input$K),
                          x_label     = "L",
                          point_x     = input$L,
                          point_y     = current_ewt,
                          line_color  = "purple",
                          hc_theme    = hc_theme))
    })

  # EWT(m)    
  output$plot2 <- renderHighchart({
  
        current_ewt = EWT(input$L, input$m, input$Taht, input$Taat, input$Lambda, input$K)
        m = seq(min_m, max_m, step_m)

        return(createPlot(line_x      = m,
                          line_y      = EWT(input$L, m, input$Taht, input$Taat, input$Lambda, input$K),
                          x_label     = "m",
                          point_x     = input$m,
                          point_y     = current_ewt,
                          line_color  = "DarkOrange",
                          hc_theme    = hc_theme))
    })

  # EWT(Taht)
  output$plot3 <- renderHighchart({
        
        current_ewt = EWT(input$L, input$m, input$Taht, input$Taat, input$Lambda, input$K)
        Taht = seq(min_Taht, max_Taht, step_Taht)
        
        return(createPlot(line_x      = Taht,
                          line_y      = EWT(input$L, input$m, Taht, input$Taat, input$Lambda, input$K),
                          x_label     = "Taht",
                          point_x     = input$Taht,
                          point_y     = current_ewt,
                          line_color  = "green",
                          hc_theme    = hc_theme))
    })
    
  # EWT(Taat)
  output$plot4 <- renderHighchart({

        current_ewt = EWT(input$L, input$m, input$Taht, input$Taat, input$Lambda, input$K)
        Taat = seq(min_Taat, max_Taat, step_Taat)
                
        return(createPlot(line_x      = Taat,
                          line_y      = EWT(input$L, input$m, input$Taht, Taat, input$Lambda, input$K),
                          x_label     = "Taat",
                          point_x     = input$Taat,
                          point_y     = current_ewt,
                          line_color  = "darkblue",
                          hc_theme    = hc_theme))
    })

  # EWT(Lambda)    
  output$plot5 <- renderHighchart({
        
        current_ewt = EWT(input$L, input$m, input$Taht, input$Taat, input$Lambda, input$K)
        Lambda = seq(min_Lambda, max_Lambda, step_Lambda)

        return(createPlot(line_x      = Lambda,
                          line_y      = EWT(input$L, input$m, input$Taht, input$Taat, Lambda, input$K),
                          x_label     = "Lambda",
                          point_x     = input$Lambda,
                          point_y     = current_ewt,
                          line_color  = "DarkTurquoise",
                          hc_theme    = hc_theme))
    })

  
    
  output$plot6 <- renderHighchart({

    current_ewt = EWT(input$L, input$m, input$Taht, input$Taat, input$Lambda, input$K)
    K <- seq(min_K, max_K, step_K)

    return(createPlot(line_x      = K,
                      line_y      = EWT(input$L, input$m, input$Taht, input$Taat, input$Lambda, K),
                      x_label     = "K",
                      point_x     = input$K * scale_x_K,
                      point_y     = current_ewt,
                      line_color  = "black",
                      hc_theme    = hc_theme))
  })
}

# Application 
shinyApp(ui = ui, server = server)
