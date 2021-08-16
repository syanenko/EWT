#
# Contact center estimation wait time calculation model
#
# (C) 2017 Bright Pattern
#
# Author: Sergey Yanenko
#
library(ggplot2)
library(shiny)
library(plotly)

#
# Constants
#

line_width = 0.35

min_L  = 0
max_L  = 100
step_L = 1

min_m  = 0
max_m  = 100
step_m = 1

min_Taht  = 1
max_Taht  = 100
step_Taht = 1

min_Taat  = 1
max_Taat  = 100
step_Taat = 1

min_Lambda  = 1
max_Lambda  = 100
step_Lambda = 1

min_K  = 0
max_K  = 1
step_K = 0.01

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
  tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: Dark Turquoise}}")),
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
                  value = 30),
      
      sliderInput("m",
                  "m (total number of agents)",
                  min = min_m,
                  max = max_m,
                  value = step_m),
      
      sliderInput("Taht",
                  "Taht (average handling time)",
                  min = min_Taht,
                  max = max_Taht,
                  value = step_Taht),
      
      sliderInput("Taat",
                  "Taat (average abandonment time)",
                  min = min_Taat,
                  max = max_Taat,
                  value = step_Taat),
      
      sliderInput("Lambda",
                  "Lambda (inbound calls rate with higher prority than current)",
                  min = min_Lambda,
                  max = max_Lambda,
                  value = step_Lambda),
      
      sliderInput("K",
                  "K (level confidence)",
                  min = min_K,
                  max = max_K,
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
          column(6, plotlyOutput("plot1", height = "300")),
          column(6, plotlyOutput("plot2", height = "300"))
        ),
      fluidRow(
          column(6, plotlyOutput("plot3", height = "300")),
          column(6, plotlyOutput("plot4", height = "300"))
        ),
      fluidRow(
          column(6, plotlyOutput("plot5", height = "300")),
          column(6, plotlyOutput("plot6", height = "300"))
        )
   )    
  )
)

#
# Server
#
server <- function(input, output) {

  EWT <- function(L, m, Taht, Taat, Lambda, K) { (L + K)  / (( m / Taht) + (L / Taat) - Lambda) }
  
  # EWT(L)    
  output$plot1 <- renderPlotly({

        m      = input$m
        L      = input$L
        Taht   = input$Taht
        Taat   = input$Taat
        Lambda = input$Lambda
        K      = input$K
        current_ewt <- EWT(L, m, Taht, Taat, Lambda, K)
        
        L = seq(min_L, max_L, step_L)
        df = data.frame ( L = L, EWT = EWT(L, m, Taht, Taat, Lambda, K) )
        p1 <- ggplotly(ggplot(df, aes(x = L, y = EWT)) + 
                         geom_path(color='purple', size=line_width) +
                         geom_point(x = input$L, y = current_ewt, color="red") +
                         theme(axis.title.y=element_blank(),
                               axis.title.x=element_blank(),
                               panel.border = element_rect(colour = "black", fill=NA, size=0.2))) %>%
                layout(annotations = list(list(x = 0.5, y = 1, text = "EWT(L)", showarrow = F, xref='paper', yref='paper'))) %>%
                config(displayModeBar = F)

        return (p1)
    })

  # EWT(m)    
  output$plot2 <- renderPlotly({

        m      = input$m
        L      = input$L
        Taht   = input$Taht
        Taat   = input$Taat
        Lambda = input$Lambda
        K      = input$K
        current_ewt <- EWT(L, m, Taht, Taat, Lambda, K)

        m = seq(min_m, max_m, step_m)
        df = data.frame ( m = m, EWT = EWT(L, m, Taht, Taat, Lambda, K) )
        p2 <- ggplotly(ggplot(df, aes(x = m, y = EWT)) + 
                         geom_path(color='DarkOrange', size=line_width) +
                         geom_point(x = input$m, y = current_ewt, color="red") +
                         theme(axis.title.y=element_blank(),
                               axis.title.x=element_blank(),
                               panel.border = element_rect(colour = "black", fill=NA, size=0.2))) %>%
                layout(annotations = list(list(x = 0.5, y = 1, text = "EWT(m)", showarrow = F, xref='paper', yref='paper'))) %>%
                config(displayModeBar = F)

        return (p2)
    })

  # EWT(Taht)
  output$plot3 <- renderPlotly({

        m      = input$m
        L      = input$L
        Taht   = input$Taht
        Taat   = input$Taat
        Lambda = input$Lambda
        K      = input$K
        current_ewt <- EWT(L, m, Taht, Taat, Lambda, K)
      
        Taht = seq(min_Taht, max_Taht, step_Taht)
        df = data.frame ( Taht = Taht, EWT = EWT(L, m, Taht, Taat, Lambda, K) )
        p3 <- ggplotly(ggplot(df, aes(x = Taht, y = EWT)) + 
                         geom_path(color='darkgreen', size=line_width) +
                         geom_point(x = input$Taht, y = current_ewt, color="red") +
                         theme(axis.title.y=element_blank(),
                               axis.title.x=element_blank(),
                               panel.border = element_rect(colour = "black", fill=NA, size=0.2))) %>%
                layout(annotations = list(list(x = 0.5,  y = 1, text = "EWT(Taht)", showarrow = F, xref='paper', yref='paper'))) %>%
                config(displayModeBar = F)

        return (p3)
    })
    
  # EWT(Taat)
  output$plot4 <- renderPlotly({

        m      = input$m
        L      = input$L
        Taht   = input$Taht
        Taat   = input$Taat
        Lambda = input$Lambda
        K      = input$K
        current_ewt <- EWT(L, m, Taht, Taat, Lambda, K)

        Taat = seq(min_Taat, max_Taat, step_Taat)
        df = data.frame ( Taat = Taat, EWT = EWT(L, m, Taht, Taat, Lambda, K) )
        p4 <- ggplotly(ggplot(df, aes(x = Taat, y = EWT)) + 
                         geom_path(color='darkblue', size=line_width) +
                         geom_point(x = input$Taat, y = current_ewt, color="red") +
                         theme(axis.title.y=element_blank(),
                               axis.title.x=element_blank(),
                               panel.border = element_rect(colour = "black", fill=NA, size=0.2))) %>%
                layout(annotations = list(list(x = 0.5,  y = 1, text = "EWT(Taat)", showarrow = F, xref='paper', yref='paper'))) %>%
                config(displayModeBar = F)

        return (p4)
    })

  # EWT(Lambda)    
  output$plot5 <- renderPlotly({

        m      = input$m
        L      = input$L
        Taht   = input$Taht
        Taat   = input$Taat
        Lambda = input$Lambda
        K      = input$K
        current_ewt <- EWT(L, m, Taht, Taat, Lambda, K)
        
        Lambda = seq(min_Lambda, max_Lambda, step_Lambda)
        df = data.frame ( Lambda = Lambda, EWT = EWT(L, m, Taht, Taat, Lambda, K) )
        p5 <- ggplotly(ggplot(df, aes(x = Lambda, y = EWT)) + 
                         geom_path(color='Dark Turquoise', size=line_width) +
                         geom_point(x = input$Lambda, y = current_ewt, color="red") +
                         theme(axis.title.y=element_blank(),
                               axis.title.x=element_blank(),
                               panel.border = element_rect(colour = "black", fill=NA, size=0.2))) %>%
                layout(annotations = list(list(x = 0.5,  y = 1, text = "EWT(Lambda)", showarrow = F, xref='paper', yref='paper'))) %>%
                config(displayModeBar = F)
              
        return (p5)
    })

  # EWT(K)
  output$plot6 <- renderPlotly({

    m      = input$m
    L      = input$L
    Taht   = input$Taht
    Taat   = input$Taat
    Lambda = input$Lambda
    K      = input$K
    current_ewt <- EWT(L, m, Taht, Taat, Lambda, K)
    
    K = seq(min_K, max_K, step_K)
    df = data.frame ( K = K, EWT = EWT(L, m, Taht, Taat, Lambda, K) )
    p6 <- ggplotly(ggplot(df, aes(x = K, y = EWT)) + 
                     geom_path(color='black', size=line_width) +
                     geom_point(x = input$K, y = current_ewt, color="red") +
                     theme(axis.title.y=element_blank(),
                           axis.title.x=element_blank(),
                           panel.border = element_rect(colour = "black", fill=NA, size=0.2))) %>%
            layout(annotations = list(list(x = 0.5,  y = 1, text = "EWT(K)", showarrow = F, xref='paper', yref='paper')))  %>%
            config(displayModeBar = F)

    return (p6)
  })
}

# Application 
shinyApp(ui = ui, server = server)
