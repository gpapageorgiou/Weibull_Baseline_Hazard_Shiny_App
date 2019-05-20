#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tags$head(
        tags$style(
            HTML("
            input#Intercept2.form-control.shiny-bound-input {
                background-color: #FF265C;
                color: #FFFFFF;
                font-weight: bold;
                border: #FFFFFF;
                border-style: solid;
                border-width: 5px;
            }
            
            input#Phi2.form-control.shiny-bound-input {
                background-color: #FF265C;
                color: #FFFFFF;
                font-weight: bold;
            }
                 ")
        )
    ),

    # Application title
    title = "Weibull Baseline Hazard Functions for 3 Transitions",
    
    # Add plot output
    plotOutput('plot', height = '600px'), 
    
    # Add horizontal line
    HTML("<hr style = 'border-color: #01EBE0; border-width: 1px; margin: 20px 20px; border-style: dotted' >"), 
    
    fluidRow(
        column(3, div(style = 'height: 200px; background-color : #1D7BFF;',
               tags$span(style = 'color : #FFFFFF; font-weight : bold; font-size : 18px', "Transition 1"),
               HTML("<hr style = 'border-color: #FFFFFF; border-width: 1px; 
                           margin: 5px 5px; border-style: solid' >"), 
               numericInput('Intercept1', tags$span(style = 'color : #FFFFFF', 'Intercept'), 
                            min = -1000, max = 1000, value = 0, step = 0.5), 
               br(), 
               numericInput('Phi1', tags$span(style = 'color : #FFFFFF', 'Phi'), 
                            min = -1000, max = 1000, value = 0, step = 0.5)
               )), 
        column(3, div(style = 'height: 200px; background-color : #EB2143;',
               tags$span(style = 'color: #FFFFFF; font-weight: bold; font-size: 18px', 'Transition 2'), 
               HTML("<hr style = 'border-color: #FFFFFF; border-width: 1px; 
                           margin: 5px 5px; border-style: solid' >"),
               numericInput('Intercept2', tags$span(style = 'color: #FFFFFF', 'Intercept'), 
                            min = -1000, max = 1000, value = 0, step = 0.5), 
               br(), 
               numericInput('Phi2', tags$span(style = 'color: #FFFFFF', 'Phi'), 
                            min = -1000, max = 1000, value = 0, step = 0.5))
        ), 
        column(3, div(style = 'height: 200px; background-color: #FF265C;',
               tags$span(style = 'color: #FFFFFF; font-weight: bold; font-size: 18px', 'Transition 3'), 
               HTML("<hr style = 'border-color: #FFFFFF; border-width: 1px; 
                           margin: 5px 5px; border-style: solid' >"),
               numericInput('Intercept3', tags$span(style = 'color: #FFFFFF', 'Intercept'), 
                            min = -1000, max = 1000, value = 0, step = 0.5), 
               br(), 
               numericInput('Phi3', tags$span(style = 'color: #FFFFFF', 'Phi'), 
                            min = -1000, max = 1000, value = 0, step = 0.5)
        )), 
        column(3, div(style = 'height: 200px; background-color: #000000;', 
                      tags$span(style = 'color: #FFFFFF; font-weight: bold; font-size: 18px', 'Time Range'), 
                      HTML("<hr style = 'border-color: #FFFFFF; border-width: 1px; 
                           margin: 5px 5px; border-style: solid' >"), 
               sliderInput('time', tags$span(style = 'color: #FFFFFF', 'time'), 
                           min = 0, max = 1000, value = c(0, 12))
        ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    x <- reactive(seq(input$time[[1]], input$time[[2]], length.out = 1000))
    y1 <- reactive(exp(input$Intercept1) * input$Phi1 * (x()^(input$Phi1 - 1)))
    y2 <- reactive(exp(input$Intercept2) * input$Phi2 * (x()^(input$Phi2 - 1)))
    y3 <- reactive(exp(input$Intercept3) * input$Phi3 * (x()^(input$Phi3 - 1)))

    output$plot <- renderPlot({
        
        ggplot() + geom_line(aes(x = x(), y = y1()), color = '#000000') + 
            geom_line(aes(x = x(), y = y2()), color = '#EB2143') + 
            geom_line(aes(x = x(), y = y3()), color = '#3A54FF') + 
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
            xlab('Time') + ylab("Baseline Hazard")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
