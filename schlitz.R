#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#download/load shiny
if(!require(shiny)) {
    install.packages("shiny")
}
library(shiny)
if(!require(ggplot2)) {
    install.packages("ggplot2")
}
library(dplyr)
if(!require(dplyr)) {
    install.packages("dplyr")
}
library(dplyr)
# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Schlitz Simulation"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            sliderInput("acceptable", 
                        "Acceptable (Desirable) Outcome (%)", 
                        value=50, 
                        min=1, 
                        max=100),
            sliderInput("prob", 
                        "Probability of a Participant Preferring Schlitz (%)", 
                        value=50, 
                        min=0, 
                        max=100),
            sliderInput("breaks", 
                        "Adjust Breaks", 
                        value=20, 
                        min=1, 
                        max=100),
            selectInput("sampsize", "Choose a sample size:",c(1,10,100,500,1000)),
            actionButton("runonce", "Run Once"),
            actionButton("run100", "Run 100 Times"),
            
            # Include clarifying text ----
            helpText("Use the above inputs to change the acceptable (desirable) outcome (i.e., the % of people preferring Schlitz which would be acceptable in order to run the ad), probability of any given participant preferring schlitz (as a percent), and the sample size. Then use the Run Once button to run the experiment once (and see the percent of people who preferred Schlitz), or the Run 100 Times button to run the experiment 100 times (and view a histogram of the results)." )
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            
            h4("Run Experiment Once:"),
            textOutput("result"),
            br(),
            br(),
            
            h4("Run Experiment 100 Times:"),
            plotOutput("hist"),
            textOutput("successes")
        
            
            
        )
        
    )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output,session) {
    rv <- reactiveValues(i = 0)
    
    output$result <- renderText({
        if(rv$i == 2) {
            x <- sample(c(0,1),
                        size=input$sampsize,
                        prob=c(1-(input$prob/100), input$prob/100),
                        replace=TRUE)
            paste(sum(x),
                  " participant(s) out of the",
                  input$sampsize, 
                  "total participant(s) (i.e., ",
                  mean(x)*100,
                  "%) \n preferred Schlitz during this taste test.")
        } else if(rv$i == 1) {
            paste("Taste testing...")
        }
    })
    observeEvent(input$runonce, {
        rv$i <- 0
        observe({
            isolate({
                rv$i <- rv$i + 1
            })
            
            if (isolate(rv$i) < 2){
                invalidateLater(250, session)
            }
        })
        
        
    })
    
    
    
    observeEvent(input$run100, {
        y <- rep(NA,100)
        for(i in 1:100){
            y[i] <- mean(
                sample(c(0,1),
                       size=input$sampsize,
                       prob=c(1-(input$prob/100), input$prob/100),
                       replace=TRUE))
        }
        output$hist <- renderPlot({
            hist(y*100, 
                 xlab="Percent Preferring Schlitz", 
                 xlim=c(0,100),
                 breaks=input$breaks,
                 main="Histogram of the percent of people who preferred Schlitz")
            abline(v=input$acceptable,col="red", lwd=3)
        })
        output$successes <- renderText({
            paste(sum(y*100>=input$acceptable),
                  "% of the experiments met your desired criterion")
        })
        
    })
    
    
}

# Create Shiny app ----
shinyApp(ui, server)
