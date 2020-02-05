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
# ui <- fluidPage(
#     
#     # App title ----
#     titlePanel("Airplane Seat Simulation"),
#     
#     # Sidebar layout with input and output definitions ----
#     sidebarLayout(
#         
#         # Sidebar panel for inputs ----
#         sidebarPanel(
#             
#             sliderInput("prob", 
#                          "Percent Chance of an Individual Showing Up", 
#                          value=95, 
#                          min=1, 
#                          max=100),
#             sliderInput("tixcost", 
#                          "Cost per Ticket", 
#                          value=500, 
#                          min=100, 
#                          max=500),
#            sliderInput("payout", 
#                          "Pay Out per Person to Switch", 
#                          value=250, 
#                          min=0, 
#                          max=750),
#            sliderInput("numtix", 
#                          "Number of tickets sold", 
#                          value=200, 
#                          min=200, 
#                          max=300),
#            actionButton("runonce", "Run Once"),
#            actionButton("run100", "Run 100 Times"),
#         ),
#         
#       # Main panel for displaying outputs ----
#       mainPanel(
#            tabsetPanel(type = "tabs",
#                        tabPanel("Run Once", 
#                        h4("Run Simulation Once:"), 
#                        h6("Use the inputs to the left change the percent chance that any individual shows up for the flight, the cost per ticket, the payout by airlines to anyone who has to switch planes and the number of tickets sold per plane (assuming that each plane has 200 seats). Click 'Run Once' to see the airline's net profit or loss for one plane. Click the button again to see how the simulation might vary given the probability of showing up. Change the parameters and click the button again to see how the result changes."), 
#                        textOutput("result2")),
#                        tabPanel("Run 100 Times",
#                        h4("Run Similation 100 Times:"),
#                        h6("Use the inputs to the left change the percent chance that any individual shows up for the flight, the cost per ticket, the payout by airlines to anyone who has to switch planes and the number of tickets sold per plane (assuming that each plane has 200 seats). Click 'Run 100 Times' to see the airline's net profit or loss for 100 flights. Change the parameters and click the button again to see how the result changes."), 
#                        plotOutput("hist"), 
#                        textOutput("result1"))
#            )
#         )
#     )
# )

ui <- fluidPage(
  
  # App title ----
  titlePanel("Airplane Seat Simulation"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      sliderInput("prob", 
                  "Percent Chance of an Individual Showing Up", 
                  value=95, 
                  min=1, 
                  max=100),
      sliderInput("tixcost", 
                  "Cost per Ticket", 
                  value=500, 
                  min=100, 
                  max=500),
      sliderInput("payout", 
                  "Pay Out per Person to Switch", 
                  value=250, 
                  min=0, 
                  max=750),
      sliderInput("numtix", 
                  "Number of tickets sold", 
                  value=200, 
                  min=200, 
                  max=300),
      actionButton("runonce", "Run Once"),
      actionButton("run100", "Run 100 Times"),
      
      
      # Include clarifying text ----
      helpText("Use the above inputs to change the percent chance that any individual shows up, the cost per ticket, the pay out by airlines to anyone who has to switch planes, and the number of tickets to sell per plane (assuming that each plane has 200 seats). Run once to see the airline's net profit or loss for one plane and run 100 times to see net profit or loss over 100 flights. Note: this app assumes that 96% capacity is the break even point for the airline" )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      h4("Run Similation 100 Times:"),
      plotOutput("hist"),
      textOutput("result1"),
      
      
      h4("Run Simulation Once:"),
      textOutput("result2")
      
      
    )
    
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output,session) {
    
    observeEvent(input$runonce, {
            x <- sample(c(0,1),
                        size=input$numtix,
                        prob=c(1-(input$prob/100), input$prob/100),
                        replace=TRUE)
            y <- 200 - sum(x)
            if(y>=0){
              rev <- input$numtix*input$tixcost-.96*200*input$tixcost-(input$numtix-sum(x))*input$tixcost
            }
            else if(y<0){
              rev <- input$numtix*input$tixcost-192*input$tixcost-(input$numtix-200)*input$tixcost-abs(y)*input$payout
            }
            if(rev < 0){
              output$result2 <- renderText({
                paste0("The airline sold ",
                      input$numtix,
                      " tickets at $",
                      input$tixcost,
                      " per ticket. Only ",
                      sum(x),
                      " people showed up to the flight. The airline LOST $",
                      abs(rev),
                      " on this flight.")
              })
            }
            else if(rev >= 0){
              output$result2 <- renderText({
                paste0("The airline sold ",
                      input$numtix,
                      " tickets at $",
                      input$tixcost,
                      " per ticket. Only ",
                      sum(x),
                      " people showed up to the flight. The airline MADE $",
                      abs(rev),
                      " on this flight.")
              })
            }
            
          
            
    })

    observeEvent(input$run100, {
            x <- y <- rep(NA,100)
            for(i in 1:100){
                x[i] <- sum(
                        sample(c(0,1),
                        size=input$numtix,
                        prob=c(1-(input$prob/100), input$prob/100),
                        replace=TRUE))
                y[i] <- 200-x[i]
                if(y[i]<0){
                    x[i] <- input$numtix*input$tixcost-.96*200*input$tixcost-(input$numtix-200)*input$tixcost-abs(y[i])*input$payout
                }
                else if(y[i]>=0){
                    x[i] <- input$numtix*input$tixcost-.96*200*input$tixcost-(input$numtix-x[i])*input$tixcost
                }
            }

            if(sum(x)>=0){
                output$result1 <- renderText({
                    paste0("The airline MADE $",
                          sum(x),
                          " overall on these 100 flights.")
                })
            }

            if(sum(x)<0){
                output$result1 <- renderText({
                    paste0("The airline LOST $",
                          abs(sum(x)),
                          " overall on these 100 flights.")
                })
            }

            output$hist <- renderPlot({
                hist(x,
                     xlab="Net Gains or Losses",
                     main="Histogram of Net Gains/Losses per Flight")
                     #abline(v=0,col="red", lwd=3)
            })

    })

    
}

# Create Shiny app ----
shinyApp(ui, server)
