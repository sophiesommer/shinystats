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

# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Survey Data"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Choose the sample size ----
            sliderInput("obs", 
                         label = h3("Select the sample size:"),
                         value = 10,
                         min = 1,
                         max = 100),
            
            hr(),
            fluidRow(column(3, verbatimTextOutput("value"))),
            
            # Input: Choose the sample size ----
            sliderInput("qs", 
                         label = h3("Choose the number of questions:"),
                         value = 1,
                         min = 1,
                         max = 5,
                         step= 1),
            
            actionButton("resample", "Re-Sample"),
            
            # Include clarifying text ----
            helpText("Use the inputs above to simulate data from a hypothetical personality test with up to five different two-types-of-people questions.")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            h4("Bar Plot of Personality Types"),
            plotOutput("barplot"),
            
            # Output: Header + table of distribution ----
            h4("Response Data"),
            tableOutput("view")
        )
        
    )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
    
    # Generate some data
    set.seed(333)
    data <- data.frame(Q1=sample(c("O", "U"), 100, prob=c(.5,.5), replace=T),
                       Q2=sample(c("C", "S"), 100, prob=c(.3,.7), replace=T),
                       Q3=sample(c("E", "F"), 100, prob=c(.6,.4), replace=T),
                       Q4=sample(c("G", "H"), 100, prob=c(.5,.5), replace=T),
                       Q5=sample(c("I", "J"), 100, prob=c(.8,.2), replace=T))
    data$type <- apply(data, 1, paste, collapse="")
    row.names(data) <- paste0("student",1:100)
    colnames(data) <- c(paste0("Q",1:5), "Personality Type")
    
    observeEvent(input$resample, {
        data <- data.frame(Q1=sample(c("O", "U"), 100, prob=c(.5,.5), replace=T),
                           Q2=sample(c("C", "S"), 100, prob=c(.3,.7), replace=T),
                           Q3=sample(c("E", "F"), 100, prob=c(.6,.4), replace=T),
                           Q4=sample(c("G", "H"), 100, prob=c(.5,.5), replace=T),
                           Q5=sample(c("I", "J"), 100, prob=c(.8,.2), replace=T))
        data$type <- apply(data, 1, paste, collapse="")
        row.names(data) <- paste0("student",1:100)
        colnames(data) <- c(paste0("Q",1:5), "Personality Type")
        
        output$barplot <- renderPlot({
            data2 <- data[1:input$obs,1:input$qs,drop=F]
            data2$type <- apply(data2, 1, paste, collapse="")
            colnames(data2) <- c(paste0("Q",1:input$qs), "Personality Type")
            unique <- length(unique(data2$`Personality Type`))
            unique2 <- unique
            if(unique2 == 1){
                unique2 <- 2
            }
            labsize <- (1/(.1+log(unique2)))*2
            # Render a barplot
            barplot(table(data2[1:input$obs,"Personality Type"]), 
                    main=paste0("Personality Types (",
                                unique," out of ",2^input$qs,
                                " possible types observed)" ),
                    ylab="Number of Students",
                    xlab="Personality Type",
                    cex.names = labsize)
        })
        
        # Show the first "n" observations ----
            output$view <- renderTable({
            data2 <- data[,1:input$qs,drop=F]
            data2$type <- apply(data2, 1, paste, collapse="")
            colnames(data2) <- c(paste0("Q",1:input$qs), "Personality Type")
            head(data2, n = input$obs)},
            rownames=TRUE)
    })

    output$barplot <- renderPlot({
        data2 <- data[1:input$obs,1:input$qs,drop=F]
        data2$type <- apply(data2, 1, paste, collapse="")
        colnames(data2) <- c(paste0("Q",1:input$qs), "Personality Type")
        unique <- length(unique(data2$`Personality Type`))
        unique2 <- unique
        if(unique2 == 1){
            unique2 <- 2
        }
        labsize <- (1/(.1+log(unique2)))*2
        # Render a barplot
        barplot(table(data2[1:input$obs,"Personality Type"]), 
                main=paste0("Personality Types (",
                            unique," out of ",2^input$qs,
                            " possible types observed)" ),
                ylab="Number of Students",
                xlab="Personality Type",
                cex.names = labsize)
    })
    
    # Show the first "n" observations ----
    output$view <- renderTable({
        data2 <- data[,1:input$qs,drop=F]
        data2$type <- apply(data2, 1, paste, collapse="")
        colnames(data2) <- c(paste0("Q",1:input$qs), "Personality Type")
        head(data2, n = input$obs)},
        rownames=TRUE)
    
}

# Create Shiny app ----
shinyApp(ui, server)
