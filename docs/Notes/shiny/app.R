#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            fluidRow(
              column(6,
                   numericInput("max_bins",
                         label = "Set max for slider",
                         min = 2,
                         value = 50)),
              column(6, 
                     actionButton("update_max",
                                  "Update the Slider")
                     )
            ),
            checkboxInput("show_sentence", "Show info about our histogram?"),
            actionButton('browse', "Check browser?")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           conditionalPanel("input.show_sentence",
                            uiOutput("hist_info")
                            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    hist_object <- reactive({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white',
           xlab = 'Waiting time to next eruption (in mins)',
           main = 'Histogram of waiting times')
    })
    
    output$distPlot <- renderPlot({
      hist_object()
    })
    
    observeEvent(input$update_max, {
      updateSliderInput(session, 
                        "bins",
                        max = input$max_bins)
    })
    
    output$hist_info <- renderUI({
      h <- hist_object()
      highest_bin <- max(h$counts)
      start_bin <- h$breaks[which(h$counts == highest_bin)[1]]
      h4(paste0("The bin with the largest number of observations starts at ", start_bin, " and the number of observations in that bin is ", highest_bin))
    })
    
    observeEvent(input$browse, {
      browser()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
