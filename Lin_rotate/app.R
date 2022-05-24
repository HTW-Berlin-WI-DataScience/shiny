library(shiny)

l <- 10 
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("winkel",
                        "Winkel:",
                        min = 0,
                        max = 360,
                        value = 45)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           textOutput("text"),
           plotOutput("plot2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  x <- eventReactive(input$winkel,{
    l * cos(input$winkel * pi / 180)
  })
  
  y <- eventReactive(input$winkel,{
    l * sin(input$winkel * pi / 180)
  })
  
  output$text <- renderText({
    paste("Das ist x2, ", round(x(), 3) , "und das y2,", round(y(), 3))
  })

  output$distPlot <- renderPlot({
    x1 <- c(0, x())
    y1 <- c(0, y())
    text("das ist x", x1, "und das y", y1 )
    plot(x1, y1, type = "b", pch = 19, 
         col = "red", xlab = "x", ylab = "y",, xlim = c(-10,10), ylim = c(-10,10))
  })
  
  output$plot2 <- renderPlot({
    x1 <- c(0, x())
    y1 <- c(0, y())
    text("das ist x", x1, "und das y", y1 )
    plot(0, 0, type = "b", pch = 19, 
         col = "red", xlab = "x", ylab = "y")
    lines(x1, y1, pch = 18, col = "blue", type = "b")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
