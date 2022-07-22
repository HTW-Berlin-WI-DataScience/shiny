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

library(plotly)


nullHypothesenMean <- 100
x <- seq(80, 150, .1)

k <- HTML("\\(\\sigma\\)")
# #Beta Upper Tail
# formel <- pnorm(upperOneTail(), input$alHyMean, input$stdErrorMean)
# 
# #Beta Lower Tail
# f <- pnorm(lowerOneTail(), input$alHyMean, input$stdErrorMean, lower.tail = F)
# 
# # Beta Two Tail
# pnorm(upperOneTail(), input$alHyMean, input$stdErrorMean) - pnorm(lowerOneTail(), input$alHyMean, input$stdErrorMean, lower.tail = F)


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons('tails', 'Chose your tail', choices = c('One Tail, Upper Tail', 'One Tail, Lower Tail', 'Two Tail')),
            sliderInput("alpha",
                        "Alpha, Type I Error rate:",
                        min = 0.00005,
                        max = 0.15,
                        value = 0.05),
            sliderInput('alHyMean', 'Alternative Hypothesis Mean',
                        min= 60,
                        max = 140,
                        value = 102),
            sliderInput('stdErrorMean', 'Standard Error of the mean Mean',
                        min= 0,1,
                        max = 10,
                        value = 1,
                        step= 0.1),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           htmlOutput("text")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

    upperOneTail <- eventReactive(c(input$alpha, input$stdErrorMean),{
      qnorm(1-input$alpha, nullHypothesenMean, input$stdErrorMean)
    })
    
    lowerOneTail <- eventReactive(c(input$alpha, input$stdErrorMean),{
      qnorm(input$alpha, nullHypothesenMean, input$stdErrorMean)
    })
    
    twoTailLeft <- eventReactive(c(input$alpha, input$stdErrorMean),{
      qnorm(input$alpha/2, nullHypothesenMean, input$stdErrorMean)
    })
    
    twoTailRight <- eventReactive(c(input$alpha, input$stdErrorMean),{
      qnorm(1-input$alpha/2, nullHypothesenMean, input$stdErrorMean)
    })
    
    
    upperTailBeta <- reactive({
      pnorm(upperOneTail(), input$alHyMean, input$stdErrorMean)
    })
    
    lowerTailBeta <- reactive({
      pnorm(lowerOneTail(), input$alHyMean, input$stdErrorMean, lower.tail = F)
    })
    
    twoTailBeta <- reactive({
      pnorm(upperOneTail(), input$alHyMean, input$stdErrorMean) - pnorm(lowerOneTail(), input$alHyMean, input$stdErrorMean, lower.tail = F)
    })
    
    output$l <- renderText({
      paste0("&beta;: ", upperTailBeta())
    })
    
    output$text <- renderText({
      if(input$tails == "One Tail, Upper Tail"){
        paste0("&beta;: ", round(upperTailBeta(), 3))
      }
      else if(input$tails == "One Tail, Lower Tail"){
        paste0("&beta;: ", round(lowerTailBeta(), 3))
      }
      else{
        paste0("&beta;: ", round(twoTailBeta(), 3))
      }
    })
  
    
    figur1 <- reactive({
      y <- dnorm(x, 100, 1)
      y2 <- dnorm(x, input$alHyMean, 1)
      dat2 <- cbind.data.frame(x,y, y2)

      
      p <- ggplot(dat2, aes(x = x, y = y)) +
        geom_line() +
        geom_area(mapping = aes(x = ifelse(x >upperOneTail(), x, 0)), fill = "red") +
        xlim(95,105) +
        geom_vline(aes(xintercept=upperOneTail()),
                   linetype="solid", size=1, colour="blue") +
        geom_hline(aes(yintercept=0),
                   linetype="solid", size=1, colour="black") +
        annotate("text", label = "fail to reject", x = 100, y = -.01, size = 4, colour = "blue")+
        annotate("text", label = "reject", x = 2.5, y = -.01, size = 4, colour = "red") +
        labs(x="Z value", y="Density") +
        theme_minimal()
      
      p +  geom_area(mapping = aes(x = ifelse(x<upperOneTail(), x, 0), y=y2), fill = "skyblue") +
        geom_line(mapping=aes(x=x,y=y2)) +
        geom_line(mapping=aes(x=x,y=y)) +
        annotate("text", label = expression(beta), x = 1.4, y = .06, size = 5, colour = "black")
    
      
    })
    
    figur2 <- reactive({
      y <- dnorm(x, 100, 1)
      y2 <- dnorm(x, input$alHyMean, 1)
      dat2 <- cbind.data.frame(x,y, y2)
      
      
      p <- ggplot(dat2, aes(x = x, y = y)) +
        geom_line() +
        geom_area(mapping = aes(x = ifelse(x < lowerOneTail(), x, 0)), fill = "red") +
        xlim(95,105) +
        geom_vline(aes(xintercept=lowerOneTail()),
                   linetype="solid", size=1, colour="blue") +
        geom_hline(aes(yintercept=0),
                   linetype="solid", size=1, colour="black") +
        annotate("text", label = "fail to reject", x = 100, y = -.01, size = 4, colour = "blue")+
        annotate("text", label = "reject", x = 2.5, y = -.01, size = 4, colour = "red") +
        labs(x="Z value", y="Density") +
        theme_minimal()
      p +
        geom_area(mapping = aes(x = ifelse(x>lowerOneTail(), x, 0), y=y2), fill = "skyblue") +
        geom_line(mapping=aes(x=x,y=y2)) +
        geom_line(mapping=aes(x=x,y=y)) +
        annotate("text", label = expression(beta), x = 1.4, y = .06, size = 5, colour = "black")
      
    })
    
    figur3 <- reactive({
      y <- dnorm(x, 100, 1)
      y2 <- dnorm(x, input$alHyMean, 1)
      dat2 <- cbind.data.frame(x,y, y2)
      
      
      p <- ggplot(dat2, aes(x = x, y = y)) +
        geom_line() +
        geom_area(mapping = aes(x = ifelse(x > upperOneTail(), x, 0)), fill = "red") +
        xlim(95,105) +
        geom_area(mapping = aes(x = ifelse(x < lowerOneTail(), x, 0)), fill = "red") +
        xlim(95,105) +
        geom_vline(aes(xintercept=upperOneTail()),
                   linetype="solid", size=1, colour="blue") +
        geom_vline(aes(xintercept=lowerOneTail()),
                   linetype="solid", size=1, colour="blue") +
        geom_hline(aes(yintercept=0),
                   linetype="solid", size=1, colour="black") +
        annotate("text", label = "fail to reject", x = 100, y = -.01, size = 4, colour = "blue")+
        annotate("text", label = "reject", x = 2.5, y = -.01, size = 4, colour = "red") +
        labs(x="Z value", y="Density") +
        theme_minimal()
      p +
        geom_area(mapping = aes(x = ifelse(x<upperOneTail() & x>lowerOneTail(), x, 0), y=y2), fill = "skyblue") +
        geom_line(mapping=aes(x=x,y=y2)) +
        geom_line(mapping=aes(x=x,y=y)) +
        annotate("text", label = expression(beta), x = 1.4, y = .06, size = 5, colour = "black")
      
    })
    
    output$distPlot <- renderPlot({
      
      if(input$tails == "One Tail, Upper Tail"){
        figur1()
      }
      else if(input$tails == "One Tail, Lower Tail"){
        figur2()
      }
      else{
        figur3()
      }
       
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
