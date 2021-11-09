library("shinydashboard")


shinyUI(fluidPage(
    
    # Application title
    titlePanel("Confidence Interval"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            uiOutput("conflevelUI"),
            uiOutput("sizeUI"),
            uiOutput("sigmaUI"),
            uiOutput("goUI"),
            uiOutput("resetUI"),
            uiOutput("speedUI"),
            uiOutput("datasetUI"),
            uiOutput("variableUI"),
            p(actionLink(
                inputId = "github",
                label = "Hier",
                icon = icon("github"),
                href = "https://github.com/Kale14/mmstat4/tree/main/inst/examples/stat/confidence_mean",
                onclick = "window.open('https://github.com/Kale14/mmstat4/tree/main/inst/examples/stat/confidence_mean')"), "kommt ihr zum Code der HU")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("outputConfPlot"),
            plotOutput("outputSamplePlot")
        )
    )
))