library("shinydashboard")


shinyUI(fluidPage(
    
    # Application title
    titlePanel("MM*Stat"),
    
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
            p("Based on code from Sigbert Klinke ", actionLink(
                inputId = "github",
                label = "Github",
                icon = icon("github"),
                href = "https://github.com/sigbertklinke/mmstat4/tree/main/inst/examples/stat/confidence_mean",
                onclick = "window.open('https://github.com/Kale14/mmstat4/tree/main/inst/examples/stat/confidence_mean')"))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("outputConfPlot"),
            plotOutput("outputSamplePlot")
        )
    )
))