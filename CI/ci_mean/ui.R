library("shiny")
library("shinydashboard")
library("shinyBS")


shinyUI(fluidPage(
    
    titlePanel("Confidence Interval"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            bsCollapse(id = "cE", multiple = TRUE, open = "Confidence interval parameter",
                       bsCollapsePanel("Confidence interval parameter", uiOutput("conflevelUI"), uiOutput("sizeUI"), uiOutput("sigmaUI")),
                       bsCollapsePanel("Sample drawing", uiOutput("goUI"), uiOutput("resetUI"),uiOutput("speedUI")),
                       bsCollapsePanel("Data choice", uiOutput("datasetUI"), uiOutput("variableUI"))
            ),
            p(actionLink(
                inputId = "github",
                label = "Hier",
                icon = icon("github"),
                href = "https://github.com/Kale14/mmstat4/tree/main/inst/examples/stat/confidence_mean",
                onclick = "window.open('https://github.com/Kale14/mmstat4/tree/main/inst/examples/stat/confidence_mean')"), "geht es zum Code der HU")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("outputConfPlot"),
            plotOutput("outputSamplePlot")
        )
    )
))
