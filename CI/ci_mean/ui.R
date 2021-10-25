library("shiny")
library("shinydashboard")


dashboardPage(
    dashboardHeader(title="MM*Stat"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(text=gettext("Confidence interval parameter"), selected=TRUE,
                     menuSubItem(uiOutput("conflevelUI"), icon = NULL),
                     menuSubItem(uiOutput("sizeUI"), icon = NULL),
                     menuSubItem(uiOutput("sigmaUI"),icon = NULL)
            ),
            menuItem(text=gettext("Sample drawing"), selected=TRUE,
                     menuSubItem(uiOutput("goUI"), icon = NULL),
                     menuSubItem(uiOutput("resetUI"), icon = NULL),
                     menuSubItem(uiOutput("speedUI"), icon = NULL)
            ),
            menuItem(text=gettext("Data choice"), selected=TRUE,
                     menuSubItem(uiOutput("datasetUI"), icon = NULL),
                     menuSubItem(uiOutput("variableUI"), icon = NULL)
            )
        )
    ),
    dashboardBody(
        fluidRow(
            column(width = 12, 
                   box(title=gettext("Confidence intervals for the mean"),
                       plotOutput("outputConfPlot"),
                       plotOutput("outputSamplePlot", height = "200px"),
                   ),
                   box(
                       actionLink(
                           inputId = "github",
                           label = "HU Code- Sigbert Klinke",
                           icon = icon("github"),
                           href = "https://github.com/Kale14/mmstat4/tree/main/inst/examples/stat/confidence_mean",
                           onclick = "window.open('https://github.com/Kale14/mmstat4/tree/main/inst/examples/stat/confidence_mean')"))
            )
        )
    ),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "test.css")
    )
)
