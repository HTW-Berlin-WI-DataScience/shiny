
library(shiny)
library(ggplot2)
library(fields)

mietspiegel <- read.table("./miete03.asc", header=TRUE)
# myseed <- round(runif(1, 1, 10000))

# lineare Regression (Nettomiete ueber Wohnflaeche) "linear model"
mieten_regression <- lm(mietspiegel$nm ~ mietspiegel$wfl)

# UI logic
ui <- fluidPage(
    
  title = 'Lineare Regression auf Stichproben',
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    position = "left",  
    #position = "right",
    
    sidebarPanel(
      
      # Application title
      titlePanel(title = h3('Lineare Regression: Unsicherheit in der Bestimmung der Geraden', align = "left")),
      
      tags$hr(),
           
      # Begleittext
      p("Lineare Regression in der einfachsten Form beschreibt den Zusammenhang zwischen 
      zwei Variablen x und y als eine Gerade, welche die Datenpunkte so gut wie möglich 
      beschreibt. Die Koeffizienten a und b in der Geradengleichung", em("y = a x + b"), 
        "werden so bestimmt, dass die Summe der quadratischen Abstände (Residuen) zwischen den Punkten 
      und der Geraden in y-Richtung minimiert werden."),
      
      p("In diesem Beispiel betrachten wir die Abhängigkeit des Mietpreises y von Wohnungen 
        von deren Wohnfläche x auf der Basis von Daten."),
                  
      br(),  
      
      # Latex-Implementierung
      uiOutput('ex1'),
      
      p("Verändere die Größe der Stichprobe und ziehe mehrfach Stichproben derselben Größe. 
        Betrachte das graue Band um die Regressionsgerade, welches die Unsicherheit über 
        die Lage der Regressionsgerade zeigt. Wo kommt diese Unsicherheit her und wie 
        hängt sie mit der Stichprobe zusammen?"),
      
      tags$hr(),
            
      sliderInput("size", 
                  "Size", 
                  min = 10, 
                  max = 2000, 
                  step = 5, 
                  value = 200 , 
                  width = '100%'), 
      
      br(),
      
      actionButton("sample",
                   "Stichprobe ziehen ",
                   width = '100%'),
      br()
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(  
    
      # Sample_Plot
      plotOutput("distplot_sample"),
      
      br()
    )
    ),
  
  # position = c("left", "right"),
  
  fluid = TRUE
)

# server logic 
server <- function(input, output) {
  
  # LaTex
  output$ex1 <- renderUI({
                        withMathJax(
                                    helpText('$$Nettomiete = a \\cdot \\textit{Wohnfläche} + b$$')
                                    )
                        })

  # Sample_Plot
  myseed <- round(runif(1, 1, 10000))
  plot1 <- output$distplot_sample <- renderPlot({
    
            set.seed(myseed)
            
            samp              <- mietspiegel[sample(1:nrow(mietspiegel),
                                                    input$size,
                                                    replace = TRUE),]
            
            p                 <-  ggplot(samp, aes(y = nm, x = wfl))+
              xlim(0, 190) +
              ylim(0, 1750) +
              
              # Plot the actual points;
              geom_point(cex = 1, color = "#1F1717", shape = 19) +
              # Regression Line
              geom_smooth(method= lm, level = 0.9999, se = TRUE, color = "#0000FF") +
              # Regression Line
              theme(panel.background = element_rect(fill = "#D6D6D6"),
                    panel.grid.major = element_line(colour = "#000000",
                                                    linetype = "blank", 
                                                    # "blank",
                                                    # "solid", 
                                                    # "dashed", 
                                                    # "dotted", 
                                                    # "dotdash", 
                                                    # "longdash", 
                                                    # "twodash"
                                                    size = 0.1),
                    axis.text        = element_text(colour = "black",
                                                    size   = rel(1.2)
                    ),
                    axis.title       =element_text(size=14,face="bold"),
                    axis.line        = element_line(arrow = arrow(angle  = 12,
                                                                  length = unit(0.22, "inches"),
                                                                  ends   = "last",  # "last", "first", or "both"
                                                                  type   = "closed" # "open" or "closed"
                    )
                    )
              ) +
              xlab("Wohnfläche") +
              ylab("Nettomiete")
            
            p
            
          })
  
  plot1
  
  
  observeEvent(input$sample, {

    myseed <- round(runif(1, 1, 10000))

    # Sample_Plot
    plotnext <- output$distplot_sample <- renderPlot({
      
                set.seed(myseed)
                
                samp              <- mietspiegel[sample(1:nrow(mietspiegel),
                                                        input$size,
                                                        replace = TRUE),]
                
                p                 <-  ggplot(samp, aes(y = nm, x = wfl))+
                  xlim(0, 190) +
                  ylim(0, 1750) +
                  
                  # Plot the actual points;
                  geom_point(cex = 1, color = "#1F1717", shape = 19) +
                  # Regression Line
                  geom_smooth(method= lm, level = 0.9999, se = TRUE, color = "#0000FF") +
                  # Regression Line
                  theme(panel.background = element_rect(fill = "#D6D6D6"),
                        panel.grid.major = element_line(colour = "#000000",
                                                        linetype = "blank", 
                                                        # "blank",
                                                        # "solid", 
                                                        # "dashed", 
                                                        # "dotted", 
                                                        # "dotdash", 
                                                        # "longdash", 
                                                        # "twodash"
                                                        size = 0.1),
                        axis.text        = element_text(colour = "black",
                                                        size   = rel(1.2)
                        ),
                        axis.title       =element_text(size=14,face="bold"),
                        axis.line        = element_line(arrow = arrow(angle  = 12,
                                                                      length = unit(0.22, "inches"),
                                                                      ends   = "last",  # "last", "first", or "both"
                                                                      type   = "closed" # "open" or "closed"
                        )
                        )
                  ) +
                  xlab("Wohnfläche") +
                  ylab("Nettomiete")
                
                p
                
              })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

