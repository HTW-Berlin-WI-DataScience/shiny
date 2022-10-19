library(ggplot2)
      
ui <- fluidPage(
  
            # Titel
            h2("Approximation der Würfelwahrscheinlichkeit", align = "left"),
            
            br(),
            br(),
            
            # Begleittext
            p("Diese interaktive Visualisierung zeigt, wie Wahrscheinlichkeiten bei wiederholter 
              Ausführung eines Zufallsexperiments konvergieren."),
            
            p("Die Taste", em("Würfeln"), "triggert das tausendmalige Werfen eines Würfels. 
              Nach jedem Wurf wird die relative Anzahl der geworfenen Sechsen neu berechnet 
              und im Graphen aufgetragen."),
            
            p("Wir können sehen, wie sich die relative Anzahl der geworfenen Sechsen 
              der erwarteten Wahrscheinlichkeit von 1/6 -- der grauen gestrichelten Linie -- annähert."),
            
            p("Mit der Taste ", em("Neustart"), " werden alle Läufe gelöscht, um eine neue Folge von 
              Experimenten durchzuführen."),
            
            
            br(),
            
            # Plot
            plotOutput("plot"),
            
            br(),
            br(),
            
            fluidRow(
              column(6,
                     # Wuerfel - und Neustartbutton
                     actionButton("reroll", 
                                  "Wuerfeln", 
                                  width = "100%")
              ),
              
              column(6,
                     actionButton("restart", 
                                  "Neustart", 
                                  width = "100%")
              )
            )
      )
    

server <- function(input, output) {
  
  
  # Plot starten
  p_blank <-  ggplot() +
              geom_hline(yintercept=(1/6), linetype="dashed", color = "black") +
              xlim(0, 1000) +
              ylim(0, 1) +
              labs(x="Anzahl der Würfe", 
                   y="Wahrscheinlichkeit einer 6") +
              theme_grey() +
              theme(legend.position  = c(.8, .8), # "bottomright", "bottom", "bottomleft", "left",
                                                  # "topleft", "top", "topright", "right" and "center"
                    legend.direction ="horizontal",
                    legend.title     =element_blank(),

                    legend.justification = c("right", "top"),
                    legend.box.just      = "right",
                    legend.margin        = margin(6, 6, 6, 6),
                    axis.text            = element_text(colour = "black", 
                                                        size   = rel(1.2)),
                    axis.title=element_text(size=14,face="bold"),
                    axis.line = element_line(arrow = arrow(angle  = 12, 
                                                           length = unit(0.22, "inches"),
                                                           ends   = "last",  # "last", "first", or "both" 
                                                           type   = "closed" # "open" or "closed"
                                                          ))
              )
  
  #reactive values
  reac <- reactiveValues()
  reac$p_lines <- p_blank
  reac$counter <- 0
  
  # button functionality
  observeEvent(input$reroll,
               {
                 input$reroll
                 a <- 1:6
                 b <- 1:1000
                 eyes <- sample(a,1000,replace=T)
                 six  <- eyes == 6
                 c    <- cumsum(six) / 1:1000
                 df   <- data.frame(b, c, counter = reac$counter)
                 gl   <- geom_line(aes(x = b , y = c, col = factor(counter)), 
                                 df, 
                                 size = 0.5,  
                                 linetype= "solid", # "blank", "solid", "dashed", "dotted", 
                                                    # "dotdash", "longdash", and "twodash" 
                                 alpha = 1.0)
                            
                 reac$p_lines <- reac$p_lines + gl
                 reac$counter <- reac$counter + 1
               })
  
  observeEvent(input$restart,
               {
                 reac$p_lines <- p_blank
                 reac$counter <- 0
               })
  
  # Plot zeichnen
  output$plot <- renderPlot(reac$p_lines)
}

shinyApp(ui=ui, server = server)