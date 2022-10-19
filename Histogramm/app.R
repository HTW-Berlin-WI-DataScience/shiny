library(ggplot2)
library(lattice)
library(plyr)
library(DT)

ui <- fluidPage(
  
            # Titel
            h2("Explore the Effects of Binning in Histograms", align = "left"),
            
            hr(),
            
            # Begleittext
            p("Changing the number and position of bins 
              can considerably change the appearance 
              and interpretation of value distributions in histograms. 
              The data are your estimates of my height collected in class."),
            
            p("You can play around with the two sliders to change the width and position of bins. Observe, 
              how the following properties change:"),
            
            tags$ul(
              tags$li("the symmetry of the histogram"),
              tags$li("the location of the highest bar ", em("(mode)")),
              tags$li("the relative location of the highest bar and the mean and median values")
            ),
            
            p("Explain the changes by taking into account the location and density of data points."),
            
            hr(),
            
            # Plot
            plotOutput("plot"),
            
            br(),
            hr(),
            
            fluidRow(
              column(6,
                     sliderInput("shift", "Shift of bins", min = 0, max = 10, step = 1, value = 0)
              ),
              
              column(6,
                     sliderInput("binwidth", "bin width in cm", min = 1, max = 10, step = 1, value=3)
              )
            ),
            
            # Tabelle mit Häufigkeiten plotten
            
            br(),
            dataTableOutput('mytable'),
            br()
      )
    

server <- function(input, output) {
  
  groesse2    <- c(173, 175, 175, 177, 178, 175, 176, 175, 175, 
                   178, 179, 178, 176, 177, 178, 176, 175, 184, 
                   186, 180, 182, 170, 180, 181, 183, 187)
  
  groesse1    <- c(176, 178, 183, 180, 186, 178, 175)
  
  zug         <- c(rep(2, length(groesse2)), rep(1, length(groesse1)))
  
  groesse_df  <- data.frame(groesse=c(groesse2, groesse1), zug=zug)
  
  
  # Tabelle des Datensatzes mit Sortierfunktion
  output$mytable = renderDataTable({
    count(groesse_df)
  }, options = list(orderClasses = TRUE, lengthMenu = c(5, 10, 20, 50), pageLength = 20,
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}")))
  
  # Plot zeichnen
  output$plot <- renderPlot({
    # verhindert "Springen der Punkte"
    set.seed(10)
    
    bins <- seq(165 + input$shift, 192 + input$shift, by = input$binwidth)
    
    p1 <- ggplot(data = groesse_df, aes(x=groesse))                                     +
          xlim(160, 200)                                                                +
          ylim(-0.02, 0.2)                                                              +
      
          geom_histogram(aes(y=..density..
                             # , 
                             # fill=..count..
                             ), 
                         breaks=bins, col = "white")                                    +
          # scale_fill_gradient("number of heights", low = "darkblue", high = "lightblue")         +
          geom_density(alpha=.1, fill="blue", colour= "white")                          +
      
          geom_jitter(data = groesse_df,aes(x=groesse,
                                            y=-0.01,
                                            colour = "point"),
                                            width=0,
                                            height=0.01)                                +
          #mean
          geom_vline(aes(xintercept=mean(groesse, na.rm=T), colour = "mean"),       
                           linetype="dashed", size=1)                                   +
                            # blank, solid, dashed, dotted, dotdash, longdash, twodash
          #median
          geom_vline(aes(xintercept=median(groesse, na.rm=T), colour = "median"),   
                           linetype="longdash", size=1)                                 +
                            # blank, solid, dashed, dotted, dotdash, longdash, twodash
          scale_x_continuous(limits=c(165,195))                                         +
          scale_colour_manual("legend", values = c("mean" = "blue", 
                                                   "median" = "red",
                                                   "point" = "black"))                  +
          labs(x="estimated height of Prof Spott", y="density")                         +
          theme_grey()                                                                  +
          theme(axis.text  = element_text(colour = "black", size     = rel(1.2)),
                      axis.title = element_text(size=14,face="plain"), 
                                                        # "plain", "italic", "bold", "bold.italic"
                      axis.line  = element_line(arrow = arrow(angle  = 12,
                                                              length = unit(0.22, "inches"),
                                                              ends   = "last",  # "last", "first", or "both"
                                                              type   = "closed" # "open" or "closed"
                      ))
                )
    
    p1
    
  })
}

shinyApp(ui=ui, server = server)