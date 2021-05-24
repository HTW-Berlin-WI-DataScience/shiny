
library(shiny)
library(ggplot2)
library(fields)

# for consistency of seeds with older versions of R
RNGkind(sample.kind = "Rounding")
set.seed(1)

# ziehe 300 Wohnungen aus den Mietspiegeldaten
mietspiegel <- read.table("./miete03.asc", header=TRUE)
indices <- sample(1:nrow(mietspiegel), 300)
mietspiegel <- mietspiegel[indices,]

reg_line <- function(b, a, x) {  a * x + b }

mean_sqr_diff <- function(a, b, x, y) {sqrt(mean((a * x + b - y)^2))} 
mean_abs_diff <- function(a, b, x, y) {mean(abs(a * x + b - y))}

# lineare Regression (Nettomiete ueber Wohnfl?che) "linear model"
mieten_regression <- lm(mietspiegel$nm ~ mietspiegel$wfl)

# wie stark aendert sich der Fehler mit Variation der Parameter a und b in der lineare Regression?
b <- mieten_regression$coefficients[1]      # Coefficient No. 1   Intercept
a <- mieten_regression$coefficients[2]      # Coefficient No. 2   mietspiegel$wfl

# UI logic
ui <- fluidPage(
  
  # pre(includeMarkdown("test.md")),
  
  title = 'Lineare Regression: Koeffizienten und Fehler',
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    position = "left",  
    #position = "right",
    
    sidebarPanel(
      
      # Application title
      titlePanel(title = h3('Lineare Regression: Einfluss der Koeffizienten auf den Fehler', align = "left")),
      
      tags$hr(),
      
      # Begleittext
      p("Lineare Regression in der einfachsten Form beschreibt den Zusammenhang zwischen 
      zwei Variablen x und y als eine Gerade, welche die Datenpunkte so gut wie möglich 
      beschreibt. Die Koeffizienten a und b in der Geradengleichung", em("y = a x + b"), 
      "werden so bestimmt, dass die Summe der quadratischen Abstände (Residuen) zwischen den Punkten 
      und der Geraden in y-Richtung minimiert werden."),
      
      p("In diesem Beispiel betrachten wir die Abhängigkeit des Mietpreises y von Wohnungen 
        von deren Wohnfläche x auf der Basis von 300 Datenpunkten:"),
      
      uiOutput('ex1'),
      
      p("Mit den Schieberegeln können von Hand die optimalen Werte von a und b verstellt werden. 
        Die Veränderungen werden durch die rote Gerade im Streudiagramm visualisiert. 
        Weiterhin zeigen die roten Punkte in den Fehlerfunktionen, wie stark sich der Fehler 
        verändert."), 
                          
      sliderInput("a", 
                  "Veränderung der Geradensteigung delta a", 
                  min = -1, 
                  max = 1, 
                  step = 0.1, 
                  value = 0 , 
                  width = '100%'),          
      
      sliderInput("b", 
                  "Veränderung des Achsenabschnitts delta b", 
                  min = -50,  
                  max = 50, 
                  step = 1, 
                  value = 0 , 
                  width = '100%'),
                  
      actionButton("residuen_gg", 
                   "Zeige Residuen", 
                   width = '100%'),
      
      br(),
      
      tags$hr(),
      
      textOutput('mad'),
      
      br(),
      
      textOutput('rmse'),           
      
      br()#,
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(                    
      
      p(strong("Streudiagramm des Mietpreises über der Wohnfläche mit Regressionsgerade"), align = "center"), 
      
      # Normalplot
      plotOutput("distPlot"),
      
      br(),       

      p(strong("Fehlerfunktionen über delta a und delta b"), align = "center"), 
      
      # 4 Delta-Plots
      plotOutput("distPlot3"),
      
      br(),
      
      # 3D-Plot Wurzel des mittleren quadratischen Fehlers 
      plotOutput("distPlot4"),
      br(),
      fixedRow(
        column(12, "Wurzel des mittleren quadratischen Fehlers", 
               fixedRow(
                 column(6,sliderInput("theta1", HTML("&theta; - Drehung Querachse"), 0, 360, 216, step = 18, animate = TRUE, width = '100%' )),
                 column(6,sliderInput("phi1", HTML("&phi; - Drehung Längsachse"), 0, 360, 18, step = 18, animate = TRUE, width = '100%' ))
               )
        )
      )
      ,
      
      br(),
      
      # 3D-Plot Mittlerer absoluter Fehler 
      plotOutput("distPlot5"),
      
      fixedRow(
        column(12, "Mittlerer absoluter Fehler",
               fixedRow(
                 column(6,sliderInput("theta2", HTML("&theta; - Drehung Querachse"), 0, 360, 216, step = 18, animate = TRUE, width = '100%' )),
                 column(6,sliderInput("phi2", HTML("&phi; - Drehung Längsachse"), 0, 360, 18, step = 18, animate = TRUE, width = '100%' ))
               )
        )
      )
      
    )
    ),
  
  # position = c("left", "right"),
  
  fluid = TRUE
)

# server logic 
server <- function(input, output) {
  
  # LaTex
  output$ex1 <- renderUI({    
        withMathJax('$$Mietpreis = a \\cdot \\textit{Wohnfläche} + b$$'
    )    
  })
  
  # Normalplot
  output$distPlot <- renderPlot({
    
    p_orig <- ggplot(mietspiegel, aes(y = nm, x = wfl) ) +
      xlim(0, 190) + 
      ylim(0, 1750) +
      
      # Add linear regression line
      geom_abline(intercept = b+input$b, slope = a+input$a, colour = "#DB5555", size = 1) +  
      
      # Plot the actual points;
      geom_point(cex = 1, color = "#1F1717", shape = 19) +  
      
      geom_smooth(method= lm, level = 0.9999, se = TRUE, color = "#0000FF")+ 
      
      #theme_bw()+  # Add theme for cleaner look
      theme(panel.background = element_rect(fill = "#e6e6e6"),
            panel.grid.major = element_line(colour = "#000000", 
                                            linetype = "blank", # "blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"
                                            size = 0.1),
            axis.text        = element_text(colour = "black",
                                            size   = rel(1.2)),
            axis.title       =element_text(size=14,face="bold"),
            axis.line        = element_line(arrow = arrow(angle  = 12,
                                                          length = unit(0.22, "inches"),
                                                          ends   = "last",  # "last", "first", or "both" 
                                                          type   = "closed" # "open" or "closed"
            )
            )
      ) +                   
      xlab("Wohnfläche") +               
      ylab("Mietpreis") 
    
    # Residuals on / off
    if ((input$residuen_gg) %% 2 == 0) {
      return(p_orig)
      
    } else {
      df <- data.frame(x1 = mietspiegel$wfl, x2 = mietspiegel$wfl, y1 = mietspiegel$nm, y2 = reg_line((b-input$b), (a-input$a), mietspiegel$wfl))
      
      p_orig <- p_orig + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), size = 0.9, linetype="dashed", colour = "red", alpha = 0.5, data = df) 
      
      return(p_orig)
    }
    
  })
  
  
  
  # Mittlerer absoluter Fehler 
  output$mad <- renderPrint({
    mad <- mean_abs_diff(a+input$a, b+input$b, mietspiegel$wfl, mietspiegel$nm)
    cat(" Mittlerer absoluter Fehler:", round(mad))          
  })
  
  # Mittlerer absoluter Fehler 
  output$rmse <- renderPrint({
    rmse <- mean_sqr_diff(a+input$a, b+input$b, mietspiegel$wfl, mietspiegel$nm)
    cat(" Wurzel mittl. quadr. Fehler:", round(rmse))          
  })       
  
  
  # Delta-Plots
  output$distPlot3 <- renderPlot({ 
    
    rmse <- mean_sqr_diff(a+input$a, b+input$b, mietspiegel$wfl, mietspiegel$nm)
    rmse1 <- mean_sqr_diff(a, b+input$b, mietspiegel$wfl, mietspiegel$nm) # optimum
    rmse2 <- mean_sqr_diff(a+input$a, b, mietspiegel$wfl, mietspiegel$nm) # optimum
    
    mad <- mean_abs_diff(a+input$a, b+input$b, mietspiegel$wfl, mietspiegel$nm)
    mad1 <- mean_abs_diff(a, b+input$b, mietspiegel$wfl, mietspiegel$nm) # optimum
    mad2 <- mean_abs_diff(a+input$a, b, mietspiegel$wfl, mietspiegel$nm) # optimum
    
    
    par(mfrow = c(2,2))
    
    
    x <- seq(-50, 50, 1)
    
    plot(x, main = "delta b : Wurzel mittl. quadr. Fehler", sapply(x, function(y) mean_sqr_diff(a+input$a, b + y, mietspiegel$wfl, mietspiegel$nm)), 
         xlab = "delta b", ylab = "Wurzel mittl. quadr. Fehler", type = "l")
    points(x = input$b, y = rmse, pch = 19, col = "red", cex = 2) +
    points(x = 0, y = rmse2, pch = 16, col = "blue", cex = 2) 
    
    
    
    x <- seq(-1, 1, 0.1)
    
    plot(x, main = "delta a : Wurzel mittl. quadr. Fehler",sapply(x, function(y) mean_sqr_diff(a + y, b+input$b, mietspiegel$wfl, mietspiegel$nm)), 
         xlab = "delta a", ylab = "Wurzel mittl. quadr. Fehler", type = "l")
    
    points(x = input$a, y = rmse, pch = 19, col = "red", cex = 2)
    points(x = 0, y = rmse1, pch = 16, col = "blue", cex = 2) 
    
    
    
    
    
    
    x <- seq(-50, 50, 1)
    
    plot(x, main = "delta b : mittlerer absoluter Fehler", sapply(x, function(y) mean_abs_diff(a+input$a, b + y, mietspiegel$wfl, mietspiegel$nm)), 
         xlab = "delta b", ylab = "mittlerer absoluter Fehler", type = "l")
    
    points(x = input$b, y = mad, pch = 19, col = "red",cex = 2)
    points(x = 0, y = mad2, pch = 16, col = "blue", cex = 2) 
    
    
    
    x <- seq(-1, 1, 0.1)
    
    plot(x, main = "delta a : mittlerer absoluter Fehler", sapply(x, function(y) mean_abs_diff(a + y, b+input$b,mietspiegel$wfl, mietspiegel$nm)),
         xlab = "delta a", ylab = "mittlerer absoluter Fehler", type = "l")
    points(x = input$a, y = mad, pch = 19, col = "red", cex = 2)
    points(x = 0, y = mad1, pch = 16, col = "blue", cex = 2) 
    
    par(mfrow = c(1,1))
  })
  
  # 3D-Plot Wurzel des mittleren quadratischen Fehlers 
  output$distPlot4 <- renderPlot({ 
    
    mean_sqr_diff2 <- function(a,b) {sqrt(mean((a * mietspiegel$wfl + b - mietspiegel$nm)^2))} 
    
    par(bg = "#ffffff")
    
    x <- seq(-1, 1, 0.1)
    y <- seq(-50, 50, 1)
    z <- outer(x, y, FUN = function(x, y) mapply(mean_sqr_diff2, a = a + x, b = b + y))
    
    rmse <- mean_sqr_diff2(a+input$a, b+input$b)
    rmse1 <- mean_sqr_diff2(a, b)
    
    nrz <- nrow(z)
    ncz <- ncol(z)
    # Create a function interpolating colors in the range of specified colors
    jet.colors <- colorRampPalette( c("#d9d9d9", "#000000") )
    # Generate the desired number of colors from this palette
    nbcol <- 100
    color <- jet.colors(nbcol)
    # Compute the z-value at the facet centres
    zfacet <- (z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz])/4    
    # Recode facet z-values into color indices
    facetcol <- cut(zfacet, nbcol)
    
    res <- persp(x, y, z,
                 r = 10,                 
                 main = "Wurzel mittlerer quadratischer Fehler über delta a und delta b",
                 col = color[facetcol],   
                 theta = input$theta1,
                 phi = input$phi1,
                 axes = T,
                 shade = 0.2,
                 box = T,
                 expand = 0.6,
                 scale = T,
                 border = NA,
                 bg = "black",
                 xlab = "delta a",
                 ylab = "delta b",
                 zlab = "Wurzel mittlerer quadratischer Fehler",
                 ticktype = "detailed" ) # "detailed", "simple"
        
    # add color bar
    image.plot(legend.only=T, zlim=range(zfacet), col=color)
    
    mypoints <- trans3d(input$a, input$b, rmse, pmat = res)
    mypoints1 <- trans3d(0, 0, rmse1, pmat = res)
    points(mypoints, pch = 16, col = "red", cex = 1.5)
    points(mypoints1, pch = 16, col = "blue", cex = 1.5)
    
  })
  
  # 3D-Plot Mittlerer absoluter Fehler 
  output$distPlot5 <- renderPlot({ 
    
    mean_abs_diff2 <- function(a,b) {mean(abs(a * mietspiegel$wfl + b - mietspiegel$nm))}    
    
    par(bg = "#ffffff")
    
    x <- seq(-1, 1, 0.1)
    y <- seq(-50, 50, 1)
    z <- outer(x, y, FUN = function(x, y) mapply(mean_abs_diff2, a = a + x, b = b + y))
    
    
    mad <- mean_abs_diff2(a+input$a, b+input$b)
    mad1 <- mean_abs_diff2(a, b)
    
    nrz <- nrow(z)
    ncz <- ncol(z)
    # Create a function interpolating colors in the range of specified colors
    jet.colors <- colorRampPalette( c("#d9d9d9", "#000000") )
    # Generate the desired number of colors from this palette
    nbcol <- 100
    color <- jet.colors(nbcol)
    # Compute the z-value at the facet centres
    zfacet <- (z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz])/4    
    # Recode facet z-values into color indices
    facetcol <- cut(zfacet, nbcol)
    
    res <- persp(x, y, z,
                 r = 10,
                 main = "Mittlerer absoluter Fehler über delta a und delta b",
                 col = color[facetcol],   
                 theta = input$theta2,
                 phi = input$phi2,
                 xlab = "delta a",
                 ylab = "delta b",
                 zlab = "mittlerer absoluter Fehler",
                 axes = T,
                 shade = 0.2,
                 box = T,
                 expand = 0.6,
                 scale = T,
                 border = NA,
                 ticktype = "detailed" # "detailed" , "simple"
    )
    
    ## add color bar
    image.plot(legend.only=T, zlim=range(zfacet), col=color)
    
    mypoints <- trans3d(input$a, input$b, mad, pmat = res)
    mypoints1 <- trans3d(0, 0, mad1, pmat = res)
    points(mypoints, pch = 16, col = "red", cex = 1.5)
    points(mypoints1, pch = 16, col = "blue", cex = 1.5)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

