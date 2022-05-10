library(waffle)
library(DiagrammeR)

server <- function(input, output) {
  selectedTest <- reactive({
    req(input$testKind)
    if(input$testKind == "Covid-19") 
      result <- c(
        c(0.2,96.52,99.68), #c(Vortestwahrscheinlichkeit, Sensitivität, Spezifität)
        '*https://www.roche.de/patienten-betroffene/informationen-zu-krankheiten/covid-19/'
      )
    if(input$testKind == "HIV") 
      result <- c(
        c(4,99.1,99.8), #c(Vortestwahrscheinlichkeit, Sensitivität, Spezifität)
        '*https://www.autotest-sante.com/de/autotest-VIH-par-AAZ-139.html#meancre6'
      )
    if(input$testKind == "Schwangerschaft") 
      result <- c(
        c(20,99.9,99.9), #c(Vortestwahrscheinlichkeit, Sensitivität, Spezifität)
        '*https://www.moelab.de/hCG.html'
      )
    return(result)
  })
  
  output$relativePositives <- renderUI({
    numericInput(inputId = "relativeTruePositives",
                 label = "Wie viele von 10.000 Getesteten sind tatsächlich positiv (in %)?",
                 value = selectedTest()[1], min = 0, max = 100, step = 0.01)
  })
  
  output$sensitivitaetOutput <- renderUI({
      numericInput(inputId = "sensitivitaet",
                   label = "Sensitivität* (in %):",
                   value = selectedTest()[2], min = 0, max = 100, step = 0.01)
  })
  
  output$spezifitaetOutput <- renderUI({
    numericInput(inputId = "spezifitaet",
                 label = "Spezifität* (in %):",
                 value = selectedTest()[3], min = 0, max = 100, step = 0.01)
  })
  
  data <- reactive({
    req(input$sensitivitaet, input$spezifitaet, input$relativeTruePositives)
    data.frame(check.names = FALSE, " " = c("1", "2", "3", "4"), 
               "Status" = c("positiv", "positiv", "negativ", "negativ"), 
               "Test" = c("richtig positiv", "falsch negativ", "richtig negativ", "falsch positiv"), 
               "Anzahl von 10.000 Getesteten" = as.integer(round(c(input$relativeTruePositives/100 * input$sensitivitaet/100 * 10000, 
                                                                   input$relativeTruePositives/100 * (1-input$sensitivitaet/100) * 10000, 
                                                                   (1-input$relativeTruePositives/100) * input$spezifitaet/100 * 10000, 
                                                                   (1-input$relativeTruePositives/100) * (1-input$spezifitaet/100) * 10000), digits = 0)))
  })
  
  
  dataKennzahlen <- reactive({
    req(input$sensitivitaet, input$spezifitaet, input$relativeTruePositives)
    data.frame(check.names = FALSE, " " = c("1", "2"), 
               "Kennzahl" = c("positiver Vorhersagewert", "negativer Vorhersagewert"), 
               "Wert (in %)" = c((input$sensitivitaet/100 * input$relativeTruePositives/100 / (input$sensitivitaet/100 * input$relativeTruePositives/100 + (1-input$spezifitaet/100) * (1-input$relativeTruePositives/100))*100), 
                                 ((1-input$relativeTruePositives/100) * input$spezifitaet/100) / ((1-input$relativeTruePositives/100) * input$spezifitaet/100 + input$relativeTruePositives/100 * (1-input$sensitivitaet/100)) *100))
  })
  
  dataWaffleplot <- reactive({
    req(input$sensitivitaet, input$spezifitaet, input$relativeTruePositives, input$testKind)
    c('richtig positiv' = as.integer(round(input$relativeTruePositives/100 * input$sensitivitaet/100 * 10000), digits = 0),
      'falsch negativ' = as.integer(round(input$relativeTruePositives/100 * (1-input$sensitivitaet/100) * 10000), digits = 0),
      'falsch positiv' = as.integer(round((1-input$relativeTruePositives/100) * (1-input$spezifitaet/100) * 10000), digits = 0),
      'richtig negativ' = as.integer(round((1-input$relativeTruePositives/100) * input$spezifitaet/100 * 10000), digits = 0))
      
  })
  
  
  output$tabelle <- renderTable({
    head(data())
  })
  
  output$tabelleKennzahlen <- renderTable({
    head(dataKennzahlen())
  })
  
  output$waffelplot = renderPlot({
    req(input$sensitivitaet, input$spezifitaet, input$relativeTruePositives)
    waffle( parts = dataWaffleplot(), rows = 100, size = 0.5,
            colors = c('#FC4E2A', '#FEB24C', '#456DCC', '#8DA0CB'),
            flip = TRUE , reverse = TRUE,
            xlab = '1 Quadrat = 1 getestete Person',
            legend_pos = "top"
    )
  }, height = 1000, width = 1000)
  

  output$baumdiagramm <- renderGrViz({
    sensitivitaet <- input$sensitivitaet/100
    spezifitaet <- input$spezifitaet/100
    
    relativeTruePositives <- input$relativeTruePositives/100 * 10000
    relativeTrueNegatives <- 10000-relativeTruePositives
    
    rN <- (1-input$relativeTruePositives/100) * input$spezifitaet/100 * 10000
    fN <- (1-input$relativeTruePositives/100) * (1-input$spezifitaet/100) * 10000
    rP <- (input$relativeTruePositives/100 * input$sensitivitaet/100 * 10000)
    fP <- (input$relativeTruePositives/100 * (1-input$sensitivitaet/100) * 10000)
    
    grViz(sprintf("digraph probTree {
                    'Tests: 10.000' -> negativ [label = %.f];
                    'Tests: 10.000' -> positiv [label = %.f];
                    negativ -> 'falsch negativ' [label = %.f];
                    negativ -> 'richtig negativ' [label = %.f];
                    positiv -> 'richtig positiv' [label = %.f];
                    positiv -> 'falsch positiv' [label = %.f];
                  }", relativeTrueNegatives, relativeTruePositives, fN, rN, rP, fP))
  })
  
  output$quelleTestDaten = renderText(selectedTest()[4])
}
