borrelTest <- list('name' = 'Care Plus Tick Test Borrelia Bacteria Indicator',
                   'testPos' = 41,
                   'testNeg' = 118,
                   'pos' = 45,
                   'neg' = 121)

darmTest <- list('name' = 'Veroval Darm- Vorsorge',
                 'testPos' = 68,
                 'testNeg' = 49,
                 'pos' = 70,
                 'neg' = 50)

eisenmangelTest <- list('name' = 'Veroval Eisenmangel',
                        'testPos' = 40,
                        'testNeg' = 63,
                        'pos' = 41,
                        'neg' = 65)

prostataTest <- list('name' = 'All Test PSA Prostata',
                     'testPos' = 205,
                     'testNeg' = 351,
                     'pos' = 207,
                     'neg' = 354)

schilddrüsenTest <- list('name' = 'ZuhauseTest Schilddrüsen',
                         'testPos' = 27,
                         'testNeg' = 40,
                         'pos' = 28,
                         'neg' = 42)

coronaTest1 <- list('name' = 'Clungene Covid-19',
                    'testPos' = 132,
                    'testNeg' = 462,
                    'pos' = 136,
                    'neg' = 465)

coronaTest2 <- list('name' = 'Hughes Covid-19',
                    'testPos' = 165,
                    'testNeg' = 433,
                    'pos' = 170,
                    'neg' = 435)

coronaTest3 <- list('name' = 'Hygisun Covid-19',
                    'testPos' = 217,
                    'testNeg' = 123,
                    'pos' = 221,
                    'neg' = 123)






library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("n",
                  "Größe der Studie:",
                  min = 1,
                  max = 50,
                  value = 30),
      sliderInput("pos",
                  "Positive Fälle:",
                  min = 1,
                  max = 50,
                  value = 30),
      sliderInput("sens",
                  "Sensitivität:",
                  min = 1,
                  max = 50,
                  value = 30),
      sliderInput("spez",
                  "Spezifität:",
                  min = 1,
                  max = 50,
                  value = 30),
      sliderInput("konf",
                  "Konfidenzniveau:",
                  min = 0,
                  max = 1,
                  value = 0.30,
                  step = 0.1),
      selectInput("data",
                  "Select Data",
                  choices = c(borrelTest$name,
                              darmTest$name,
                              eisenmangelTest$name,
                              prostataTest$name,
                              schilddrüsenTest$name,
                              coronaTest1$name,
                              coronaTest2$name,
                              coronaTest3$name
                  ),
                  selected = borrelTest$name)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("table1"),
      textOutput("text1")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  
  #trigger for multible events
  triggerChanges <- reactive({
    paste(input$pos, input$sens, input$spez, input$n)
  })
  
  # #1
  # testTruePos <- eventReactive(triggerChanges(),{
  #   input$pos * (input$sens/100)
  # })
  # 
  # #2
  #  testNegTruePos <- eventReactive(triggerChanges(),{
  #    input$pos - testTruePos()
  #  })
  # 
  #  #3
  #  sumNeg <- eventReactive(triggerChanges(),{
  #    input$n - input$pos
  #  })
  # 
  #  #4
  #  testTrueNeg <- eventReactive(triggerChanges(),{
  #    sumNeg() * (input$spez/100)
  #  })
  #  
  #  #5
  #  testPosTrueNeg <- eventReactive(triggerChanges(),{
  #    sumNeg() - testTrueNeg()
  #  })
  #  
  #  #6
  # summeTP <- eventReactive(triggerChanges(),{
  #   round(testTruePos()) + round(testPosTrueNeg())
  # })
  # 
  # #7
  # summeTN <- eventReactive(triggerChanges(),{
  #   round(testNegTruePos()) + round(testTrueNeg())
  # })
  # 
  # #8
  # summeAller <- eventReactive(triggerChanges(),{
  #   summeTN() + summeTP()
  # })
  
  testTruePos <- reactiveValues()
  testNegTruePos <- reactiveValues()
  sumNeg <- reactiveValues()
  testTrueNeg <- reactiveValues()
  testPosTrueNeg <- reactiveValues()
  summeTP <- reactiveValues()
  summeTN <- reactiveValues()
  summeAller <- reactiveValues()
  
  observeEvent(triggerChanges(),{
    testTruePos$calc <- input$pos * (input$sens/100)
    testNegTruePos$calc <- input$pos - testTruePos$calc
    sumNeg$calc <- input$n - input$pos
    testTrueNeg$calc <- sumNeg$calc * (input$spez/100)
    testPosTrueNeg$calc <- sumNeg$calc - testTrueNeg$calc
    summeTP$calc <- round(testTruePos$calc) + round(testPosTrueNeg$calc)
    summeTN$calc <- round(testNegTruePos$calc) + round(testTrueNeg$calc)
    summeAller$calc <- summeTN$calc + summeTP$calc
    
  })
  
  output$table1 <- renderTable({
    df <- structure(list(Tabelle= c("Test positiv", "Test negativ", "Summe"),
                         positiv =c(round(testTruePos$calc), round(testNegTruePos$calc), input$pos),
                         negativ = c(round(testPosTrueNeg$calc), round(testTrueNeg$calc), sumNeg$calc),
                         Summe = c(summeTP$calc, summeTN$calc, summeAller$calc)), .Names = c(" ","True positiv", "True negativ", "Summe"),
                    row.names = c(NA,3L), class = "data.frame")
  })
  
  myval <- reactiveValues()
  
  observeEvent(c(input$pos, input$konf),{
    x <- testTruePos$calc
    calci <- binom.test(round(testTruePos$calc), 90, conf.level = input$konf)$conf.int
    
    myval$calc <- paste("Sensivität: [", round(calci[1], digits = 2), "], [" , round(calci[2], digits = 2), "]", round(x), input$pos)
  })
  
  #Darstellung druch binomtest
  
  
  output$text1 <- renderText({
    myval$calc
  })
  
  
  
  
  # as <- reactive({
  #   if(input$data == darmTest$name){
  #     input$pos
  #   }
  #   else if (input$data == borrelTest$name){
  #     borrelTest$pos
  #   }
  #   else{
  #     input$pos
  #   }
  # })
  
  
  # Überarbeiten!! wenn input$pos weg, dann kann größe der Studie verändert werden
  observeEvent(c(input$data, input$pos),{
    
    range = c(0,100)
    rangePos = c(0,100)
    rangeSens = c(0,100)
    rangeSpez = c(0,100)
    pos = 0
    
    
    #DarmTest
    if(input$data == darmTest$name) {
      pos = input$pos
      sensD = 100*(darmTest$testPos / pos)
      spezD = 100*(darmTest$testNeg / darmTest$neg)
      nD = darmTest$pos + darmTest$neg
      range = c(0,nD)
      rangePos = c(darmTest$pos, nD)
      rangeSens = c(round(sensD-20), round(sensD))
      rangeSpez = c(round(spezD-20), round(spezD))
    }
    
    #EisenmangelTest
    else if(input$data == eisenmangelTest$name) {
      pos = input$pos
      sensD = 100*(eisenmangelTest$testPos / pos)
      spezD = 100*(eisenmangelTest$testNeg / eisenmangelTest$neg)
      nD = eisenmangelTest$pos + eisenmangelTest$neg
      range = c(0,nD)
      rangePos = c(eisenmangelTest$pos, nD)
      rangeSens = c(round(sensD-20), round(sensD))
      rangeSpez = c(round(spezD-20), round(spezD))
    }
    
    #ProstataTest
    else if(input$data == prostataTest$name) {
      pos = input$pos
      sensD = 100*(prostataTest$testPos / pos)
      spezD = 100*(prostataTest$testNeg / prostataTest$neg)
      nD = prostataTest$pos + prostataTest$neg
      range = c(0,nD)
      rangePos = c(prostataTest$pos, nD)
      rangeSens = c(round(sensD-20), round(sensD))
      rangeSpez = c(round(spezD-20), round(spezD))
    }
    
    #Schilddrüsentest
    else if(input$data == schilddrüsenTest$name) {
      pos = input$pos
      sensD = 100*(schilddrüsenTest$testPos / pos)
      spezD = 100*(schilddrüsenTest$testNeg / schilddrüsenTest$neg)
      nD = schilddrüsenTest$pos + schilddrüsenTest$neg
      range = c(0,nD)
      rangePos = c(schilddrüsenTest$pos, nD)
      rangeSens = c(round(sensD-20), round(sensD))
      rangeSpez = c(round(spezD-20), round(spezD))
    }
    
    #Coronatest1
    else if(input$data == coronaTest1$name) {
      pos = input$pos
      sensD = 100*(coronaTest1$testPos / pos)
      spezD = 100*(coronaTest1$testNeg / coronaTest1$neg)
      nD = coronaTest1$pos + coronaTest1$neg
      range = c(0,nD)
      rangePos = c(coronaTest1$pos, nD)
      rangeSens = c(round(sensD-20), round(sensD))
      rangeSpez = c(round(spezD-20), round(spezD))
    }
    
    #Coronatest2
    else if(input$data == coronaTest2$name) {
      pos = input$pos
      sensD = 100*(coronaTest2$testPos / pos)
      spezD = 100*(coronaTest2$testNeg / coronaTest2$neg)
      nD = coronaTest2$pos + coronaTest2$neg
      range = c(0,nD)
      rangePos = c(coronaTest2$pos, nD)
      rangeSens = c(round(sensD-20), round(sensD))
      rangeSpez = c(round(spezD-20), round(spezD))
    }
    
    #Coronatest3
    else if(input$data == coronaTest3$name) {
      pos = input$pos
      sensD = 100*(coronaTest3$testPos / pos)
      spezD = 100*(coronaTest3$testNeg / coronaTest3$neg)
      nD = coronaTest3$pos + coronaTest3$neg
      range = c(0,nD)
      rangePos = c(coronaTest3$pos, nD)
      rangeSens = c(round(sensD-20), round(sensD))
      rangeSpez = c(round(spezD-20), round(spezD))
    }
    else {
      pos = input$pos
      sensE = 100*(borrelTest$testPos / pos)
      spezE = 100*(borrelTest$testNeg / borrelTest$neg)
      nE = borrelTest$pos + borrelTest$neg
      range = c(0,nE)
      
      #ändern wenn positiv verändert wird
      rangePos = c(borrelTest$pos, nE)
      rangeSens = c(round(sensE-20), round(sensE))
      rangeSpez = c(round(spezE-20), round(spezE))
    }
    updateSliderInput(session, "n", 
                      value = max(range),
                      min = min(range),
                      max = max(range),
                      step = 5)
    
    updateSliderInput(session, "pos", 
                      value = pos,
                      min = min(rangePos),
                      max = max(rangePos),
                      step = 5)
    
    updateSliderInput(session, "sens", 
                      value = max(rangeSens),
                      min = min(rangeSens),
                      max = max(rangeSens),
                      step = 2)
    
    updateSliderInput(session, "spez", 
                      value = max(rangeSpez),
                      min = min(rangeSpez),
                      max = max(rangeSpez),
                      step = 2)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
