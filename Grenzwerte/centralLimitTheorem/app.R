
# Visualisation of the central limit theorem (Zentraler Grenzwertsatz): 
# the mean values of population samples are approximately normally distributed around the true population mean

m <- 1000  # number of samples; fest einstellen
mean <- 8 # true population mean, z.B. Miete in Euro/qm; fest einstellen
sd <- 2  # population standard deviation; fest einstellen

# Antworten Frage 1
quizOne_answer_one <- "sie verändert sich nicht"
quizOne_answer_two <- "sie wird kleiner"
quizOne_answer_three <- "sie wird größer" #(richtig)

# Antwort Frage 2
quizTwo_answer_one <- "einer Gleichverteilung um den wahren Mittelwert"
quizTwo_answer_two <- "einer Normalverteilung um den wahren Mittelwert" #(richtig)
quizTwo_answer_three <- "einer Exponentialverteilung mit dem wahren Mittelwert"

#Antwort Frage 3
quizThree_answer_one <- helpText('$\\sigma / n$')
quizThree_answer_two <- helpText('$\\sigma / sqrt{n}$')
quizThree_answer_three <- helpText('$\\sigma * n^2$')

# Labels for Questions
label_question_one <- 'Wie verändert sich die Streuung der tausend Mittelwerte, wenn wir die Stichproben vergrößern?'
label_question_two <- 'Welcher Wahrscheinlichkeitsverteilung folgen die Mittelwerte näherungsweise?'
label_question_three <- helpText('Wenn $\\sigma$ die Standardabweichung aller Mietpreise pro Quadratmeter ist und n die Stichprobengröße, dann beträgt die Standabweichung der Stichprobenmittelwerte')

# Main Text
text1 <- "In dieser App werden Aussagen des Zentralen Grenzwertsatzes der Statistik demonstriert. Als Beispiel möchten wir die mittlere Miete pro Quadratmeter in einer Stadt bestimmen. In der Praxis kennen wir üblicherweise nur die Mietpreise von einer Stichprobe der Immobilien und können daraus die wahre mittlere Miete nur schätzen. Der zentrale Grenzwertsatz hilft uns dabei, die Genauigkeit unserer Schätzung zu untersuchen."
text2 <- "Um die Theorie zu erklären, gehen wir davon aus, dass wir den Mietpreis pro qm aller Immobilien in der Stadt und damit auch dessen wahren Mittelwert und die Standardabweichung ... als Maß für die Streuung kennen. Wir ziehen nun tausendmal eine Stichprobe der Größe n aus den Daten und berechnen für jede der tausend Stichproben die mittlere Miete und schauen, wie sich die tausend Schätzungen zum wahren Mittelwert verhalten."
text3 <- "Das Histogramm zeigt die Verteilung der eintausend Mittelwerte sowie den wahren Mittelwert als rote gestrichelte Linie."

library(shiny)
library(ggplot2)


ui <- fluidPage(

    # Application title
    titlePanel("Zentraler Grenzwertsatz"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("sample",
                        "sample size:",
                        step = 10,
                        min = 10,
                        max = 500,
                        value = 100),
            selectInput(
                inputId = 'mainInput',
                label = label_question_one,
                selected = quizOne_answer_one,
                choices = c("",quizOne_answer_one,
                            quizOne_answer_two,
                            quizOne_answer_three)
            ),
            uiOutput(
                outputId = 'secondInputUI'
            ),
            uiOutput(
                outputId = 'theeredInput'
            ),
            uiOutput(
                outputId = 'lastInput'
            )
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            p(text1),
            br(),
            p(text2),
            br(),
            p(text3),
            plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    size <- reactive({
        as.numeric(input$sample)
    })
    
    output$plot <- renderPlot({
        samples <- matrix(rnorm(size() *m, mean, sd), ncol = m)
        mean_vector <- data.frame(means = colMeans(samples))
        my_binwidth <- (max(mean_vector) - min(mean_vector)) / 12
        
        ggplot(data = mean_vector, aes(x=means)) +
            geom_histogram(aes(y=..density..), binwidth = my_binwidth, col = "white") + 
            #  geom_density(alpha=.2, fill="#FF6666") +
            geom_vline(aes(xintercept=mean), 
                       color="red", linetype="dashed", size=1) +
            stat_function(fun = function(x) dnorm(x,mean,sd/sqrt(size())),colour = "red") +
            #  geom_jitter(aes(x=means, y=0), col = "blue") + 
            xlim(6, 10) +
            xlab("x") +
            ylab("Dichte") +
            ggtitle("Histogramm der Stichprobenmittelwerte, wahrer Mittelwert 
          und deren theoretische Normalverteilung (rot)")
    })
    
    
    
    observeEvent(input$mainInput, {
        if (input$mainInput == quizOne_answer_three)
            output$secondInputUI <- 
                renderUI(
                    selectInput(
                        inputId = 'secondInput',
                        label = label_question_two,
                        choices = c("",quizTwo_answer_one,
                                    quizTwo_answer_two,
                                    quizTwo_answer_three)
                    )
                )
    })
    
    output$anser_one <- renderUI({
        withMathJax(quizThree_answer_one)  
    })
    
    output$anser_two <- renderUI({
        withMathJax(quizThree_answer_two)  
    })
    
    output$anser_three <- renderUI({
        withMathJax(quizThree_answer_three)  
    })
    
    observeEvent(input$secondInput, {
        if (input$secondInput == quizTwo_answer_two)
            output$theeredInput <- 
                renderUI(
                    selectInput(
                        inputId = 'theeredInput',
                        label = label_question_three,
                        choices = c("",uiOutput("answer_one"),
                                    uiOutput("answer_two"),
                                    uiOutput("answer_three"))
                    )
                )
    })
    
    
    observeEvent(input$theeredInput, {
        if (input$theeredInput == quizThree_answer_three)
            output$lastInput <- 
                renderUI("alle Antworten richtig")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
