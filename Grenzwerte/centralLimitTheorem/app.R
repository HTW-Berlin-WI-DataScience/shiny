library(shiny)
library(ggplot2)
##Author: ...

m <- 1000  # number of samples; fest einstellen
mean <- 8 # true population mean, z.B. Miete in Euro/qm; fest einstellen
sd <- 2  # population standard deviation; fest einstellen


# Antworten Frage 1
quizOne_answer_one <- "sie verändert sich nicht"
quizOne_answer_two <- "sie wird kleiner"
quizOne_answer_three <- "sie wird größer" #(richtig)

# Antworten Frage 2
quizTwo_answer_one <- "einer Gleichverteilung um den wahren Mittelwert"
quizTwo_answer_two <- "einer Normalverteilung um den wahren Mittelwert" #(richtig)
quizTwo_answer_three <- "einer Exponentialverteilung mit dem wahren Mittelwert"

#Antworten Frage 3
greekCodes <- list(HTML("\\sigma / n"), HTML("\\sigma / \\sqrt{n}"), HTML("\\sigma * n^2")) # note the six backslahes



# Labels for Questions
label_question_one <- HTML('Wie verändert sich die Streuung der tausend Mittelwerte, wenn wir die Stichproben vergrößern?')
label_question_two <- 'Welcher Wahrscheinlichkeitsverteilung folgen die Mittelwerte näherungsweise?'
label_question_three <- HTML("Wenn \\(\\sigma\\) die Standardabweichung aller Mietpreise pro Quadratmeter ist und n die Stichprobengröße, dann beträgt die Standabweichung der Stichprobenmittelwerte")

# Main Text
text1 <- HTML("In dieser <b> App </b> werden Aussagen des Zentralen Grenzwertsatzes der Statistik demonstriert. Als Beispiel möchten wir die mittlere Miete pro Quadratmeter in einer Stadt bestimmen. In der Praxis kennen wir üblicherweise nur die Mietpreise von einer Stichprobe der Immobilien und können daraus die wahre mittlere Miete nur schätzen. Der zentrale Grenzwertsatz hilft uns dabei, die Genauigkeit unserer Schätzung zu untersuchen.")
text2 <- HTML("Um die Theorie zu erklären, gehen wir davon aus, dass wir den Mietpreis pro qm aller Immobilien in der Stadt und damit auch dessen wahren Mittelwert und die Standardabweichung \\(\\sigma\\) als Maß für die Streuung kennen.
              Wir ziehen nun tausendmal eine Stichprobe der Größe n aus den Daten und berechnen für jede der tausend Stichproben die mittlere Miete und schauen, wie sich die tausend Schätzungen zum wahren Mittelwert verhalten.")
text3 <- 'Das Histogramm zeigt die Verteilung der eintausend Mittelwerte sowie den wahren Mittelwert als rote gestrichelte Linie.'

ui <- fluidPage(
    withMathJax(),
    tags$head(
        tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css", integrity="sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH", crossorigin="anonymous"),
        tags$script(src="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js", integrity="sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm", crossorigin="anonymous")
    ),
    tags$head(
        tags$link(rel="stylesheet",
                  href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css",
                  integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
                  crossorigin="anonymous"),
        HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
        HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
        HTML('
    <script>
      document.addEventListener("DOMContentLoaded", function(){
        renderMathInElement(document.body, {
          delimiters: [{left: "$", right: "$", display: false}]
        });
      })
    </script>')
    ),
    
    titlePanel("Zentraler Grenzwertsatz"),
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("sample",
                        "Stichproben größe:",
                        step = 10,
                        min = 10,
                        max = 500,
                        value = 100),
            selectInput(
                inputId = 'mainInput',
                label = label_question_one,
                choices = c("",quizOne_answer_one,
                            quizOne_answer_two,
                            quizOne_answer_three)
            ),
            uiOutput(
                outputId = 'secondInputUI'
            ),
            uiOutput(
                outputId = 'text'
            ),
            uiOutput(
                outputId = 'thirdInput'
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


server <- function(input, output, session) {
    
    #Event wenn Antwort für erste Frage getätig wird --> zur zweiten Frage
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
    
    
    
    
    #Event wenn Antwort für erste Frage getätig wird --> zur dritten Frage
    observeEvent(input$secondInput, {
        if (input$secondInput == quizTwo_answer_two)
            output$thirdInput <-
                renderUI(
                    selectizeInput(
                        'thirdInput',
                        label = NULL,
                        choices = greekCodes,
                        options = list(render = I("
                                    {
                                    item:   function(item, escape) { 
                                      var html = katex.renderToString(item.label);
                                      return '<div>' + html + '</div>'; 
                                    },
                                    option: function(item, escape) { 
                                      var html = katex.renderToString(item.label);
                                      return '<div>' + html + '</div>'; 
                                    }
                                    }
                                                              "))
                    )
                )
        
        output$text <- renderUI(
            withMathJax(label_question_three)
        )
        
    })
    
    # reactive Werte für das Histo
    size <- reactive({
        as.numeric(input$sample)
    })
    
    samples <- reactive({
        matrix(rnorm(size() *m, mean, sd), ncol = m)
    })
    
    mean_vector <- reactive({
        data.frame(means = colMeans(samples()))
    })
    
    my_binwidth <- reactive({
        (max(mean_vector()) - min(mean_vector())) / 12
    })
    
    value_reactive <- reactive({
        input$secondInput
    })
    
    
    plotA_reactive <- reactive({
        ggplot(data = mean_vector(), aes(x=means)) +
            geom_histogram(aes(y=..density..), binwidth = my_binwidth(), col = "white") + 
            #  geom_density(alpha=.2, fill="#FF6666") +
            geom_vline(aes(xintercept=mean), 
                       color="red", linetype="dashed", size=1) +
            #anser_reactive()+
            #  geom_jitter(aes(x=means, y=0), col = "blue") + 
            xlim(6, 10) +
            xlab("x") +
            ylab("Dichte") +
            ggtitle("Histogramm der Stichprobenmittelwerte, wahrer Mittelwert 
          und deren theoretische Normalverteilung. (rot)")
    }) 
    
    
    #Plot Histo
    output$plot <- renderPlot({
        
        if(length(value_reactive()) > 0){
            
            if(!value_reactive() == ""){
                
                if(value_reactive() == quizTwo_answer_two){
                    plotA_reactive() + stat_function(fun = function(x) dnorm(x,mean,sd/sqrt(size())),colour = "red")
                }
                
            }
            else {
                plotA_reactive()
            }
            
        } 
        else {
            plotA_reactive()
        }
        
    })
    
}

shinyApp(ui, server)
