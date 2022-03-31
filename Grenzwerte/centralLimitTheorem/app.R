library(shiny)
library(ggplot2)

library(plotly)
##Author: ...

m <- 1000  # number of samples; fest einstellen
mean <- 8 # true population mean, z.B. Miete in Euro/qm; fest einstellen
sd <- 2  # population standard deviation; fest einstellen
data <- rnorm(1000, mean = mean, sd = sd)

mean_vector <- vector()


# Antworten Frage 1
quizOne_answer_one <- "sie verändert sich nicht"
quizOne_answer_two <- "sie wird kleiner"
quizOne_answer_three <- "sie wird größer" #(richtig)

# Antworten Frage 2
quizTwo_answer_one <- "einer Gleichverteilung um den wahren Mittelwert"
quizTwo_answer_two <- "einer Normalverteilung um den wahren Mittelwert" #(richtig)
quizTwo_answer_three <- "einer Exponentialverteilung mit dem wahren Mittelwert"

#Antworten Frage 3
greekCodes <- list("", HTML("\\sigma / n"), HTML("\\sigma / \\sqrt{n}"), HTML("\\sigma * n^2")) # note the six backslahes



# Labels for Questions    den wahren Mittelwert als rote gestrichelte Linie.
label_question_one <- HTML('Wie verändert sich die Streuung der tausend Mittelwerte, wenn wir die Stichproben vergrößern?')
label_question_two <- 'Welcher Wahrscheinlichkeitsverteilung folgen die Mittelwerte näherungsweise?'
label_question_three <- HTML("<b>Wenn \\(\\sigma\\) die Standardabweichung aller Mietpreise pro Quadratmeter ist und n die Stichprobengröße, dann beträgt die Standardabweichung der Stichprobenmittelwerte</b>")

# Main Text
text1 <- HTML("In dieser App werden Aussagen des Zentralen Grenzwertsatzes der Statistik demonstriert. Als Beispiel möchten wir die mittlere Miete pro Quadratmeter in einer Stadt bestimmen. In der Praxis kennen wir üblicherweise nur die Mietpreise von einer Stichprobe der Immobilien und können daraus die wahre mittlere Miete nur schätzen. Der zentrale Grenzwertsatz hilft uns dabei, die Genauigkeit unserer Schätzung zu untersuchen.")
text2 <- HTML("Um die Theorie zu erklären, gehen wir davon aus, dass wir den Mietpreis pro qm aller Immobilien in der Stadt und damit auch dessen wahren Mittelwert und die Standardabweichung \\(\\sigma\\) als Maß für die Streuung kennen.
              Wir ziehen nun tausendmal eine Stichprobe der Größe n aus den Daten und berechnen für jede der tausend Stichproben die mittlere Miete und schauen, wie sich die tausend Schätzungen zum wahren Mittelwert verhalten.")
text3 <- 'Das Histogramm zeigt die Verteilung der eintausend Mittelwerte und den wahren Mittelwert als rote gestrichelte Linie,'
text4 <- "sowie die theoretische Normalverteilung der Mittelwerte."
text5 <- "Das hellrote Histogramm zeigt die Verteilung der Grundgesamtheit an (Mietpreis aller Immobilien)."

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
                        "Stichprobengröße:",
                        step = 10,
                        min = 10,
                        max = 500,
                        value = 100),
            selectInput("mainInput", label_question_one, choices = c("",quizOne_answer_one,
                                                                     quizOne_answer_two,
                                                                     quizOne_answer_three), selectize = FALSE),
            textOutput("falscheAntwortOne"),
            tags$head(tags$style("#falscheAntwortOne{color: red;
                                 font-size: 15px;
                                 }"
            )
            ),
            
          
            htmlOutput("answerOne"),
            tags$head(tags$style("#answerOne{color: green;
                                 font-size: 15px;
                                 }"
                                 )
            ),
            br(),
            
            uiOutput(
                outputId = 'secondInputUI'
            ),
            textOutput("falscheAntwortTwo"),
            tags$head(tags$style("#falscheAntwortTwo{color: red;
                                 font-size: 15px;
                                 }"
            )
            ),
            
            
            htmlOutput("answerTwo"),
            tags$head(tags$style("#answerTwo{color: green;
                                 font-size: 15px;
                                 }"
                                 )
            ),
            br(),
            
            uiOutput(
                outputId = 'text'
            ),
            
            uiOutput(
                outputId = 'thirdInput'
            ),
            textOutput("falscheAntwortThree"),
            tags$head(tags$style("#falscheAntwortThree{color: red;
                                 font-size: 15px;
                                 }"
            )
            ),
            htmlOutput("answerThree"),
            tags$head(tags$style("#answerThree{color: green;
                                 font-size: 15px;
                                 }"
                                 )
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            p(text1),
            br(),
            p(text2),
            br(),
            textOutput("answertext"),
            br(),
            p(text5),
            plotlyOutput("plot")
        )
    )
)


server <- function(input, output, session) {
  
  
  
  falscheAntwortReaktiv <- reactive({
    if (input$mainInput != quizOne_answer_two){
      "Die Antwort ist leider falsch"
    }
  })
  
  observeEvent(input$mainInput, {
    if (input$mainInput == quizOne_answer_two){
      removeUI(
        selector = "div:has(> #mainInput)"
      )
      
      output$answerOne <- renderText({
        paste("<b>Richtig: " ,quizOne_answer_two, "</b>")
      })
      
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
    }
    else
      if(input$mainInput != ""){
      output$falscheAntwortOne <- renderText({
        falscheAntwortReaktiv()
      })
    }
    
  })

    
  
  falscheAntwortReaktivTwo <- reactive({
    if (input$secondInput != quizTwo_answer_two){
      "Die Antwort ist leider falsch"
    }
  })
    
    #Event wenn Antwort für erste Frage getätig wird --> zur dritten Frage
    observeEvent(input$secondInput, {
        if (input$secondInput == quizTwo_answer_two){
          removeUI(
            selector = "div:has(> #secondInput)"
          )
          
          output$answerTwo <- renderText({
            paste("<b>Richtig: " ,quizTwo_answer_two, "</b>")
          })
          
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
        } else
          if(input$secondInput != ""){
            output$falscheAntwortTwo <- renderText({
              falscheAntwortReaktivTwo()
            })
          }
      
        if (input$secondInput == quizTwo_answer_two){
          output$text <- renderUI(
            withMathJax(label_question_three)
          )
        }
    })
    
    
    
    
    falscheAntwortReaktivThree <- reactive({
      if (input$thirdInput != HTML("\\sigma / \\sqrt{n}")){
        "Die Antwort ist leider falsch"
      }
    })
    
    
    observeEvent(input$thirdInput, {
      if (input$thirdInput == HTML("\\sigma / \\sqrt{n}")){
        removeUI(
          selector = "div:has(> #thirdInput)"
        )
        output$answerThree <- renderText({
          paste("<b>Richtig:", withMathJax("\\(\\sigma / \\sqrt{n}\\)"), "</b>")
        })
        
      }
      else
        if(input$thirdInput != ""){
          output$falscheAntwortThree <- renderText({
            falscheAntwortReaktivThree()
          })
        }
    })
    
    
    
    # reactive Werte für das Histo
    size <- reactive({
        as.numeric(input$sample)
    })
    
    samples <- reactive({
        matrix(rnorm(size() *m, mean, sd), ncol = m)
    })
    
    mean_vector <- reactive({
      replicate(1000, mean(sample(data, size(), replace = FALSE)))
    })
    
    dataFrame_means <- reactive({
      data.frame(daten = mean_vector())
    })
    
    my_binwidth <- reactive({
        (max(dataFrame_means() - min(dataFrame_means()))) / 12
    })
    
    value_reactive <- reactive({
        input$secondInput
    })
    
    output$answertext <- reactive({
      if(length(value_reactive()) > 0){
        
        if(!value_reactive() == ""){
          
          if(value_reactive() == quizTwo_answer_two){
            paste(text3, text4)
          }else{
            text3
          }
          
        }
        else {
          text3
        }
        
      } 
      else {
        text3
      }
    })
    
    
    figur2 <- reactive({
      ggplotly(
        ggplot(data = dataFrame_means(), aes(x=daten)) +
          geom_histogram(aes(y=..density..), binwidth = my_binwidth(), col = "white") +
          #  geom_density(alpha=.2, fill="#FF6666") +
          geom_vline(aes(xintercept=mean),
                     color="red", linetype="dashed", size=1) +
          #anser_reactive()+
          #  geom_jitter(aes(x=means, y=0), col = "blue") +
          xlim(0, 15) +
          xlab("mittlerer Mietpreis pro quadratmeter") +
          ylab("Dichte") +
          geom_histogram(data = data.frame(data = data), aes(x = data, y=..density..), binwidth = my_binwidth(), col = "white", alpha=.2, fill="#FF6666")
      )%>%
        rangeslider() %>%
        layout(xaxis = list(range = c(6, 10))) 
    })
  
    
    # #Plot Histo
    output$plot <- renderPlotly({
        if(length(value_reactive()) > 0){

            if(!value_reactive() == ""){

                if(value_reactive() == quizTwo_answer_two){
                  xa <- seq(0,15, length.out=1000)
                  dd <- with(dataFrame_means(), data.frame(xa= xa, y = dnorm(xa, mean(daten), sd(daten))))
                  ggplotly(
                    ggplot(data = dataFrame_means(), aes(x=daten)) +
                      geom_histogram(aes(y=..density..), binwidth = my_binwidth(), col = "white") +
                      #  geom_density(alpha=.2, fill="#FF6666") +
                      geom_vline(aes(xintercept=mean),
                                 color="red", linetype="dashed", size=1) +
                      #anser_reactive()+
                      #  geom_jitter(aes(x=means, y=0), col = "blue") +
                      xlim(0, 15) +
                      xlab("mittlerer Mietpreis pro quadratmeter") +
                      ylab("Dichte") + geom_function(fun = dnorm) +
                      geom_line(data = dd, aes(x = xa, y=y), color = "red")+
                      geom_histogram(data = data.frame(data = data), aes(x = data, y=..density..), binwidth = my_binwidth(), col = "white", alpha=.2, fill="#FF6666")
                    #stat_function(fun = function(x) dnorm(x,mean,sd/sqrt(size())),colour = "red")
                  ) %>%
                    rangeslider()%>%
                    layout(xaxis = list(range = c(6, 10))) 

                }
              else {
                figur2()
                }

            }
            else {
              figur2()
            }

        }
        else {
          figur2()
        }

    })
    
}

shinyApp(ui, server)
