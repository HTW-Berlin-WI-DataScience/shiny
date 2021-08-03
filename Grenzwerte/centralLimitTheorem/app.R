
# Visualisation of the central limit theorem (Zentraler Grenzwertsatz): 
# the mean values of population samples are approximately normally distributed around the true population mean

m <- 1000  # number of samples; fest einstellen
mean <- 8 # true population mean, z.B. Miete in Euro/qm; fest einstellen
sd <- 2  # population standard deviation; fest einstellen

answer_one <- "Die Annäherung ist umso besser, je größer der Stichporbenumfang ist"

answer_two <- "Sample Size ist die Stichprobengröße"

answer_three <- "Wenn smaple size < 30 ist, ist die Annährung der Normalverteilung gut"

answer <- "Antwort auf die Frage kann hier angegeben werden... Platzhalter Text"

library(shiny)
library(ggplot2)

ui <- fluidPage(
    
    # Application title
    titlePanel("Central Limit Theorem"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("sample",
                        "sample size:",
                        step = 10,
                        min = 10,
                        max = 500,
                        value = 100),
            selectInput("UserInput",
                        "Welche Aussage trifft zu?",
                        choices = c("",answer_one,
                                    answer_two,
                                    answer_three)), 
            tags$b(textOutput("Result")),
            textOutput("Text")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
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
    
    
    GrenzwertQuiz =function(q.c){ 
        if (q.c == answer_one){
            QuizResult= "Richtige Antwort" 
        }
        else{
            QuizResult="Falsche Antwort"
        }
        
        return(QuizResult)
    }
    
    output$Result <- renderText({
        GrenzwertQuiz(input$UserInput)
    })
    
    output$Text <- renderText({
        if(GrenzwertQuiz(input$UserInput) == "Richtige Antwort"){
            answer
        }
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
