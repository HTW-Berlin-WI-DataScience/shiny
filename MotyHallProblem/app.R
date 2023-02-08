#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

n <- 1000
doors <- c(1,2,3)

monty_df = NULL

monty_df_2 = NULL

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Monty Hall Problem"),

    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        
        radioButtons("selected", "Spieler*innen Verhalten", choices = c("Immer wechseln", "Nur wechseln, wenn Monty Hall die gewählte Tür der Spieler*in öffnet"), selected = "Immer wechseln"),
        radioButtons("selectedTwo", "Monty Hall öffnet zufällig ausgewählte Ziegentür", choices = c("Darf von Spieler*in gewählte Tür öffnen", "Darf von Spieler*in gewählte Tür nicht öffnen"), selected = "Darf von Spieler*in gewählte Tür öffnen"),
        actionButton("start", "Start")
      ),
      # Show a plot of the generated distribution
        mainPanel(
          plotOutput("plot"),
          textOutput("text1"),
          textOutput("text2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  p_blank <-  ggplot()+
                geom_hline(yintercept = 1/3, color = '#8080ff', linetype = 2) +
                geom_hline(yintercept = 1/2, color = '#8080ff', linetype = 2) +
                geom_hline(yintercept = 2/3, color = '#8080ff', linetype = 2) +
                geom_text() + annotate("text", label = "1/3", x = 1050, y = 0.3, size = 4, colour = "black")+
                geom_text() + annotate("text", label = "2/3", x = 1050, y = 0.63, size = 4, colour = "black")+
                ylab('Relative Gewinnhäufigkeit')+xlab('Iteration') +
                theme(legend.title = element_blank())+
                ylim(0,1)+
                ggtitle("Gewinnwahrscheinlichkeit")
  
  
  reac <- reactiveValues()
  reac$p_lines <- p_blank
  reac$wahrscheinlichkeit <- ""
  reac$counter <- 0

    observeEvent(c(input$selected, input$selectedTwo),{
      reac$counter <- 1
      
      for(i in 1:n){
        
        monty_df_i <- NULL
        
        #Get winning door
        winning_door <- sample(doors, 1)
        
        #Get players door
        players_original_choice <- sample(doors, 1)
        

        
        #Get losing doors
        losing_doors <- setdiff(doors, winning_door)
        
        
        switcherTwo <- input$selectedTwo
        
        if(switcherTwo == "Darf von Spieler*in gewählte Tür öffnen"){
          door_open <- sample(losing_doors, 1)
        }else{
          door_open <- ifelse(players_original_choice == winning_door, 
                              sample(losing_doors, 1),
                              setdiff(losing_doors, players_original_choice))
        }

        
        #TO- Do, auch beu Auto wechseln
        #change value (Immer wechseln , Nie wechseln , Bei Ziege wechseln, Bei Auto wechseln)
        switcher <- input$selected
        
        #Finale choice of player
        players_final_choice <-
          if(switcher == "Immer wechseln" & door_open == players_original_choice){
            sample(setdiff(doors, c(door_open, players_original_choice)), 1)
          }else if(switcher == "Immer wechseln"){
            setdiff(doors, c(door_open, players_original_choice))
          } else if (switcher == "Nur wechseln, wenn Monty Hall die gewählte Tür der Spieler*in öffnet" & door_open == players_original_choice){
            sample(setdiff(doors, c(door_open, players_original_choice)), 1)
          } else{
            players_original_choice
          }
        

        
        
        won <- ifelse(players_final_choice == winning_door,1,0)
        lost <- ifelse(players_final_choice == winning_door,0,1)
        
        #Append into dataframe
        monty_df_i <- data.frame(i, players_final_choice, players_original_choice, winning_door, door_open, won, lost, counter = reac$counter)
        
        
        monty_df <- rbind.data.frame(monty_df, monty_df_i)
        
        win_prob <- cumsum(monty_df$won) / (1:i)
        
        lose_prob <- cumsum(monty_df$lost) / (1:i)
        
        
        monty_df_new <- monty_df
        
        monty_df_new$win_prob <- win_prob
        monty_df_new$lose_prob <- lose_prob
        
      }
      
      
      
      gl <- geom_line(aes(x = i , y = win_prob, color = factor(counter)),
                      monty_df_new, 
                      size = 0.5,  
                      linetype= "solid",
                      alpha = 1.0)
      
      reac$p_lines <- p_blank
      reac$p_lines <- reac$p_lines + gl
      
      
      
     
      
                                   
    })
    
    
    observeEvent(input$start, {
      reac$counter <- reac$counter + 1
      for(i in 1:n){
        
        
        
        #Get winning door
        winning_door <- sample(doors, 1)
        
        #Get players door
        players_original_choice <- sample(doors, 1)
        
        #Get losing doors
        losing_doors <- setdiff(doors, winning_door)
        
        switcherTwo <- input$selectedTwo
        
        if(switcherTwo == "Darf von Spieler*in gewählte Tür öffnen"){
          door_open <- sample(losing_doors, 1)
        }else{
          door_open <- ifelse(players_original_choice == winning_door, 
                              sample(losing_doors, 1),
                              setdiff(losing_doors, players_original_choice))
        }
        

        
        
        
        #TO- Do, auch beu Auto wechseln
        #change value (Immer wechseln , Nie wechseln , Bei Ziege wechseln, Bei Auto wechseln)
        switcher <- input$selected
        
        #Finale choice of player
        players_final_choice <-
          if(switcher == "Immer wechseln" & door_open == players_original_choice){
            sample(setdiff(doors, c(door_open, players_original_choice)), 1)
          }else if(switcher == "Immer wechseln"){
            setdiff(doors, c(door_open, players_original_choice))
          } else if (switcher == "Nur wechseln, wenn Monty Hall die gewählte Tür der Spieler*in öffnet" & door_open == players_original_choice){
            sample(setdiff(doors, c(door_open, players_original_choice)), 1)
          }else{
            players_original_choice
          }
        
        
        won <- ifelse(players_final_choice == winning_door,1,0)
        lost <- ifelse(players_final_choice == winning_door,0,1)
        
       
        
        #Append into dataframe
        monty_df_i <- data.frame(i, players_final_choice, players_original_choice, winning_door, door_open, won, lost, counter = reac$counter)
        
        
        
        monty_df <- rbind.data.frame(monty_df, monty_df_i)
        
        win_prob <- cumsum(monty_df$won) / (1:i)
        
       
        
        lose_prob <- cumsum(monty_df$lost) / (1:i)
        
        
        monty_df_new <- monty_df
        
        monty_df_new$win_prob <- win_prob
        monty_df_new$lose_prob <- lose_prob
        
        
        
        
      }
      if(switcher == "Immer wechseln"){
        reac$wahrscheinlichkeit <- ""
        reac$wahrscheinlichkeit <- paste(reac$wahrscheinlichkeit , "Relative Gewinnhäufigkeit nach 1000 Durchläufen: ", round(sum(monty_df_new$win_prob)/n, digits= 3))
      }else{
        reac$wahrscheinlichkeit <- ""
        reac$wahrscheinlichkeit <- paste(reac$wahrscheinlichkeit , "Relative Gewinnhäufigkeit nach 1000 Durchläufen: ", round(1 - sum(monty_df_new$lose_prob)/n, digits = 3))
      }
      
      
      
      gl <- geom_line(aes(x = i , y = win_prob, color = factor(counter)),
                      show.legend = FALSE,
                      monty_df_new, 
                      size = 0.5,  
                      linetype= "solid",
                      alpha = 1.0)
      
      reac$p_lines <- reac$p_lines + gl
      
      
    })
    

  
  
  output$plot <- renderPlot(reac$p_lines)
  
  output$text1 <- renderText(reac$wahrscheinlichkeit)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
