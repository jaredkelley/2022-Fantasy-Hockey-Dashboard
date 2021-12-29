# Data -----------------------------------------------

library(stringr)
library(shiny)
library(shinydashboard)
library(rvest)
library(tidyverse)


# Order by points
Fantasy_Players <- Fantasy_Players %>% 
  arrange(desc(PTS))



Teams <- Fantasy_Players %>% 
  group_by(Owner) %>% 
  summarise(PTS = sum(PTS),
            A = sum(A),
            G = sum(G),
            GP = sum(GP))

# Create the server -------------------------------------------------------


server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(data = Teams, aes_string(x = 'Owner', y = input$y_var, fill = "Owner"))+
      geom_col(stat = "identity")+
      labs(x = "Owners", y = input$y_var) +
      ggtitle("2021-2022 Season") + coord_flip()
  })
  output$table <- renderDataTable(Fantasy_Players, options = list)
  #output$table <- renderDataTable(Teams, options= list)
}

