# Load libraries, data -----------------------------------------------

library(stringr)
library(shiny)
library(shinydashboard)
library(rvest)
library(tidyverse)


# Page 1 - Visualization -------------------------------------------
select_values <- colnames(Teams)
select_values <- select_values[select_values %in% c('PTS', 'GP', 'G', 'A')]

sidebar_content <- sidebarPanel(
  selectInput(
    "y_var",
    label = "Statistics",
    choices = select_values,
    selected = 'PTS'
  )
)

main_content <- mainPanel(
  plotOutput("plot")
)

first_panel <- tabPanel(
  "Standings",
  titlePanel("Fantasy Hockey 2021-2022"),
  p("Select Inputs Below"),
  sidebarLayout(
    sidebar_content, main_content
  )
)

# Panel 2 - All Fantasy Players Table -------------------------------------------
main_content_2 <- mainPanel(
  dataTableOutput('table')
)

second_panel <- tabPanel(
  "Player Performance",
  titlePanel("Fantasy Hockey 2021"),
  mainPanel(
    main_content_2
  )
)


# User Interface -----------------------------------------------------
ui <- navbarPage(
  "Fantasy Hockey 2021-2022",
  first_panel,
  second_panel
)


