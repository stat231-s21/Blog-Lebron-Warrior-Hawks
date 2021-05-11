library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(lubridate)
library(bslib)
library(shinycssloaders)
## Import Data
all_players_norm <- read.csv("Data/all_players_norm.csv")

## Generated lists and names for TAB1: team line graphs

## Get stats for 2021 season
player_2021 <- all_players_norm %>%
  filter(year == 2021)

## Tab 3 and 4: Table choices

player_choices <- unique(player_2021$Player)

ui <- navbarPage(
  ## Introducing a minty theme
  theme = bs_theme(version = 4, bootswatch = "minty"),
  title = "NBA Stats",
 
  tabPanel(
    title = "Historical Player Comparison",
    
    sidebarLayout(
      ## user selects players to display Data
      sidebarPanel(
        selectizeInput(inputId = "Player_Stats"
                       , label = "Choose a player in the current season:"
                       , choices = player_choices
                       , selected = "LeBron James"
                       , multiple = FALSE),
        ## Added NBA Logo to Side Panel
        h6("Table loading takes time (~90 seconds). Please be patient. Thank you!"),
        img(src = "nbalogo.jpeg", height = 290, width = 150)
      ),
      mainPanel(
        shinycssloaders::withSpinner(
          DT::dataTableOutput(outputId = "table")
        ),
        p("Note: A lower euclidian distance (euc) means higher similarity")
      )
    )
  )


)
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

server <- function(input,output){
  
  ## data for selected player in 2021
  ## data for all other players for all other seasons
  
  data_for_table <- reactive({
    ## When user changes/adds players, filter the dataset for them
    player_data <- all_players_norm %>%
      filter(Player == input$Player_Stats, year == 2021)
    #Creates other player season dataset
    other_data <- all_players_norm %>%
      filter(Player != input$Player_Stats) %>%
      mutate(euc = 0)
    #for loop for euclidean distance
    for(i in 1:nrow(other_data)){
      euc <-  round(euc.dist(player_data[4:47], other_data[i, 4:47]),3)
      other_data[i, 48] = euc
    }
    other_data %>%
      arrange(euc) %>%
      select(Player, year, euc) %>%
      rename(Season = year) %>%
      head(20)%>%
      return()
  })

  output$table <- DT::renderDataTable({ 
    data_for_table()
  })
  
}
shinyApp(ui = ui, server = server)


