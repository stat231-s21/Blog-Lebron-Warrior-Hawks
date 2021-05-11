library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(lubridate)
library(bslib)
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
    title = "Player Table",
    
    sidebarLayout(
      ## user selects players to display Data
      sidebarPanel(
        selectizeInput(inputId = "Player_Stats"
                       , label = "Choose a player:"
                       , choices = player_choices
                       , selected = "LeBron James"
                       , multiple = FALSE),
        ## Added NBA Logo to Side Panel
        img(src = "nbalogo.jpeg", height = 290, width = 150)
      ),
      mainPanel(
        DT::dataTableOutput(outputId = "table")
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
    data <- all_players_norm %>%
      filter(Player != input$Player_Stats) %>%
      mutate(euc = 0)
    player <- all_players_norm %>%
      filter(Player == input$Player_Stats & year == 2021)
    for(i in 1:nrow(data)){
      euc <-  euc.dist(player[3:46], data[i, 3:46])
      data[i, 47] = euc
    }
    return(
      data
    )
  })
  output$table <- DT::renderDataTable({ 
    data_for_table()
  })
  
}
shinyApp(ui = ui, server = server)


