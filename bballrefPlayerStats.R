library(tidyverse) 
library(rvest)
library(robotstxt)
library(janitor)

#Check If Webscraping is Allowed

paths_allowed("https://www.basketball-reference.com/leagues/")
df_player = data.frame()
for(i in 1982:2021){
  
  y = toString(i)
  reglink <- paste0("https://www.basketball-reference.com/leagues/NBA_",y,"_per_game.html")
  regtables <- reglink %>%
    read_html() %>%
    html_nodes("table")%>%
    html_table(fill = TRUE)
  regstats <- regtables[[1]] %>%
    filter(Player != "Player")
  
  advlink <- paste0("https://www.basketball-reference.com/leagues/NBA_",y,"_advanced.html")
  advtables <- advlink %>%
    read_html() %>%
    html_nodes("table")%>%
    html_table(fill = TRUE)
  
  advstats <- advtables[[1]]  %>%
    remove_empty("cols") %>%
    filter(!(str_detect(Player, "Player"))) %>%
    select(-c(Rk, Pos, Age, G, MP))
  
  df_years <- full_join(regstats, advstats, by = c("Player", "Tm")) %>%
    drop_na() %>%
    mutate(year = i)
  
  df_player <- rbind(df_player, df_years) %>%
    select(year, everything())
}

write_csv(df_player, "Data/all_player_data.csv")
