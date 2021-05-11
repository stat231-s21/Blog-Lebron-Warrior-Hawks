library(dplyr)
library(mosaic)

name <- "LeBron James"
select_year <- 2021
#all_players <- read.csv("Data/all_player_data.csv") %>%
  #select(-c("Tm", "Pos", "Rk", "G", "GS"))
#all_players[is.na(all_players)] = 0
#all_players <- all_players %>%
  #group_by(year, Player) %>%
  #summarise(
    #across(1:44, mean)
  #) %>%
  #filter(VORP > 1)

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
#all_players_norm <- cbind(all_players %>% select(year, Player), 
 #                         as.data.frame(lapply(all_players[3:46], min_max_norm)))

#write.csv(all_players_norm, "Data/all_players_norm.csv")
#x <- player_comparison(all_players_norm, name, select_year)
all_players_norm <- read.csv("Data/all_players_norm.csv")

player_data <- all_players_norm %>%
  filter(Player == name, year == select_year)
other_data <- all_players_norm %>%
  filter(Player != name) %>%
  mutate(euc = 0)

for(i in 1:nrow(other_data)){
  euc <-  euc.dist(player_data[4:47], other_data[i, 4:47])
  other_data[i, 48] = euc
}

other_data %>%
  arrange(euc) %>%
  select(Player, year, euc)
