library(dplyr)
library(mosaic)

name <- "LeBron James"
select_year <- 2021
all_players <- read.csv("Data/all_player_data.csv") %>%
  select(-c("Tm", "Pos", "Rk", "G", "GS"))
all_players[is.na(all_players)] = 0
all_players <- all_players %>%
  group_by(year, Player) %>%
  summarise(
    across(1:44, mean)
  )

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
euc.dist(all_players[1, 3:46], all_players[2, 3:46])
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
all_players_norm <- cbind(all_players %>% select(year, Player), 
                          as.data.frame(lapply(all_players[3:46], min_max_norm)))

player_comparison <- function(data, player_name, year) {
  player_data <- data %>%
    filter(Player == name, year == year)
  other_data <- data %>%
    filter(Player != name) %>%
    rowwise() %>%
    mutate(euc = euc.dist(player_data[3:46], .[[3:46]]))
  other_data <-  other_data %>%
    sort(desc(euc))
  return(
    other_data[1, ]
  )
}

x <- player_comparison(all_players_norm, name, select_year)


player_data <- all_players_norm %>%
  filter(Player == name, year == select_year)
other_data <- all_players_norm %>%
  filter(Player != name)
other_data <- other_data %>%
  rowwise() %>%
  mutate(euc = euc.dist(player_data[3:46], other_data[., 3:46]))
other_data <-  other_data %>%
  sort(desc(euc))
other_data[1, ]
