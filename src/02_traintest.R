library(tidyverse)
library(nflfastR)
library(here)
library(ggrepel)
options(scipen = 9999)

# loading dataset
wp_df <- readRDS("data/wp_df_baseline_2020s.rds")

# setting seed
set.seed(67)

# creating train/test dfs with one row per game
games <- wp_df %>% dplyr::distinct(game_id)

# adding 20 percent of games to test set
test_games <- games %>% dplyr::slice_sample(prop = 0.2)

# adding plays from non-test games to train dataset
train <- wp_df %>% dplyr::anti_join(test_games, by = "game_id")

# adding plays from test games to test dataset
test  <- wp_df %>% dplyr::inner_join(test_games, by = "game_id")

saveRDS(train, "data/train_data_2020s.rds")
saveRDS(test, "data/test_data_2020s.rds")


# View(train %>%
#     filter(!is.na(desc)) %>%
#     select(home_possession, yardline_100) %>%
#     head(20))

# View(train %>%
#     filter(
#         game_seconds_remaining > 3500,
#         abs(home_score_differential) <= 3
#     ) %>%
#     group_by(home_win) %>%
#     summarize(
#         mean_possession = mean(home_possession),
#         mean_yardline = mean(yardline_100),
#         mean_ydstogo = mean(ydstogo),
#         pct_down1 = mean(down == "1"),
#         pct_down2 = mean(down == "2"),
#         pct_down3 = mean(down == "3"),
#         pct_down4 = mean(down == "4"),
#         n = n()
#     ))
