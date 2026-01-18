library(tidyverse)
library(nflfastR)
library(here)
library(ggrepel)
options(scipen = 9999)

# loading pbp data from the 2020s
pbp <- load_pbp(2020:2025)

# creating a dataframe with select variables
wp_df <- pbp %>%
  select(
    game_id, # unique game identifier
    posteam_type,
    home_team,
    away_team,
    qtr,
    game_seconds_remaining,
    score_differential,
    down,
    ydstogo,
    yardline_100,
    result
  ) # this is the most basic set of conditions we can use.

# mutating "result" into a boolean "home win" variable
wp_df <- wp_df %>%
  dplyr::mutate(home_win = dplyr::case_when(
    result > 0 ~ 1,
    result < 0 ~ 0,
    TRUE ~ NA_integer_
  ))

# mutating "posteam_type" into a home possession bool variable
wp_df <- wp_df %>%
  dplyr::mutate(home_possession = as.integer(posteam_type == "home"))

# dropping rows where necessary info is missing
wp_df <- wp_df %>%
  dplyr::filter(
    !is.na(home_possession),
    !is.na(down),
    !is.na(ydstogo),
    !is.na(yardline_100),
    !is.na(game_seconds_remaining),
    !is.na(score_differential),
    !is.na(home_win)
  )

#saving dataset
saveRDS(wp_df, "data/wp_df_baseline_2020s.rds")
