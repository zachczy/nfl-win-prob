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
    posteam_type, # will switch this to boolean "home_possession"
    qtr,
    game_seconds_remaining,
    score_differential,
    down,
    ydstogo,
    yardline_100,
    home_timeouts_remaining,
    result
  ) # this is an informative set of conditions we can use.

# mutating "result" into a boolean "home win" variable
wp_df <- wp_df %>%
  dplyr::mutate(home_win = dplyr::case_when(
    result > 0 ~ 1,
    result < 0 ~ 0,
    TRUE ~ NA_integer_
  ))
wp_df$result <- NULL

# mutating "posteam_type" into a home possession bool variable
wp_df <- wp_df %>%
  dplyr::mutate(home_possession = as.factor(posteam_type == "home"))
wp_df$posteam_type <- NULL

# changing "score_differential" to s.diff. for home team rather than posteam
wp_df <- wp_df %>%
  mutate(
    home_score_differential = if_else(
      home_possession == TRUE,
      score_differential,
      -score_differential
    )
  )
wp_df$score_differential <- NULL

# changing yardline to home team perspective
# wp_df <- wp_df %>%
#   mutate(
#     yardline_100 = if_else(
#       home_possession == TRUE,
#       yardline_100,
#       100 - yardline_100
#     )
#   )

## changing state of down & ydstogo - who has the ball?

# down
wp_df <- wp_df %>%
  mutate(
    home_down_state =
      as.factor(
                paste0(
                       ifelse(
                              home_possession == TRUE,
                              "home_", "away_"), "dn",
                       down))
  )

wp_df$down <- NULL


# ydstogo - positive for home, neg for away
# wp_df <- wp_df %>%
#   mutate(
#     home_ydstogo = ifelse(home_possession == TRUE, ydstogo, -ydstogo)
#   )
# wp_df$ydstogo <- NULL

# changing col types
wp_df <- wp_df %>%
  mutate(across(
                c(home_down_state, qtr, home_timeouts_remaining, home_win),
                as.factor)
  )

# dropping rows where necessary info is missing
wp_df <- wp_df %>%
  dplyr::filter(
    !is.na(home_down_state),
    !is.na(ydstogo),
    !is.na(yardline_100),
    !is.na(game_seconds_remaining),
    !is.na(home_score_differential),
    !is.na(home_win),
    !is.na(home_timeouts_remaining)
  )

#saving dataset
saveRDS(wp_df, "data/wp_df_baseline_2020s.rds")
