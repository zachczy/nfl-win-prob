library(nflreadr)
library(dplyr)
library(ggplot2)
library(nflplotR)

wp_model <- readRDS("nfl-win-prob-model-v1.rds")
down_state_levels <- levels(wp_df$home_down_state) # home_dn1, away_dn4

pbp_2025 <- load_pbp(2025)

random_id <- (pbp_2025 %>% sample_n(1))$game_id
  
game_id_to_plot <- random_id
pbp_game <- load_pbp(2025) %>%
  filter(game_id == game_id_to_plot) %>%
  filter(!is.na(down))

game_curve_data <- pbp_game %>%
  mutate(
    home_score_differential = home_score - away_score,
    home_dist_to_goal =
      ifelse(posteam == home_team, yardline_100, 100 - yardline_100),
    yardline_100 = home_dist_to_goal, # to clarify for model
    home_timeouts_remaining = as.factor(home_timeouts_remaining),
    home_ydstogo = ifelse(posteam == home_team, ydstogo, -ydstogo),

    # Matching the trained factor levels
    home_down_state = factor(
      paste0(ifelse(posteam == home_team, "home_", "away_"), "dn", down),
      levels = down_state_levels
    )
  )

game_curve_data$wp <- predict(wp_model,
                              newdata = game_curve_data, type = "response")

# Plotting - Does not work for Overtime
ggplot(game_curve_data, aes(x = game_seconds_remaining, y = wp)) +
  geom_line(color = "steelblue", size = 1) +
  # Reverse X-axis so it reads 3600 (start) to 0 (end)
  scale_x_reverse(breaks = seq(0, 3600, 900),
                  labels = c("End", "Q4", "Q3", "Q2", "Start")) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  labs(
    title = paste("Win Probability Curve:", game_id_to_plot),
    x = "Time Remaining",
    y = "Home Win Probability"
  ) +
  theme_minimal()
