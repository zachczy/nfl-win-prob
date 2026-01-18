wp_model <- readRDS("nfl-win-prob-model-v1.rds")

wp_df <- readRDS("data/wp_df_baseline_2020s.rds")

# getting possible levels for variables
down_state_levels <- levels(wp_df$home_down_state) # home_dn1, away_dn4

# ydstogo is negative if away team has the ball

# game_seconds_remaining should be 0 to 3600

# yardline_100 should be 0 to 100

# home timeouts should be 0 to 3

predict_current_wp <- function(home_score, away_score,
                               game_secs_remaining,
                               ydstogoal, down, ydstogo,
                               pos_team, home_team,
                               home_tos) {

  h_possession <- ifelse(pos_team == home_team, 1, 0)
  h_score_diff <- home_score - away_score
  h_dist_goal <- ifelse(h_possession == 1, ydstogoal, 100 - ydstogoal)
  h_timeouts <- home_tos
  h_ydstogo <- ifelse(h_possession == 1, ydstogo, -ydstogo)

  dn_string <- paste0(
                      ifelse(h_possession == 1, "home_", "away_"),
                      "dn", down)

  input_data <- data.frame(
    home_down_state = factor(dn_string, levels = down_state_levels),
    home_score_differential = h_score_diff,
    game_seconds_remaining = game_secs_remaining,
    yardline_100 = h_dist_goal,
    home_ydstogo = h_ydstogo,
    home_timeouts_remaining = as.factor(h_timeouts)
  )
  prob <- predict(wp_model, newdata = input_data, type = "response")
  return(prob)
}


predict_current_wp(home_score = 20, away_score = 20, game_secs_remaining = 30,
                   ydstogoal = 75, down = 1, ydstogo = 10,
                   pos_team = "ARI", home_team = "NE",
                   home_tos = 3)
