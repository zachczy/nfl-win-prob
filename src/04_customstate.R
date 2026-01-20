wp_model <- readRDS("nfl-win-prob-model-v1.rds")

wp_df <- readRDS("data/wp_df_baseline_2020s.rds")

pbp_2025 <- readRDS("data/2025_pbp_data.rds")

# getting possible levels for variables

down_levels <- levels(wp_df$home_down_state) # home_dn1, away_dn4

# game_seconds_remaining should be 0 to 3600

# yardline_100 should be 0 to 100

# home timeouts should be 0 to 3

predict_current_wp <- function(home_score, away_score,
                               game_secs_remaining,
                               ydstogoal, down, yardstogo,
                               pos_team, home_team,
                               home_tos) {
  if (pos_team == home_team) {
    h_possession <- 1
  } else {
    h_possession <- 0
  }
  h_score_diff <- home_score - away_score
  h_dist_goal <- ifelse(h_possession == 1, ydstogoal, 100 - ydstogoal)
  h_timeouts <- home_tos
  yds_to_go <- yardstogo

  dn_string <- paste0(
                      ifelse(h_possession == 1, "home_", "away_"),
                      "dn", down)

  input_data <- data.frame(
    home_down_state = factor(dn_string, levels = down_levels),
    home_score_differential = h_score_diff,
    game_seconds_remaining = game_secs_remaining,
    yardline_100 = h_dist_goal,
    ydstogo = yds_to_go,
    home_timeouts_remaining = as.factor(h_timeouts)
  )
  prob <- predict(wp_model, newdata = input_data, type = "response")
  return(prob)
}




add_zachs_wp <- function(model = wp_model,
                         down_state_levels = down_levels, id) {
  if (missing(id)) {
    id_to_plot <- (pbp_2025 %>% sample_n(1))$game_id
  } else {
    id_to_plot <- id
  }
  pbp_game <- pbp_2025 %>%
    filter(.data$game_id == id_to_plot)

  last_play <- pbp_game %>% slice_tail(n = 1)
  if (last_play$qtr == 5) {
    ot_seconds <- 600 - last_play$quarter_seconds_remaining
    total_seconds <- 3600 + ot_seconds
    qtr_breaks <- c(
      0, ot_seconds, ot_seconds + 900,
      ot_seconds + 1800, ot_seconds + 2700, total_seconds
    )
    qtr_labels <- c("Final", "OT", "Q4", "Q3", "Q2", "Q1")
    pbp_game <- pbp_game %>%
      mutate(
        game_seconds_remaining = ifelse(qtr < 5,
          .data$game_seconds_remaining + ot_seconds,
          .data$game_seconds_remaining
        )
      )
  } else {
    total_seconds <- 3600
    qtr_breaks <- c(0, 900, 1800, 2700, 3600)
    qtr_labels <- c("Final", "Q4", "Q3", "Q2", "Q1")
  }

  scrimmage_data <- pbp_game %>%
    filter(!is.na(.data$down)) %>%
    mutate(
      home_score_differential = .data$home_score - .data$away_score,
      home_dist_to_goal =
        ifelse(.data$posteam == .data$home_team,
          .data$yardline_100, 100 - .data$yardline_100
        ),
      yardline_100 = .data$home_dist_to_goal, # to clarify for model
      home_timeouts_remaining = as.factor(.data$home_timeouts_remaining),
      home_ydstogo = ifelse(.data$posteam == .data$home_team,
        .data$ydstogo, -.data$ydstogo
      ),

      # Matching the trained factor levels
      home_down_state = factor(
        paste0(
          ifelse(.data$posteam == .data$home_team, "home_", "away_"),
          "dn", .data$down
        ),
        levels = down_state_levels
      )
    )

  scrimmage_data$zachs_wp <- predict(wp_model,
    newdata =
      scrimmage_data, type = "response"
  )

  pbp_game <- pbp_game %>%
    left_join(
      scrimmage_data %>% select(play_id, zachs_wp),
      by = "play_id"
    )
  game_curve_data <- pbp_game %>%
    arrange(desc(game_seconds_remaining)) %>%
    fill(zachs_wp, .direction = "up")

  return(game_curve_data)
}

pbp_example <- add_zachs_wp(id = "2025_05_HOU_BAL")

see_game_state_and_wp <- function(pbp, secs_left) {
  # getting closest entry
  idx <- which.min(abs(pbp$game_seconds_remaining - secs_left))
  row <- pbp[idx, ]

  wp_diff <- row$wp - row$zachs_wp

  cat("Game ID:", row$game_id, "\n")
  cat("Home Team:", row$home_team, "\n")
  cat("nflfastR WP:", row$wp, "\n")
  cat("Zach's WP:", row$zachs_wp, "\n")
  cat("WP Diff:", row$wp - row$zachs_wp, "\n")
  if (wp_diff > 0.3) {
    cat("===== WARNING: Substantial wp diff =====", "\n")
  } else {}
  cat("Time Left:", row$game_seconds_remaining, "\n")
  cat("Score:", row$total_home_score, "-", row$total_away_score, "\n")
  cat("Score Diff:", row$score_differential, "\n")
  cat("Possession", row$posteam, "\n")
  cat("Down:", row$down, "and", row$ydstogo, "\n")
  cat("Yardline:", row$yardline_100, "\n")
  cat("Timeouts Remaining:", row$home_timeouts_remaining, "\n")
}

see_game_state_and_wp(pbp = pbp_example, secs_left = 3200)

predict_current_wp(home_score = 3, away_score = 0, game_secs_remaining = 3400,
                   ydstogoal = 75, down = 1, yardstogo = 10,
                   pos_team = "ARI", home_team = "NE",
                   home_tos = 3)
