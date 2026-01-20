library(nflreadr)
library(dplyr)
library(ggplot2)
library(nflplotR)
library(tidyr)
library(ggtext)
library(scales)

wp_df <- readRDS("data/wp_df_baseline_2020s.rds")
wp_model <- readRDS("nfl-win-prob-model-v2.rds")
pbp_2025 <- readRDS("data/2025_pbp_data.rds")


teams_colors_logos <- nflreadr::load_teams()
# initializing game_curve_data
# game_curve_data <- pbp_2025 %>%
#   sample_n(1) %>%
#   filter(!is.na(down)) %>%
#   mutate(
#     home_score_differential = home_score - away_score,
#     home_dist_to_goal =
#       ifelse(posteam == home_team, yardline_100, 100 - yardline_100),
#     yardline_100 = home_dist_to_goal, # to clarify for model
#     home_timeouts_remaining = as.factor(home_timeouts_remaining),

#     # Matching the trained factor levels
#     home_down_state = factor(
#       paste0(ifelse(posteam == home_team, "home_", "away_"), "dn", down),
#       levels = down_levels
#     )
#   )


plot_wp_curve <- function(model = wp_model, id) {
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

  pbp_game <- pbp_game %>%
    dplyr::mutate(h_possession = (posteam_type == "home"))

  scrimmage_data <- pbp_game %>%
    filter(!is.na(down)) %>%
    mutate(
      home_score_differential = home_score - away_score,
      yardline_100 =  # <-- CHANGE THIS
        ifelse(posteam == home_team,
          yardline_100, 100 - yardline_100
        ),
      # Remove this line: yardline_100 = home_dist_to_goal,
      home_timeouts_remaining = as.factor(home_timeouts_remaining),
      home_ydstogo = ifelse(posteam == home_team,
        ydstogo, -ydstogo
      ),
      down = as.factor(down),
      home_possession = as.numeric(h_possession)
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
  # getting week, colors and logos
  week <- (game_curve_data %>% slice_tail(n = 1))$week

  teams_colors_logos <- nflreadr::load_teams()
  home_team_abbr <- game_curve_data$home_team[1]
  away_team_abbr <- game_curve_data$away_team[1]

  home_logo_url <- teams_colors_logos %>%
    filter(team_abbr == home_team_abbr) %>%
    pull(team_logo_espn)

  winning_team_abbr = game_curve_data %>%
    slice(1) %>%
    summarize(winner = ifelse(.data$result > 0, home_team_abbr,
      away_team_abbr
    )) %>%
    pull(.data$winner)

  winning_team_color <- teams_colors_logos %>%
    filter(.data$team_abbr == winning_team_abbr) %>%
    pull(.data$team_color)

  winning_team_second_color <- teams_colors_logos %>%
    filter(team_abbr == winning_team_abbr) %>%
    pull(team_color2)


  logo_data <- data.frame(
    x = c(3350, 3350),
    y = c(0.95, 0.05),
    team = c(away_team_abbr, home_team_abbr)
  )



  game_curve_data$winner <- winning_team_abbr

  # determining scoring plays #####################################
  scoring_plays <- pbp_game %>%
    filter(.data$sp == 1) %>%
    mutate(
      # Check if the NEXT play is an XP or 2PT by the same team
      next_play_type = lead(.data$play_type),
      next_play_points = lead(.data$sp),
      next_play_2pt = lead(.data$two_point_conv_result),
      # Create the display label
      score_label = case_when(
        (touchdown == 1 &
          next_play_type == "extra_point" & next_play_points == 1 ~ "+7"),
        (touchdown == 1 &
          next_play_2pt == "success" ~ "+8"),
        (touchdown == 1 &
          next_play_2pt == "return" ~ "+4"),
        (touchdown == 1 ~ "+6"), # Failed PAT or end of game
        (play_type == "field_goal" ~ "+3"),
        (safety == 1 ~ "+2"),
        TRUE ~ NA_character_ # This hides the standalone XP/2PT rows
      )
    ) %>%
    mutate(y_pos = zachs_wp) %>%
    filter(!is.na(.data$score_label))
  scoring_plays <- scoring_plays %>%
    mutate(scoring_team = case_when(
      !is.na(td_team) & td_team != posteam ~ td_team,
      is.na(td_team) & play_type == "safety" ~ defteam, # Catch safeties
      TRUE ~ posteam
    ))
  # plotting
  ggplot(game_curve_data, aes(x = .data$game_seconds_remaining, y = zachs_wp)) +
    # bottom team fill
    geom_ribbon(aes(ymin = 0, ymax = zachs_wp, fill = home_team_abbr),
      alpha = 0.5,
      inherit.aes = TRUE
    ) +
    # top team fill
    geom_ribbon(aes(ymax = 1, ymin = zachs_wp, fill = away_team_abbr),
      alpha = 0.5,
      inherit.aes = TRUE
    ) +
    geom_line(color = winning_team_color, size = 1.4) +
    nflplotR::scale_fill_nfl(type = "primary") +
    nflplotR::scale_color_nfl(type = "secondary") +
    geom_line(aes(y = home_wp),
      color = "#000000", size = 1.4, linetype = "dashed"
    ) +
    geom_nfl_logos(
      data = logo_data, aes(x = .data$x, y = .data$y, team_abbr = .data$team),
      width = 0.12, inherit.aes = FALSE
    ) +
    scale_x_reverse(
      breaks = qtr_breaks, labels = qtr_labels,
      expand = expansion(mult = c(0.00, 0.00))
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.00, 0.00)),
      limits = c(0, 1),
      oob = scales::squish
    ) +
    # geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.8) +

    # labels and theme
    labs(
      title = paste(
        "Win Probability Curve:",
        away_team_abbr, "at", home_team_abbr, "- Week", week
      ),
      x = "",
      y = "",
      subtitle = "zach's prediction: shaded\nnflfastr prediction: dashed",
    ) +
    coord_cartesian(ylim = c(0, 1), expand = FALSE, clip = "off") +
    theme_minimal() +
    theme(
      plot.margin = margin(t = 20, r = 40, b = 10, l = 40),
      plot.background = element_rect(fill = "#777777", color = NA),
      # axis.line = element_line(color = "gray10"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      panel.border = element_rect(color = "#282828", fill = NA, linewidth = 2),
      plot.title = element_text(
        size = 28, face = "bold", hjust = 0.5, margin = margin(b = 10),
      ),
      plot.subtitle = element_text(
        size = 20, hjust = 0.5,
        color = "grey30", margin = margin(b = 15)
      )
    ) +
    # adding scoring plays
    geom_nfl_logos(
      data = scoring_plays,
      aes(
        x = game_seconds_remaining,
        y = y_pos, team_abbr = scoring_team
      ),
      width = 0.06, inherit.aes = FALSE
    ) +
    geom_text(
      data = scoring_plays,
      aes(
        x = game_seconds_remaining,
        y = y_pos, label = score_label
      ),
      hjust = 2,
      size = 7,
      fontface = "bold",
      color = "black",
      inherit.aes = FALSE
    )
}
plot_wp_curve(id = "2025_15_CLE_CHI") # This is crazy!

plot_wp_curve()



ggsave(
  filename = "nfl_win_probability.png",
  plot = last_plot(), # Saves the most recent plot displayed
  width = 12, # Horizontal length
  height = 6, # Vertical height (creating a 2:1 wide look)
  units = "in", # Inches
  dpi = 300 # High resolution for crisp logos and textbg = "#404040"
)
