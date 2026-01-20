library(tidyverse)
library(nflfastR)
library(here)
library(ggrepel)
options(scipen = 9999)

# loading dataset
train <- readRDS("data/train_data_2020s.rds")
test <- readRDS("data/test_data_2020s.rds")

# fitting baseline logistic regression
wp_model <- glm(
  home_win ~ home_score_differential * game_seconds_remaining +
    home_down_state * ydstogo +
    yardline_100 * home_down_state +
    home_timeouts_remaining * game_seconds_remaining,
  data = train,
  family = binomial()
)

saveRDS(wp_model, "nfl-win-prob-model-v1.rds")

# adding predictor of home win prob
test <- test %>%
  dplyr::mutate(
    pred_home_win_prob = predict(wp_model, newdata = test, type = "response")
  )

# computing brier score
brier <- mean(
  (test$pred_home_win_prob - as.numeric(
    as.character(test$home_win)
  ))^2
)

# building calibration table
calibration <- test %>%
  dplyr::mutate(
    bin = ntile(pred_home_win_prob, 20)
  ) %>%
  dplyr::group_by(bin) %>%
  dplyr::summarise(
    mean_pred = mean(pred_home_win_prob, na.rm = TRUE),
    mean_actual = mean(as.numeric(as.character(home_win)), na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# creating calibration plot
library(ggplot2)

# graph
simple_graph <- ggplot(calibration, aes(x = mean_pred, y = mean_actual)) +
  geom_point(size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    title = "Calibration Plot: Home Win Probability",
    x = "Average predicted probability",
    y = "Observed home win rate"
  ) +
  theme_minimal()

# histogram
simple_hist <- ggplot(test, aes(pred_home_win_prob)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Predicted Home Win Probabilities",
    x = "Predicted probability",
    y = "Count"
  ) +
  theme_minimal()


library(tidyverse)

# 1. Create the calibration data
calibration_df <- test %>%
  # Create bins of 5% (0.05)
  mutate(bin = round(pred_home_win_prob * 20) / 20) %>%
  group_by(bin) %>%
  summarise(
    # Convert factor/logical to numeric 0/1 for the mean
    actual_win_rate = mean(as.numeric(as.character(home_win)), na.rm = TRUE),
    count = n()
  )

# 2. Plotting
ggplot(calibration_df, aes(x = bin, y = actual_win_rate)) +
  # The points represent your model's performance in each bin
  geom_point(aes(size = count), color = "steelblue") +
  geom_smooth(
              method = "lm", se = FALSE,
              color = "#5cff95", linetype = "solid") +
  # The dashed line represents "Perfect Calibration"
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    title = "Calibration Plot: Home Win Probability",
    subtitle = "Points should follow the red dashed line",
    x = "Predicted Probability",
    y = "Actual Win Rate",
    size = "Number of Plays"
  ) +
  theme_minimal()
