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
  home_win ~ score_differential +
    game_seconds_remaining +
    down +
    ydstogo +
    yardline_100 +
    home_possession,
  data = train,
  family = binomial()
)

# adding predictor of home win prob
test <- test %>%
  dplyr::mutate(
    pred_home_win_prob = predict(wp_model, newdata = test, type = "response")
  )

# computing brier score
brier <- mean((test$pred_home_win_prob - test$home_win)^2)

# building calibration table
calibration <- test %>%
  dplyr::mutate(
    bin = ntile(pred_home_win_prob, 20)
  ) %>%
  dplyr::group_by(bin) %>%
  dplyr::summarise(
    mean_pred = mean(pred_home_win_prob, na.rm = TRUE),
    mean_actual = mean(home_win, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# creating calibration plot
library(ggplot2)

# graph
ggplot(calibration, aes(x = mean_pred, y = mean_actual)) +
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
ggplot(test, aes(pred_home_win_prob)) +
  geom_histogram(bins = 50) +
  labs(
    title = "Distribution of Predicted Home Win Probabilities",
    x = "Predicted probability",
    y = "Count"
  ) +
  theme_minimal()
