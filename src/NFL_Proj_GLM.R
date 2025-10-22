# Load necessary libraries
library(dplyr)
library(tidyr)
library(glmnet)
library(ggplot2)
library(ggrepel)

# ----------

# Load datasets
passing <- read.csv("pro-football-reference-2010-2024-passing.csv", stringsAsFactors = TRUE)
passing25 <- read.csv("pro-football-reference-2025-passing.csv", stringsAsFactors = TRUE)

# ----------

# Function to clean & prepare dataset
clean_passing_data <- function(df) {
  df <- df %>%
    rename(
      colNo = X,
      rank = Rk,
      playerName = Player,
      age = Age,
      team = Team,
      position = Pos,
      gamesPlayed = G,
      gamesStarted = GS,
      qbRecord = QBrec,
      passCompletions = Cmp,
      passAttempts = Att,
      compPercent = Cmp.,
      yards = Yds,
      passingTD = TD,
      percentTD = TD.,
      interceptions = Int,
      percentInt = Int.,
      firstDowns = X1D,
      passingSuccessRate = Succ.,
      longPass = Lng,
      yrdsPerPass = Y.A,
      adjYrdsPerPass = AY.A,
      ydsPerPassComp = Y.C,
      yrdsPerGame = Y.G,
      passerRating = Rate,
      qbRatingESPN = QBR,
      sacked = Sk,
      yrdsLostSack = Yds.1,
      sackAttPassPercent = Sk.,
      netYrdsPerPass = NY.A,
      adjNetYrdsPerPassAtt = ANY.A,
      comeback = X4QC,
      gameWinningDrive = GWD,
      awards = Awards,
      season = Year
    ) %>%
    # Filter for QBs only
    filter(position == "QB") %>%
    mutate(
      playerName = as.character(playerName),
      team = as.character(team),
      position = as.character(position),
      qbRecord = as.character(qbRecord),
      # Create binary Pro Bowl column
      isProBowl = grepl("PB", awards)
    ) %>%
    # Separate qbRecord into wins, losses, ties
    separate(qbRecord, into = c("wins", "losses", "ties"), sep = "-", convert = TRUE) %>%
    # Convert Pro Bowl logical to factor with fixed levels
    mutate(isProBowl = factor(isProBowl, levels = c(FALSE, TRUE)))
  
  return(df)
}

# Clean datasets
passing <- clean_passing_data(passing)
passing25 <- clean_passing_data(passing25)

# ----------

# Prepare train, test, and prediction sets

train_set <- passing %>%
  filter(season >= 2010 & season <= 2023) %>%
  # Remove columns not needed in model matrix & drop NA rows
  select(-playerName, -team, -awards, -rank, -position) %>%
  drop_na()

test_set <- passing %>%
  filter(season == 2024) %>%
  select(-playerName, -team, -awards, -rank, -position) %>%
  drop_na()

predict_set <- passing25 %>%
  filter(season == 2025) %>%
  select(-playerName, -team, -awards, -rank, -position) %>%
  drop_na()

# Confirm no NA rows remain
cat("Train rows:", nrow(train_set), "\n")
cat("Test rows:", nrow(test_set), "\n")
cat("Predict rows:", nrow(predict_set), "\n")

# ----------

# Build model matrices for glmnet (exclude intercept column)

x_train <- model.matrix(isProBowl ~ ., data = train_set)[, -1]
y_train <- as.numeric(train_set$isProBowl) - 1

x_test <- model.matrix(isProBowl ~ ., data = test_set)[, -1]
y_test <- as.numeric(test_set$isProBowl) - 1

x_predict <- model.matrix(~ ., predict_set %>% select(-isProBowl))[, -1]

# Verify dimension match
stopifnot(nrow(x_train) == length(y_train))
stopifnot(nrow(x_test) == length(y_test))

# ----------

# Train LASSO logistic regression

set.seed(123)
cv_fit <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)

best_lambda <- cv_fit$lambda.min
cat("Best lambda:", best_lambda, "\n")

# ----------

# Evaluate on test data (2024)

pred_probs_test <- predict(cv_fit, s = best_lambda, newx = x_test, type = "response")
pred_class_test <- ifelse(pred_probs_test > 0.5, TRUE, FALSE)

conf_matrix <- table(Predicted = pred_class_test, Actual = test_set$isProBowl)
print(conf_matrix)

accuracy <- mean(pred_class_test == test_set$isProBowl)
cat("Test accuracy:", accuracy, "\n")

# ----------

# Predict for 2025

pred_probs_2025 <- predict(cv_fit, s = best_lambda, newx = x_predict, type = "response")
pred_class_2025 <- ifelse(pred_probs_2025 > 0.5, TRUE, FALSE)

# Add predictions to 2025 dataset with player names for plotting

# First, recreate the cleaned 2025 prediction data (same used before creating x_predict)
predict_data_cleaned <- predict_set %>%
  drop_na()

# Then add predictions to the corresponding cleaned data
results_2025 <- passing25 %>%
  filter(season == 2025) %>%
  semi_join(predict_data_cleaned, by = intersect(names(passing25), names(predict_data_cleaned))) %>%
  mutate(
    proBowlPrediction = pred_class_2025,
    proBowlProbability = as.vector(pred_probs_2025)
  )


# ----------

# Plot 2025 results

ggplot(results_2025, aes(x = yards, y = passingTD)) +
  geom_point(aes(color = proBowlPrediction, size = proBowlProbability), alpha = 0.75) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray50") +
  geom_text_repel(
    data = results_2025 %>% filter(proBowlPrediction == TRUE),
    aes(label = playerName),
    size = 3.5,
    box.padding = 0.3,
    max.overlaps = 20
  ) +
  scale_color_manual(values = c("TRUE" = "firebrick", "FALSE" = "gray70")) +
  scale_size_continuous(range = c(2, 6)) +
  labs(
    title = "2025 Predicted Pro Bowl QBs",
    subtitle = "Passing Yards vs Passing Touchdowns",
    x = "Passing Yards",
    y = "Passing Touchdowns",
    color = "Predicted Pro Bowl?",
    size = "Prediction Probability"
  ) +
  theme_minimal()


# Get top 5 QBs by predicted Pro Bowl probability
top5_qbs <- results_2025 %>%
  arrange(desc(proBowlProbability)) %>%
  slice_head(n = 5)

# Plot with automatic labeling for top 5
ggplot(results_2025, aes(x = yards, y = passingTD)) +
  geom_point(aes(color = proBowlPrediction, size = proBowlProbability), alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "gray50", linetype = "dashed") +
  geom_text_repel(
    data = top5_qbs,
    aes(label = playerName),
    size = 4,
    box.padding = 0.4,
    max.overlaps = 20
  ) +
  scale_color_manual(values = c("TRUE" = "firebrick", "FALSE" = "gray70")) +
  scale_size_continuous(range = c(2, 6)) +
  labs(
    title = "2025 Predicted Pro Bowl QBs (Top 5 Highlighted)",
    subtitle = "Top 5 QBs by Pro Bowl probability labeled",
    x = "Passing Yards",
    y = "Passing Touchdowns",
    color = "Predicted Pro Bowl?",
    size = "Prediction Probability"
  ) +
  theme_minimal()


# Set threshold level
threshold <- 0.08

# Add prediction class based on threshold
results_2025 <- results_2025 %>%
  mutate(proBowlPrediction = proBowlProbability > threshold)

# Plot
ggplot(results_2025, aes(x = yards, y = passingTD)) +
  geom_point(aes(color = proBowlPrediction, size = proBowlProbability), alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "gray50", linetype = "dashed") +
  geom_text_repel(
    data = results_2025 %>% filter(proBowlPrediction == TRUE),
    aes(label = playerName),
    size = 3.5,
    box.padding = 0.3,
    max.overlaps = 20
  ) +
  scale_color_manual(values = c("TRUE" = "firebrick", "FALSE" = "gray70")) +
  scale_size_continuous(range = c(2, 6)) +
  labs(
    title = paste0("2025 Predicted Pro Bowl QBs (Threshold = ", threshold, ")"),
    subtitle = "Highlighting QBs with high Pro Bowl probability",
    x = "Passing Yards",
    y = "Passing Touchdowns",
    color = "Pro Bowl Prediction",
    size = "Prediction Probability"
  ) +
  theme_minimal()
