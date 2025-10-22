
# load necessary packages
library(dplyr)
library(ggplot2)
library(caret)
library(ggplot2)
library(reshape2)
library(randomForest)
library(ggrepel)
library(RColorBrewer)
library(tidyverse) 
library(GGally)
library(pROC)
library(glmnet)
library(lubridate)
library(rpart.plot)
library(scales)
library(viridis)
library(ROCR)


# -------------

# reading in datasets
passing <- read.csv("pro-football-reference-2010-2024-passing.csv", stringsAsFactors = TRUE)

# -------------

# summary passing
names(passing)
# renaming columns for precise understanding
# new name -> old name
passing <- passing %>% rename(
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
  adjNetYrdsPerPassAtt= ANY.A,
  comeback = X4QC, 
  gameWinningDrive = GWD,
  awards = Awards, 
  season = Year
)

# summary of dataset
summary(passing)
# preview of dataset
head(passing)

# -------------

# exploratory analysis

str(passing)
# playerName should be a string, team should be a string, position should be a string, and need to drop all
# positions that are not QB, qbRecord should be a string or we can split into wins loss and tie, 
# awards could be turned into binary columns
# changing playerName to character string
passing$playerName <- as.character(passing$playerName)
str(passing$playerName)
# changing team to character string
passing$team <- as.character(passing$team)
str(passing$team)
# changing position to character string
passing$position <- as.character(passing$position)
str(passing$position)
# checking all unique values in position column - only want QB
unique(passing$position)
# filtering data to only include QB positions
passing <- passing %>%
  filter(position == 'QB')
unique(passing$position)
# changing qbRecord to character string
passing$qbRecord <- as.character(passing$qbRecord)
# parse data and manipulate this column so there is a win tie and loss column
passing <- passing %>%
  separate(qbRecord, into = c("wins", "losses", 'ties'), sep = "-", convert = TRUE)
# create binary column from awards column where PB is either true or false
passing$isProBowl <- grepl("PB", passing$awards)
summary(passing$isProBowl)

# -------------

# visualizations

# looking at distribution of columms 
ggplot(passing, aes(x = yards)) + 
  geom_histogram(binwidth = 200, fill = 'violet', color = 'black') + 
  labs(title = "Distribution of Passing Yards", 
       x = 'Yards', y = 'Count')

ggplot(passing, aes(x = passerRating)) + 
  geom_histogram(binwidth = 5, fill = 'violet', color = 'black') + 
  labs(title = "Distribution of Passer Rating",
       x = "Passer Rating",
       y = "Count")

ggplot(passing, aes(x = interceptions)) + 
  geom_histogram(binwidth = 2, fill = 'violet', color = 'black') + 
  labs(title = "Distribution of Interceptions", 
       x = "Interceptions",
       y = "Count")

ggplot(passing, aes(x = isProBowl, y = passingTD, fill = isProBowl)) + 
  geom_boxplot() +
  labs(title = "Passing TDs by Pro Bowl Selection",
       x = 'Pro Bowl',
       y = "Passing TDs")

ggplot(passing, aes(x = isProBowl, y = passerRating, fill = isProBowl)) + 
  geom_boxplot() + 
  labs(title = "Passer Rating by Pro Bowl Selection", 
       x = 'Pro Bowl', 
       y = "Passer Rating")

ggplot(passing, aes(x = isProBowl, y = passerRating, fill = isProBowl)) + 
  geom_boxplot() + 
  facet_wrap(~season) +
  labs(title = "Passer Rating by Pro Bowl Selection", 
       x = 'Pro Bowl', 
       y = "Passer Rating")

numeric_vals <- passing %>%
  select(passingTD, interceptions, yards, passerRating, compPercent, wins)

ggpairs(numeric_vals)

ggplot(passing, aes(x = yards, y = passingTD)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  labs(title = "Passing TDs vs Yards", x = "Passing Yards", y = "Passing TDs") +
  theme_minimal()

ggplot(passing, aes(x = yards, y = passingTD, color = isProBowl)) +
  geom_point(alpha = 0.7) +
  labs(title = "TDs vs Yards (Colored by Pro Bowl)", x = "Yards", y = "TDs") +
  theme_minimal()

passing %>%
  group_by(season) %>%
  summarise(avgRating = mean(passerRating, na.rm = TRUE)) %>%
  ggplot(aes(x = season, y = avgRating)) +
  geom_line(color = "purple", size = 1.2) +
  geom_point(color = "purple") +
  labs(title = "Average Passer Rating by Season", x = "Season", y = "Avg Passer Rating") +
  theme_minimal()

# looking into previous pro bowlers
# Create a new label column: only Pro Bowlers get their name shown
passing <- passing %>%
  mutate(label = ifelse(isProBowl & season %in% c(2023, 2024), playerName, NA))

# plot with names of pro bowlers
ggplot(passing, aes(x = yards, y = passingTD)) +
  geom_point(aes(color = isProBowl), alpha = 0.7, size = 3) +
  # adding regression line
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  # only label pro bowlers
  geom_text_repel(aes(label = label), size = 3.5, max.overlaps = 20) +
  scale_color_manual(
    values = c("TRUE" = "firebrick", "FALSE" = "gray70"),
    labels = c("FALSE" = "Non-Pro Bowler", "TRUE" = "Pro Bowler")
  ) +
  labs(
    title = "Passing TDs vs Yards (Pro Bowlers Highlighted)",
    x = "Passing Yards",
    y = "Passing TDs",
    color = "Pro Bowl"
  ) +
  theme_minimal()


# using awards column let's predict whether a prospect will be PB nominated (pro bowl)

model_passing <- passing %>%
  select(-playerName, -team, -position, -awards, -label, -rank) %>%
  mutate(isProBowl = as.factor(isProBowl)) %>%
  drop_na()

# train and test data based on season
train_data <- model_passing %>% filter(season < 2024)
test_data <- model_passing %>% filter(season == 2024)

test_data$isProBowl <- relevel(test_data$isProBowl, ref = "TRUE")

# training a random forest
set.seed(121212)
rf_model = randomForest(isProBowl ~., data = train_data, importance = TRUE, ntree = 500)

pred_probs <- predict(rf_model, test_data, type = "prob")[,2]
preds_labels <- predict(rf_model, test_data)

confusionMatrix(preds_labels, test_data$isProBowl)

# ROC AUC
pred_obj <- prediction(pred_probs, test_data$isProBowl)
perf <- performance(pred_obj, "tpr", "fpr")
plot(perf, main = "ROC Curve")
abline(a = 0, b = 1, col = 'gray', lty = 2)

auc <- performance(pred_obj, "auc")@y.values[[1]]
cat("AUC:", auc, "\n")

# -----------

# testing on 2025 data 

# reading in datasets
passing25 <- read.csv("pro-football-reference-2025-passing.csv", stringsAsFactors = TRUE)

# -------------

# summary passing
names(passing25)
# renaming columns for precise understanding
# new name -> old name
passing25 <- passing25 %>% rename(
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
  adjNetYrdsPerPassAtt= ANY.A,
  comeback = X4QC, 
  gameWinningDrive = GWD,
  awards = Awards, 
  season = Year
)

# summary of dataset
summary(passing25)
# preview of dataset
head(passing25)

# -------------

# exploratory analysis

str(passing25)
# playerName should be a string, team should be a string, position should be a string, and need to drop all
# positions that are not QB, qbRecord should be a string or we can split into wins loss and tie, 
# awards could be turned into binary columns
# changing playerName to character string
passing25$playerName <- as.character(passing25$playerName)
str(passing25$playerName)
# changing team to character string
passing25$team <- as.character(passing25$team)
str(passing25$team)
# changing position to character string
passing25$position <- as.character(passing25$position)
str(passing25$position)
# checking all unique values in position column - only want QB
unique(passing25$position)
# filtering data to only include QB positions
passing25 <- passing25 %>%
  filter(position == 'QB')
unique(passing25$position)
# changing qbRecord to character string
passing25$qbRecord <- as.character(passing25$qbRecord)
# parse data and manipulate this column so there is a win tie and loss column
passing25 <- passing25 %>%
  separate(qbRecord, into = c("wins", "losses", 'ties'), sep = "-", convert = TRUE)
# create binary column from awards column where PB is either true or false
passing25$isProBowl <- grepl("PB", passing25$awards)
summary(passing25$isProBowl)



library(dplyr)
library(ggplot2)
library(ggrepel)
library(randomForest)

# Assume rf_model is already loaded in your environment

#### 2. Load and prepare 2025 QB data ####

# Filter just the 2025 season
qb_2025 <- passing25 %>% filter(season == 2025)

# Prepare dataset for prediction (remove identifiers and non-predictive columns)
qb_2025_model <- qb_2025 %>%
  select(-playerName, -team, -position, -awards, -isProBowl, -rank) %>%
  drop_na()

# Keep the matching rows with player info (names, etc.)
qb_2025_used <- qb_2025 %>%
  filter(row_number() %in% as.numeric(rownames(qb_2025_model)))

#### 3. Predict Pro Bowl selections ####

# Predict TRUE/FALSE class
preds_2025 <- predict(rf_model, qb_2025_model)

# Predict probabilities (assuming second column is 'TRUE' class)
probs_2025 <- predict(rf_model, qb_2025_model, type = "prob")[, 2]

# Combine predictions with player info

results_2025 <- qb_2025_used %>%
  mutate(
    proBowlPrediction = preds_2025,
    proBowlProb = probs_2025
  ) %>%
  arrange(desc(proBowlProb))

results_2025 <- results_2025 %>%
  mutate(
    proBowlPrediction = proBowlProb > 0.1  # adjust threshold as needed
  )


#### 4. Plot: Passing Yards vs Passing TDs with Pro Bowl labels ####

ggplot(results_2025, aes(x = yards, y = passingTD)) +
  
  geom_point(aes(color = proBowlPrediction, size = proBowlProb), alpha = 0.75) +
  
  geom_smooth(method = "lm", se = FALSE, color = "gray60", linetype = "dashed") +
  
  geom_text_repel(
    data = results_2025 %>% filter(proBowlPrediction == TRUE),
    aes(label = playerName),
    size = 3.5,
    box.padding = 0.4,
    max.overlaps = 20
  ) +
  
  scale_color_manual(values = c("TRUE" = "firebrick", "FALSE" = "gray70")) +
  scale_size_continuous(range = c(2, 6)) +
  
  labs(
    title = "Predicted Pro Bowl QBs (2025)",
    subtitle = "Based on current season stats — highlighted and labeled",
    x = "Passing Yards",
    y = "Passing Touchdowns",
    color = "Predicted Pro Bowl?",
    size = "Pro Bowl Probability"
  ) +
  theme_minimal()



results_2025 <- results_2025 %>%
  mutate(
    proBowlPrediction = proBowlProb > 0.07  # adjust threshold as needed
  )


#### 4. Plot: Passing Yards vs Passing TDs with Pro Bowl labels ####

ggplot(results_2025, aes(x = yards, y = passingTD)) +
  
  geom_point(aes(color = proBowlPrediction, size = proBowlProb), alpha = 0.75) +
  
  geom_smooth(method = "lm", se = FALSE, color = "gray60", linetype = "dashed") +
  
  geom_text_repel(
    data = results_2025 %>% filter(proBowlPrediction == TRUE),
    aes(label = playerName),
    size = 3.5,
    box.padding = 0.4,
    max.overlaps = 20
  ) +
  
  scale_color_manual(values = c("TRUE" = "firebrick", "FALSE" = "gray70")) +
  scale_size_continuous(range = c(2, 6)) +
  
  labs(
    title = "Predicted Pro Bowl QBs (2025)",
    subtitle = "Based on current season stats — highlighted and labeled",
    x = "Passing Yards",
    y = "Passing Touchdowns",
    color = "Predicted Pro Bowl?",
    size = "Pro Bowl Probability"
  ) +
  theme_minimal()


results_2025 <- results_2025 %>%
  mutate(
    proBowlPrediction = proBowlProb > 0.08  # adjust threshold as needed
  )


#### 4. Plot: Passing Yards vs Passing TDs with Pro Bowl labels ####

ggplot(results_2025, aes(x = yards, y = passingTD)) +
  
  geom_point(aes(color = proBowlPrediction, size = proBowlProb), alpha = 0.75) +
  
  geom_smooth(method = "lm", se = FALSE, color = "gray60", linetype = "dashed") +
  
  geom_text_repel(
    data = results_2025 %>% filter(proBowlPrediction == TRUE),
    aes(label = playerName),
    size = 3.5,
    box.padding = 0.4,
    max.overlaps = 20
  ) +
  
  scale_color_manual(values = c("TRUE" = "firebrick", "FALSE" = "gray70")) +
  scale_size_continuous(range = c(2, 6)) +
  
  labs(
    title = "Predicted Pro Bowl QBs (2025)",
    subtitle = "Based on current season stats — highlighted and labeled",
    x = "Passing Yards",
    y = "Passing Touchdowns",
    color = "Predicted Pro Bowl?",
    size = "Pro Bowl Probability"
  ) +
  theme_minimal()


# --------------


library(dplyr)
library(glmnet)
library(ggplot2)
library(ggrepel)

# 1. Prepare the datasets ---------------------------------------------------

# Train on 2010-2023
train_set <- passing %>%
  filter(season >= 2010 & season <= 2023) %>%
  select(-playerName, -team, -awards, -rank, -position) %>%
  filter(!is.na(isProBowl)) %>%
  mutate(isProBowl = factor(isProBowl, levels = c("FALSE", "TRUE")))

# Test on 2024
test_set <- passing %>%
  filter(season == 2024) %>%
  select(-playerName, -team, -awards, -rank, -position) %>%
  filter(!is.na(isProBowl)) %>%
  mutate(isProBowl = factor(isProBowl, levels = c("FALSE", "TRUE")))

# Predict for 2025
predict_set <- passing25 %>%
  filter(season == 2025) %>%
  select(-playerName, -team, -awards, -rank, -position)

# 2. Create model matrices ---------------------------------------------------

x_train <- model.matrix(isProBowl ~ ., train_set)[, -1]
y_train <- as.numeric(train_set$isProBowl) - 1  # Convert factor to 0/1 numeric

x_test <- model.matrix(isProBowl ~ ., test_set)[, -1]
y_test <- as.numeric(test_set$isProBowl) - 1

x_predict <- model.matrix(~ ., predict_set)[, -1]

# 3. Fit the LASSO logistic regression model ---------------------------------

set.seed(123)
cv_fit <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)

best_lambda <- cv_fit$lambda.min
cat("Best lambda selected by cross-validation:", best_lambda, "\n")

# 4. Evaluate on the 2024 test set -------------------------------------------

pred_probs_test <- predict(cv_fit, s = best_lambda, newx = x_test, type = "response")
pred_class_test <- ifelse(pred_probs_test > 0.5, TRUE, FALSE)

confusion_matrix <- table(Predicted = pred_class_test, Actual = test_set$isProBowl)
print(confusion_matrix)

accuracy <- mean(pred_class_test == test_set$isProBowl)
cat("Test accuracy:", accuracy, "\n")

# 5. Predict on 2025 ---------------------------------------------------------

pred_probs_2025 <- predict(cv_fit, s = best_lambda, newx = x_predict, type = "response")
pred_class_2025 <- ifelse(pred_probs_2025 > 0.5, TRUE, FALSE)

# Add predictions back to 2025 data for plotting, keep playerName for labels
results_2025 <- passing %>%
  filter(season == 2025) %>%
  mutate(
    proBowlPrediction = pred_class_2025,
    proBowlProbability = as.vector(pred_probs_2025)
  )

# 6. Plot results -------------------------------------------------------------

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

