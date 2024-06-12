library(xgboost)
library(dplyr)
library(readr)

data <- read_csv("/mnt/data/MasterTrackmanData.csv")

# Selecting relevant columns
data <- data %>% select(TaggedPitchType, RelSpeed, InducedVertBreak, HorzBreak, SpinRate, RelHeight, RelSide, Extension)

# Normalize features
data <- data %>%
  mutate(NormalizedRelSpeed = (RelSpeed - mean(RelSpeed)) / sd(RelSpeed),
         # ... do this for other features
  )

# Calculate differences for breaking balls/off-speed pitches
# Assuming you have a way to identify fastballs, and have mean values for fastball metrics
data <- data %>%
  mutate(VeloDiff = ifelse(TaggedPitchType == "BreakingBall", RelSpeed - mean_fastball_speed, 0),
         # ... and similar for other difference features
  )

features <- as.matrix(data[, c("NormalizedRelSpeed", "InducedVertBreak", ...)])  # include all relevant features
labels <- data$TargetVariable  # replace with your actual target variable for Stuff+

dtrain <- xgb.DMatrix(data = features, label = labels)

params <- list(
  booster = "gbtree",
  objective = "reg:linear",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.5,
  colsample_bytree = 0.5
)

xgb_model <- xgboost(params = params, data = dtrain, nrounds = 100)

# If you have a test set
dtest <- xgb.DMatrix(data = test_features)
preds <- predict(xgb_model, dtest)
# calculate and print evaluation metrics

# Feature importance
importance_matrix <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance_matrix)

xgb.save(xgb_model, "stuffplus_model.model")
