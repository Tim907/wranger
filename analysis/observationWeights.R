
library(wranger)
library(ggplot2)


# Load stratified and population datasets
data("api", package = "survey")
trainIndex = match(apistrat$cds, apipop$cds)

apipop$growth = as.numeric(apipop$growth)

# Convert classification label to binary output
apipop$sch.wide = ifelse(apipop$sch.wide == "Yes", T, ifelse(apipop$sch.wide == "No", F, NA))

# Add indicator variable for missing target
apipop$missingTarget = as.numeric(is.na(apipop$target))


# Missing values in these following variables replaced with median,
# because only a few schools of the population contain missing values
apipop$target[is.na(apipop$target)] = median(apipop$target, na.rm = T)
apipop$mobility[is.na(apipop$mobility)] = median(apipop$mobility, na.rm = T)
apipop$avg.ed[is.na(apipop$avg.ed)] = median(apipop$avg.ed, na.rm = T)
apipop$full[is.na(apipop$full)] = median(apipop$full, na.rm = T)
apipop$emer[is.na(apipop$emer)] = median(apipop$emer, na.rm = T)
apipop$enroll[is.na(apipop$enroll)] = median(apipop$enroll, na.rm = T)
apipop$pcttest[is.na(apipop$pcttest)] = median(apipop$pcttest, na.rm = T)

# no missingMobility variable, because not a single missing in strat
# no missingAvg.ed variable, because not a single missing in strat
# no missingFull variable, because not a single missing in strat
# no missingEmer variable, because not a single missing in strat
# no missingEnroll variable, because not a single missing in strat
# no missingPcttest variable, because not a single missing in strat

# Removed year-round variable, because no school in pop contains a single "yes"
apipop$yr.rnd = NULL

# training matrix
X = model.frame(~ stype + cname + api99 + target + meals + ell + mobility +
                  pct.resp + not.hsg + hsg + some.col + col.grad + grad.sch + avg.ed + full +
                  emer + enroll + api.stu + pcttest + missingTarget, na.action = na.pass, apipop)
# X = model.frame(~ stype, na.action = na.pass, apipop)

# check columns containing NA
stopifnot(colSums(apply(X, 2, is.na)) == 0)

# One hot encode categorical variables for comparison between packages
X = model.matrix(~ . - 1, X)



# predicts population of Classification with weights
class_population = function() {

  dtrain = X[trainIndex,]
  dtest = X[-trainIndex,]
  labels = apipop$sch.wide
  n_oob = length(labels[-trainIndex])
  weights = apistrat$pw
  weights = weights / sum(weights)

  unloadNamespace("wranger")
  suppressMessages(library(ranger))
  # Original ranger without weights
  model1 = ranger::ranger(labels[trainIndex] ~ ., data = data.frame(dtrain), num.trees = 5000, seed = 1, probability = T)
  pred1 = predict(model1, data.frame(dtest))

  # Original ranger with case.weights
  model2 = ranger(labels[trainIndex] ~ ., data = data.frame(dtrain), num.trees = 5000, case.weights = weights, seed = 1, probability = T)
  pred2 = predict(model2, data.frame(dtest))


  # XGBOOST
  library("xgboost")
  m = ncol(dtrain)
  params = list(
    objective = "binary:logistic",
    learning_rate = 1,
    num_parallel_tree = 5000,
    subsample = 0.63,
    colsample_bynode = floor(sqrt(m)) / m,
    reg_lambda = 0,
    max_depth = 20,
    min_child_weight = 2
  )
  model3 = xgb.train(
    params,
    data = xgb.DMatrix(dtrain, label = labels[trainIndex], weight = nrow(dtrain) * weights),
    nrounds = 1,
    verbose = 0
  )
  pred3 = predict(model3, dtest)

  unloadNamespace("ranger")
  suppressMessages(library(wranger))
  # Modified ranger with obs.weights
  model4 = ranger(labels[trainIndex] ~ ., data = data.frame(dtrain), num.trees = 5000, obs.weights = weights, seed = 1, num.threads = 1)
  pred4 = predict(model4, data.frame(dtest))


  # Brier score of original ranger without weighting
  scoreRanger = sum((pred1$predictions[, 1] - (labels[-trainIndex] == T))^2, (pred1$predictions[, 2] - (labels[-trainIndex] == F))^2) / n_oob

  # Brier score of original ranger with case.weights
  scoreRangerWeight = sum((pred2$predictions[, 1] - (labels[-trainIndex] == T))^2, (pred2$predictions[, 2] - (labels[-trainIndex] == F))^2) / n_oob

  # Brier score of xgboost with weights
  scoreXgboost = sum((pred3 - (labels[-trainIndex] == T))^2, ((1-pred3) - (labels[-trainIndex] == F))^2) / n_oob

  # Brier score of modified ranger with obs.weights
  scoreObsWeight = sum((pred4$predictions[, 1] - (labels[-trainIndex] == T))^2, (pred4$predictions[, 2] - (labels[-trainIndex] == F))^2) / n_oob

  # Brier score for baseline model with arithmetic mean
  scoreBase = sum((mean(labels[trainIndex]) - (labels[-trainIndex] == T))^2, (mean(!labels[trainIndex]) - (labels[-trainIndex] == F))^2) / n_oob

  # Brier score for baseline model with weighted arithmetic mean
  scoreBaseWeight = sum((sum(labels[trainIndex] * weights) / sum(weights) - (labels[-trainIndex] == T))^2, (sum( (!labels[trainIndex]) * weights) / sum(weights) - (labels[-trainIndex] == F))^2) / n_oob


  data = data.frame(
    Model = c(
      "baseline mean",
      "baseline weighted mean",
      "ranger",
      "ranger with case.weights",
      "wranger with obs.weights",
      "xgboost with weights"
    ),
    Brier_Score = c(scoreBase, scoreBaseWeight, scoreRanger, scoreRangerWeight, scoreObsWeight, scoreXgboost)
  )
  # Create the barplot
  ggplot(data, aes(x = Model, y = Brier_Score, fill = Model)) +
    geom_bar(stat = "identity", alpha = 0.4) +
    labs(title = "Classification Model Comparison - Brier Score",
         x = "",
         y = "Brier Score") +
    theme_minimal() +
    scale_fill_manual(values = rep(c("blue", "green", "red", "purple", "orange", "pink"), each = 1))



  # Function to split the Brier Score by schooltype of given prediction matrix
  split_brier_by_schooltype = function(pred) {
    sqe = (pred$predictions[, 1] - (labels[-trainIndex] == T))^2 + (pred$predictions[, 2] - (labels[-trainIndex] == F))^2
    Brier_score = NULL
    for (stype in c("stypeE", "stypeH", "stypeM")) {
      Brier_score[stype] = sum(sqe[dtest[, stype] == 1]) / n_oob
    }
    data.frame(Brier_score)
  }

  # Split the Brier score by school type for original Ranger without weighting
  split_brier_by_schooltype(pred1)

  # Split the Brier score by school type for original Ranger with case.weights
  split_brier_by_schooltype(pred2)

  # Split the Brier score by school type for modified Ranger with obs.weights
  split_brier_by_schooltype(pred4)
}


# predicts population of Regression with weights
reg_population = function() {

  dtrain = X[trainIndex,]
  dtest = X[-trainIndex,]
  labels = apipop$growth
  weights = apistrat$pw
  weights = weights / sum(weights)

  unloadNamespace("wranger")
  suppressMessages(library(ranger))
  # Original ranger without weights
  model1 = ranger::ranger(labels[trainIndex] ~ ., data = data.frame(dtrain), num.trees = 5000, seed = 1)
  pred1 = predict(model1, data.frame(dtest))

  # Original ranger with case.weights
  model2 = ranger(labels[trainIndex] ~ ., data = data.frame(dtrain), num.trees = 5000, case.weights = weights, seed = 1)
  pred2 = predict(model2, data.frame(dtest))


  # XGBOOST
  library("xgboost")
  m = ncol(dtrain)
  params = list(
    objective = "reg:squarederror",
    learning_rate = 1,
    num_parallel_tree = 5000,
    subsample = 0.63,
    colsample_bynode = floor(sqrt(m)) / m,
    reg_lambda = 0,
    max_depth = 20,
    min_child_weight = 2
  )
  model3 = xgb.train(
    params,
    data = xgb.DMatrix(dtrain, label = labels[trainIndex], weight = nrow(dtrain) * weights),
    nrounds = 1,
    verbose = 0
  )
  pred3 = predict(model3, dtest)

  unloadNamespace("ranger")
  suppressMessages(library(wranger))
  # Modified ranger with obs.weights
  model4 = ranger(labels[trainIndex] ~ ., data = data.frame(dtrain), num.trees = 5000, obs.weights = weights, seed = 1, num.threads = 1)
  pred4 = predict(model4, data.frame(dtest))


  # Squared error of original ranger without weighting
  scoreRanger = sqrt(mean((pred1$predictions - labels[-trainIndex])^2))

  # Squared error of original ranger with case.weights
  scoreRangerWeight = sqrt(mean((pred2$predictions - labels[-trainIndex])^2))

  # Squared error of xgboost with weights
  scoreXgboost = sqrt(mean((pred3 - labels[-trainIndex])^2))

  # Squared error of modified ranger with obs.weights
  scoreObsWeight = sqrt(mean((pred4$predictions - labels[-trainIndex])^2))

  # Squared error for baseline model with arithmetic mean
  scoreBase = sqrt(mean((mean(labels[trainIndex]) - labels[-trainIndex])^2))

  # Brier score for baseline model with weighted arithmetic mean
  scoreBaseWeight = sqrt(mean((sum(labels[trainIndex] * weights) / sum(weights) - labels[-trainIndex])^2))

  data = data.frame(
    Model = c(
      "baseline mean",
      "baseline weighted mean",
      "ranger",
      "ranger with case.weights",
      "wranger with obs.weights",
      "xgboost with weights"
    ),
    Brier_Score = c(scoreBase, scoreBaseWeight, scoreRanger, scoreRangerWeight, scoreObsWeight, scoreXgboost)
  )
  # Create the barplot
  ggplot(data, aes(x = Model, y = Brier_Score, fill = Model)) +
    geom_bar(stat = "identity", alpha = 0.4) +
    labs(title = "Classification Model Comparison - Brier Score",
         x = "",
         y = "Brier Score") +
    theme_minimal() +
    scale_fill_manual(values = rep(c("blue", "green", "red", "purple", "orange", "pink"), each = 1))


  # Function to split the Root Mean Squared Error by schooltype of given prediction vector
  rmsqe_by_schooltype = function(pred) {
    sqe = (pred$predictions - labels[-trainIndex])^2
    RMSQE = NULL
    for (stype in c("stypeE", "stypeH", "stypeM")) {
      RMSQE[stype] = sqrt(mean(sqe[dtest[, stype] == 1]))
    }
    data.frame(RMSQE)
  }

  # RMSQE by school type for original Ranger without weighting
  rmsqe_by_schooltype(pred1)

  # RMSQE by school type for original Ranger with case.weights
  rmsqe_by_schooltype(pred2)

  # RMSQE by school type for modified Ranger with obs.weights
  rmsqe_by_schooltype(pred4)
}

