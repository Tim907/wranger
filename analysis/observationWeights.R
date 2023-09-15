
library(wranger)


test_that("weighting population classification", {

  data("api", package = "survey")
  trainIndex = match(apistrat$cds, apipop$cds)

  apipop$sch.wide = ifelse(apipop$sch.wide == "Yes", T, ifelse(apipop$sch.wide == "No", F, NA))
  apipop$missingTarget = as.numeric(is.na(apipop$target))
  apipop$target[is.na(apipop$target)] = median(apipop$target, na.rm = T)
  apipop$mobility[is.na(apipop$mobility)] = median(apipop$mobility, na.rm = T)
  apipop$avg.ed[is.na(apipop$avg.ed)] = median(apipop$avg.ed, na.rm = T)
  apipop$full[is.na(apipop$full)] = median(apipop$full, na.rm = T)
  apipop$emer[is.na(apipop$emer)] = median(apipop$emer, na.rm = T)
  apipop$enroll[is.na(apipop$enroll)] = median(apipop$enroll, na.rm = T)
  apipop$pcttest[is.na(apipop$pcttest)] = median(apipop$pcttest, na.rm = T)
  apipop$growth = as.numeric(apipop$growth)

  X = model.frame(~ stype + cname + api99 + target + meals + ell + mobility +
                    pct.resp + not.hsg + hsg + some.col + col.grad + grad.sch + avg.ed + full +
                    emer + enroll + api.stu + pcttest, na.action = na.pass, apipop)
  X = model.frame(~ stype, na.action = na.pass, apipop)
  # Removed year-round variable, because no school in pop contains a single "yes"

  # Following variables replaced with median, because only a few schools of the population contain missing values
  # no missingMobility variable, because not a single missing in strat
  # no missingAvg.ed variable, because not a single missing in strat
  # no missingFull variable, because not a single missing in strat
  # no missingEmer variable, because not a single missing in strat
  # no missingEnroll variable, because not a single missing in strat
  # no missingPcttest variable, because not a single missing in strat
  # check columns containing NA
  stopifnot(colSums(apply(X, 2, is.na)) == 0)

  # One hot encode categorical variables for comparison between packages
  X = model.matrix(~ . - 1, X)
  dtrain = X[trainIndex,]
  dtest = X[-trainIndex,]
  labels = apipop$sch.wide
  weights = apistrat$pw
  weights = weights / sum(weights)

  unloadNamespace("wranger")
  suppressMessages(library(ranger))
  model1 = ranger::ranger(labels[trainIndex] ~ ., data = data.frame(dtrain), num.trees = 5000, seed = 1, importance = "impurity")
  pred1 = predict(model1, data.frame(dtest))

  model4 = ranger(labels[trainIndex] ~ ., data = data.frame(dtrain), num.trees = 5000, case.weights = weights, seed = 1)
  pred4 = predict(model4, data.frame(dtest))

  unloadNamespace("ranger")
  suppressMessages(library(wranger))


  library("xgboost")
  m = ncol(dtrain)
  params <- list(
    objective = "binary:logistic",
    learning_rate = 1,
    num_parallel_tree = 5000,
    subsample = 0.63,
    colsample_bynode = floor(sqrt(m)) / m,
    reg_lambda = 0,
    max_depth = 20,
    min_child_weight = 2
  )
  model2 = xgb.train(
    params,
    data = xgb.DMatrix(dtrain, label = labels[trainIndex], weight = nrow(dtrain) * weights),
    nrounds = 1,
    verbose = 0
  )
  pred2 = predict(model2, dtest)

  model3 = ranger(labels[trainIndex] ~ ., data = data.frame(dtrain), num.trees = 5000, obs.weights = weights, seed = 1, num.threads = 1)
  pred3 = predict(model3, data.frame(dtest))

  2*mean(c((pred1$predictions[, 1] - (labels[-trainIndex] == T))^2, (pred1$predictions[, 2] - (labels[-trainIndex] == F))^2))
  2*mean(c((pred2 - (labels[-trainIndex] == T))^2, ((1-pred2) - (labels[-trainIndex] == F))^2))
  2*mean(c((pred3$predictions[, 1] - (labels[-trainIndex] == T))^2, (pred3$predictions[, 2] - (labels[-trainIndex] == F))^2))
  2*mean(c((pred4$predictions[, 1] - (labels[-trainIndex] == T))^2, (pred4$predictions[, 2] - (labels[-trainIndex] == F))^2))
  2*mean(c((mean(labels[trainIndex]) - (labels[-trainIndex] == T))^2, (mean(!labels[trainIndex]) - (labels[-trainIndex] == F))^2))

  temp = c((pred1$predictions[1] - (labels[-trainIndex] == T))^2, (pred1$predictions[2] - (labels[-trainIndex] == F))^2)
  for (stype in levels(dtest$stype)) {
    print(mean(temp[dtest$stype == stype]))
  }
})




test_that("weighting population regression", {

  # devtools::install_github("mnwright/simpleRF")
  data("api", package = "survey")
  trainIndex = match(apistrat$cds, apipop$cds)

  apipop$sch.wide = ifelse(apipop$sch.wide == "Yes", T, ifelse(apipop$sch.wide == "No", F, NA))
  apipop$missingTarget = as.numeric(is.na(apipop$target))
  apipop$target[is.na(apipop$target)] = median(apipop$target, na.rm = T)
  apipop$mobility[is.na(apipop$mobility)] = median(apipop$mobility, na.rm = T)
  apipop$avg.ed[is.na(apipop$avg.ed)] = median(apipop$avg.ed, na.rm = T)
  apipop$full[is.na(apipop$full)] = median(apipop$full, na.rm = T)
  apipop$emer[is.na(apipop$emer)] = median(apipop$emer, na.rm = T)
  apipop$enroll[is.na(apipop$enroll)] = median(apipop$enroll, na.rm = T)
  apipop$pcttest[is.na(apipop$pcttest)] = median(apipop$pcttest, na.rm = T)
  apipop$growth = as.numeric(apipop$growth)

  X = model.frame(~ stype + cname + api99 + target + meals + ell + mobility +
                    pct.resp + not.hsg + hsg + some.col + col.grad + grad.sch + avg.ed + full +
                    emer + enroll + api.stu + pcttest, na.action = na.pass, apipop)
  X = model.frame(~ stype, na.action = na.pass, apipop)

  # Removed year-round variable, because no school in pop contains a single "yes"

  # Following variables replaced with median, because only a few schools of the population contain missing values
  # no missingMobility variable, because not a single missing in strat
  # no missingAvg.ed variable, because not a single missing in strat
  # no missingFull variable, because not a single missing in strat
  # no missingEmer variable, because not a single missing in strat
  # no missingEnroll variable, because not a single missing in strat
  # no missingPcttest variable, because not a single missing in strat
  # check columns containing NA
  stopifnot(colSums(apply(X, 2, is.na)) == 0)

  # One hot encode categorical variables for comparison between packages
  X = model.matrix(~ . - 1, X)
  dtrain = X[trainIndex,]
  dtest = X[-trainIndex,]
  labels = apipop$growth
  weights = apistrat$pw
  weights = weights / sum(weights)

  unloadNamespace("wranger")
  suppressMessages(library(ranger))
  model1 = ranger::ranger(labels[trainIndex] ~ ., data = data.frame(dtrain), num.trees = 5000, seed = 1)
  pred1 = predict(model1, data.frame(dtest))

  model4 = ranger::ranger(labels[trainIndex] ~ ., data = data.frame(dtrain), num.trees = 5000, case.weights = weights, seed = 1)
  pred4 = predict(model4, data.frame(dtest))

  unloadNamespace("ranger")
  suppressMessages(library(wranger))

  library("xgboost")
  m = ncol(dtrain)
  params <- list(
    objective = "reg:squarederror",
    learning_rate = 1,
    num_parallel_tree = 5000,
    subsample = 0.63,
    colsample_bynode = floor(sqrt(m)) / m,
    reg_lambda = 0,
    max_depth = 20,
    min_child_weight = 2
  )
  model2 = xgb.train(
    params,
    data = xgb.DMatrix(dtrain, label = labels[trainIndex], weight = nrow(dtrain) * weights),
    nrounds = 1,
    verbose = 0
  )
  pred2 = predict(model2, dtest)

  model3 = ranger(labels[trainIndex] ~ ., data = data.frame(dtrain), num.trees = 5000, obs.weights = weights, seed = 1)
  pred3 = predict(model3, data.frame(dtest))

  sqrt(mean((pred1$predictions - labels[-trainIndex])^2))
  sqrt(mean((pred2 - labels[-trainIndex])^2))
  sqrt(mean((pred3$predictions - labels[-trainIndex])^2))
  sqrt(mean((pred4$predictions - labels[-trainIndex])^2))
  sqrt(mean((mean(labels[trainIndex]) - labels[-trainIndex])^2))

  temp = (pred3$predictions - labels[-trainIndex])^2
  temp = (pred1$predictions - labels[-trainIndex])^2
  for (stype in levels(dtest$stype)) {
    print(sqrt(mean(temp[dtest$stype == stype])))
  }
})


test_that("Weighting Regression", {

  data("api", package = "survey")
  apistrat$sch.wide = ifelse(apistrat$sch.wide == "Yes", T, ifelse(apistrat$sch.wide == "No", F, NA))
  apistrat$missingTarget = as.numeric(is.na(apistrat$target))
  apistrat$target[is.na(apistrat$target)] = median(apistrat$target, na.rm = T)
  apistrat$growth = as.numeric(apistrat$growth)

  X = model.frame(~ growth + stype + cname + api99 + target + meals + ell + yr.rnd + mobility + pct.resp + not.hsg + hsg + some.col + col.grad + grad.sch + avg.ed + full + emer + enroll + api.stu + pcttest, na.action = na.pass, apistrat)
  # no column containts NA
  stopifnot(colSums(apply(X, 2, is.na)) == 0)


  folds = 1:nrow(X)
  rmse_baseline = NULL
  wrmse_baseline = NULL
  rmse = NULL
  wrmse = NULL
  for (k in 1:max(folds)) {
    trainIndex = which(folds != k)
    dtrain = X[trainIndex,]
    dtest = X[-trainIndex,]

    unloadNamespace("wranger")
    suppressMessages(library(ranger))
    model1 = ranger::ranger(growth ~ ., data = dtrain, num.trees = 1000, seed = 1)
    pred1 = predict(model1, dtest)

    unloadNamespace("ranger")
    suppressMessages(library(wranger))
    model2 = ranger(growth ~ ., data = dtrain, num.trees = 1000, obs.weights = rep(1, nrow(dtrain)), seed = 1)
    pred2 = predict(model2, dtest)


    weights = apistrat$pw[trainIndex]
    weights = weights / sum(weights)
    model3 = ranger(growth ~ ., data = dtrain, num.trees = 1000, obs.weights = weights, seed = 1)
    pred3 = predict(model3, dtest)

    rmse_baseline[k] = (pred1$predictions - dtest$growth)^2
    rmse[k] = (pred3$predictions - dtest$growth)^2

    wrmse_baseline[k] = (pred1$predictions - dtest$growth)^2 * apistrat$pw[-trainIndex]
    wrmse[k] = (pred3$predictions - dtest$growth)^2 * apistrat$pw[-trainIndex]
  }

  sqrt(mean(rmse_baseline))
  sqrt(mean(rmse))

  sqrt(sum(wrmse_baseline / sum(apistrat$pw)))
  sqrt(sum(wrmse / sum(apistrat$pw)))
})






test_that("Weighting Classification", {

  data("api", package = "survey")
  apistrat$sch.wide = ifelse(apistrat$sch.wide == "Yes", T, ifelse(apistrat$sch.wide == "No", F, NA))
  apistrat$missingTarget = as.numeric(is.na(apistrat$target))
  apistrat$target[is.na(apistrat$target)] = median(apistrat$target, na.rm = T)
  apistrat$growth = as.numeric(apistrat$growth)


  X = model.frame(~ sch.wide + stype + cname + api99 + target + meals + ell + yr.rnd + mobility + pct.resp + not.hsg + hsg + some.col + col.grad + grad.sch + avg.ed + full + emer + enroll + api.stu + pcttest, na.action = na.pass, apistrat)
  # no column containts NA
  stopifnot(colSums(apply(X, 2, is.na)) == 0)


  folds = 1:nrow(X)
  brier_baseline = NULL
  wbrier_baseline = NULL
  brier = NULL
  wbrier = NULL
  for (k in 1:max(folds)) {
    trainIndex = which(folds != k)
    dtrain = X[trainIndex,]
    dtest = X[-trainIndex,]

    unloadNamespace("wranger")
    suppressMessages(library(ranger))
    model1 = ranger::ranger(sch.wide ~ ., data = dtrain, num.trees = 5000, seed = 1)
    pred1 = predict(model1, dtest)

    unloadNamespace("ranger")
    suppressMessages(library(wranger))


    weights = apistrat$pw[trainIndex]
    weights = weights / sum(weights)
    model3 = ranger(sch.wide ~ ., data = dtrain, num.trees = 5000, obs.weights = weights, seed = 1)
    pred3 = predict(model3, dtest)

    brier_baseline[k] = (pred1$predictions[1] - (dtest$sch.wide == T))^2 + (pred1$predictions[2] - (dtest$sch.wide == F))^2
    brier[k] = (pred3$predictions[1] - (dtest$sch.wide == T))^2 + (pred3$predictions[2] - (dtest$sch.wide == F))^2

    wbrier_baseline[k] = (pred1$predictions[1] - (dtest$sch.wide == T))^2 + (pred1$predictions[2] - (dtest$sch.wide == F))^2 * apistrat$pw[-trainIndex]
    wbrier[k] = (pred3$predictions[1] - (dtest$sch.wide == T))^2 + (pred3$predictions[2] - (dtest$sch.wide == F))^2 * apistrat$pw[-trainIndex]
  }

  mean(brier_baseline)
  mean(brier)

  sum(wbrier_baseline / sum(apistrat$pw))
  sum(wbrier / sum(apistrat$pw))
})

