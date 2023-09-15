
library(wranger)


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
    expect_equal(model1$predictions, model2$predictions)
    expect_equal(model1$prediction.error, model2$prediction.error)
    expect_equal(model1$r.squared, model2$r.squared)
    expect_equal(pred1$predictions, pred2$predictions)
  }
  print("Test erfolgreich")
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

    model2 = ranger(sch.wide ~ ., data = dtrain, num.trees = 5000, obs.weights = rep(1, nrow(dtrain)), seed = 1)
    pred2 = predict(model2, dtest)
    expect_equal(model1$predictions, model2$predictions)
    expect_equal(model1$prediction.error, model2$prediction.error)
    expect_equal(model1$r.squared, model2$r.squared)
    expect_equal(pred1$predictions, pred2$predictions)
  }
  print("Test erfolgreich")
})

