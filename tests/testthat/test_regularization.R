library(wranger)
library(survival)
context("ranger_regularization")

n <- 50
p <- 4
dat_reg <- data.frame(y = rnorm(n), x = replicate(p, runif(n)))
dat_class <- data.frame(y = factor(rbinom(n, 1, .5)), x = replicate(p, runif(n)))
dat_surv <- data.frame(time = runif(n), status = rbinom(n, 1, .5), x = replicate(p, runif(n)))

get_num_splitvars <- function(rf) {
  all_splitvars <- do.call(c, lapply(1:rf$num.trees, function(t) {
    treeInfo(rf, t)[, "splitvarID"]
  }))
  length(unique(all_splitvars[!is.na(all_splitvars)]))
}

test_that("same results with 1 and p regularization coefficients, regression", {
  seed <- runif(1 , 0, .Machine$integer.max)
  set.seed(seed)
  rf1 <- ranger(y ~ ., dat_reg, num.trees = 5, num.threads = 1,
                regularization.factor = .1)
  set.seed(seed)
  rf2 <- ranger(y ~ ., dat_reg, num.trees = 5, num.threads = 1,
                regularization.factor = rep(.1, p))
  expect_equal(rf1$prediction.error, rf2$prediction.error)
})

test_that("same results with 1 and p regularization coefficients, classification", {
  seed <- runif(1 , 0, .Machine$integer.max)
  set.seed(seed)
  rf1 <- ranger(y ~ ., dat_class, num.trees = 5, num.threads = 1,
                regularization.factor = .1)
  set.seed(seed)
  rf2 <- ranger(y ~ ., dat_class, num.trees = 5, num.threads = 1,
                regularization.factor = rep(.1, p))
  expect_equal(rf1$prediction.error, rf2$prediction.error)
})


# Regression
test_that("Fewer variables used with regularization, regression", {
  rf_noreg <- ranger(y ~ ., dat_reg, num.trees = 5, min.node.size = 10, mtry = 4)
  rf_reg <- ranger(y ~ ., dat_reg, num.trees = 5, min.node.size = 10, mtry = 4, num.threads = 1,
                  regularization.factor = .0001, regularization.usedepth = TRUE)
  expect_lt(get_num_splitvars(rf_reg),
            get_num_splitvars(rf_noreg))
})



# Classification
test_that("Fewer variables used with regularization, classification", {
  rf_noreg <- ranger(y ~ ., dat_class, num.trees = 5, min.node.size = 10, mtry = 4)
  rf_reg <- ranger(y ~ ., dat_class, num.trees = 5, min.node.size = 10, mtry = 4, num.threads = 1,
                   regularization.factor = .0001, regularization.usedepth = TRUE)
  expect_lt(get_num_splitvars(rf_reg),
            get_num_splitvars(rf_noreg))
})


# Probability
test_that("Fewer variables used with regularization, probability", {
  rf_noreg <- ranger(y ~ ., dat_class, num.trees = 5, min.node.size = 10, mtry = 4, classification = TRUE)
  rf_reg <- ranger(y ~ ., dat_class, num.trees = 5, min.node.size = 10, mtry = 4, classification = TRUE, num.threads = 1,
                   regularization.factor = .0001, regularization.usedepth = TRUE)
  expect_lt(get_num_splitvars(rf_reg),
            get_num_splitvars(rf_noreg))
})



