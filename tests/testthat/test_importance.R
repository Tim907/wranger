## Tests for importance measures

library(wranger)
context("ranger_imp")
set.seed(123)

## Probability estimation
rg.imp.prob <- ranger(Species ~ ., data = iris,
                      num.trees = 5, importance = "impurity")
rg.perm.prob <- ranger(Species ~ ., data = iris,
                      num.trees = 5, importance = "permutation")
rg.scale.perm.prob <- ranger(Species ~ ., data = iris,
                             num.trees = 5, importance = "permutation", scale.permutation.importance = TRUE)

## Regression
rg.imp.regr <- ranger(Sepal.Length ~ ., data = iris,
                      num.trees = 5, importance = "impurity")
rg.perm.regr <- ranger(Sepal.Length ~ ., data = iris,
                       num.trees = 5, importance = "permutation")
rg.scale.perm.regr <- ranger(Sepal.Length ~ ., data = iris,
                             num.trees = 5, importance = "permutation", scale.permutation.importance = TRUE)


## Tests
test_that("importance measures work, probability", {
  expect_is(rg.imp.prob$variable.importance, "numeric")
  expect_is(rg.perm.prob$variable.importance, "numeric")
  expect_is(rg.scale.perm.prob$variable.importance, "numeric")
})

test_that("importance measures work, regression", {
  expect_is(rg.imp.regr$variable.importance, "numeric")
  expect_is(rg.perm.regr$variable.importance, "numeric")
  expect_is(rg.scale.perm.regr$variable.importance, "numeric")
})


# TODO observation.weights
# test_that("impurity importance is larger than 1", {
#   expect_gt(rg.imp.prob$variable.importance[1], 1)
#   expect_gt(rg.imp.regr$variable.importance[1], 1)
# })

test_that("unscaled importance is smaller than 1", {
  expect_lt(rg.perm.prob$variable.importance[1], 1)
  expect_lt(rg.perm.regr$variable.importance[1], 1)
})

test_that("scaled importance is larger than unscaled importance", {
  expect_gt(abs(rg.scale.perm.prob$variable.importance[1]), abs(rg.perm.prob$variable.importance[1]))
  expect_gt(abs(rg.scale.perm.regr$variable.importance[1]), abs(rg.perm.regr$variable.importance[1]))
})

test_that("error thrown if no importance in object", {
  rf <- ranger(Species ~ ., data = iris, num.trees = 5)
  expect_error(importance(rf), "No variable importance found. Please use 'importance' option when growing the forest.")
})


# TODO observation.weights
# test_that("Gini importance non-negative with class weights", {
#   rf <- ranger(Species ~ ., data = iris, class.weights = c(.2, .3, .8),
#                num.trees = 5, importance = "impurity")
#   expect_true(all(rf$variable.importance >= 0))
#
#   rf <- ranger(Species ~ ., data = iris, class.weights = c(.2, .3, .8),
#                num.trees = 5, importance = "impurity")
#   expect_true(all(rf$variable.importance >= 0))
# })
