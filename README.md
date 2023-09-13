# wranger: Weighted observations extension of ranger
wranger is an extension of the popular package ranger that adds support for weighted observations during the random forest and gradient boosting model training. This package allows you to assign different weights to individual observations, giving you more control over the influence of each data point on the model's learning process.

We would like to express our sincere gratitude to the creators and contributors of the original Ranger package, which served as the foundation for this modified version. Ranger, developed by Marvin N. Wright, is a powerful machine learning library for R that has significantly influenced our work. Our extension builds upon Ranger's robust features, adding support for weighted observations while maintaining the spirit of open-source collaboration. We are indebted to the Ranger community for their invaluable contributions to the field of machine learning. Please support the original package: https://github.com/imbs-hl/ranger

## Features

    Train Random Forest models with weighted observations.
    Assign weights to data points to emphasize or de-emphasize their importance.
    Currently implemented are Regression with splitrule Variance and Classification with splitrule Gini Impurity.

## Installation
### R version

To install the development version from GitHub using devtools, run

    devtools::install_github("imbs-hl/ranger")

R version >= 3.1 is required. With recent R versions, multithreading on Windows platforms should just work. If you compile yourself, the new RTools toolchain is required.

## Usage
For usage of the R version see ?ranger in R. Most importantly, see the Examples section. As a first example you could try

    ranger(Species ~ ., data = iris, obs.weights = rep(1, nrow(iris)))
The splitrule does not need to be specified. 
## References
- Wright, M. N. & Ziegler, A. (2017). ranger: A fast implementation of random forests for high dimensional data in C++ and R. J Stat Softw 77:1-17. https://doi.org/10.18637/jss.v077.i01.
