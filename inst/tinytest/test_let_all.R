cat("\nContext:","let_all", "\n")

data(iris)
data(mtcars)
dt_iris = as.data.table(iris)
expect_equal(
    let_all(iris, if(is.numeric(.x)) scale(.x)),
    dt_iris[, names(dt_iris[,-5]) := lapply(.SD, scale), .SDcols = -5]
)
expect_equal(
    let_all(iris, mean_ = mean(.x), by = Species),
    dt_iris[,lapply(.SD, mean), by = Species]
)
