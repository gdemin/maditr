cat("\nContext:","let_all", "\n")

data(iris)
data(mtcars)
dt_iris = as.data.table(iris)
expect_equal(
    let_all(iris, if(is.numeric(.x)) scale(.x)),
    dt_iris[, names(dt_iris[,-5]) := lapply(.SD, scale), .SDcols = -5]
)
dt_iris = as.data.table(iris)
expect_equal(
    let_all(iris, mean = mean(.x), by = Species),
    dt_iris[, paste0("mean_", names(dt_iris)[-5]) := lapply(.SD, mean), by = Species]
)

dt_iris = as.data.table(iris)
expect_equal(
    let_all(iris, mean = mean(.x), by = Species, sep = ""),
    dt_iris[, paste0("mean", names(dt_iris)[-5]) := lapply(.SD, mean), by = Species]
)
