cat("\nContext:","take_all", "\n")


data(iris)
data(mtcars)
dt_iris = as.data.table(iris)
expect_equal(
    take_all(iris, if(is.numeric(.j)) mean(.j)),
    dt_iris[,lapply(.SD, mean), .SDcols = -5]
)
expect_equal(
    take_all(iris, mean, by = Species),
    dt_iris[,lapply(.SD, mean), by = Species]
)

expect_equal(
    take_all(iris, mean, by = Species, prefix = "mean_"),
    {
        res = dt_iris[,lapply(.SD, mean), by = Species]
        names(res)[-1] = paste0("mean_", names(res)[-1])
        res
    }
)

expect_equal(
    take_all(iris, function(x) 2*mean(x), by = Species, prefix = "mean_"),
    {
        res = dt_iris[,lapply(.SD, function(x) 2*mean(x)), by = Species]
        names(res)[-1] = paste0("mean_", names(res)[-1])
        res
    }
)



new_names = gsub("\\.", "_", names(iris)[-5])

expect_equal(
    take_all(iris, mean, by = Species, prefix = function(x) gsub("\\.", "_", x)),
    {
        res = dt_iris[,lapply(.SD, mean), by = Species]
        names(res)[-1] = new_names
        res
    }
)

expect_equal(
    take_all(iris, mean, by = Species, prefix = function(x) gsub("\\.", "_", x), i = Species!="setosa"),
    {
        res = dt_iris[Species!="setosa", lapply(.SD, mean), by = Species]
        names(res)[-1] = new_names
        res
    }
)

expect_equal(
    take_all(iris, mean, by = Species, prefix = "mean_", .SDcols = -(1:2)),
    {
        res = dt_iris[,lapply(.SD, mean), by = Species, .SDcols = -(1:2)]
        names(res)[-1] = paste0("mean_", names(res)[-1])
        res
    }
)

expect_error(
    take_all(iris, mean, by = Species, prefix = function(x) gsub("\\.", "_", x), Species!="setosa")
)
