cat("\nContext:","take_all", "\n")


data(iris)
data(mtcars)
dt_iris = as.data.table(iris)
expect_equal(
    take_all(iris, if(is.numeric(.x)) mean(.x)),
    dt_iris[,lapply(.SD, mean), .SDcols = -5]
)
expect_equal(
    take_all(iris, mean, by = Species),
    dt_iris[,lapply(.SD, mean), by = Species]
)

expect_equal(
    take_all(dt_iris, mean, sd, by = Species),
    dt_iris[,c(
        setNames(
        lapply(.SD, mean),
        paste0("mean_", colnames(.SD))
        ),
        setNames(
            lapply(.SD, sd),
            paste0("sd_", colnames(.SD))
        )), by = Species]
)


expect_equal(
    take_all(dt_iris, mean, sd, by = Species, suffix = TRUE),
    dt_iris[,c(
        setNames(
            lapply(.SD, mean),
            paste0(colnames(.SD), "_mean")
        ),
        setNames(
            lapply(.SD, sd),
            paste0(colnames(.SD), "_sd")
        )), by = Species]
)

expect_equal(
    take_all(iris, stat_ = mean, by = Species, suffix = FALSE),
    {
        res = dt_iris[,lapply(.SD, mean), by = Species]
        names(res)[-1] = paste0("stat_", names(res)[-1])
        res
    }
)

expect_equal(
    take_all(iris, stat_ = function(x) 2*mean(x), by = Species, suffix = FALSE),
    {
        res = dt_iris[,lapply(.SD, function(x) 2*mean(x)), by = Species]
        names(res)[-1] = paste0("stat_", names(res)[-1])
        res
    }
)


expect_equal(
    take_all(iris, "_mean" = mean, by = Species, i = Species!="setosa"),
    {
        res = dt_iris[Species!="setosa", lapply(.SD, mean), by = Species]
        names(res)[-1] = paste0(names(res)[-1], "_mean")
        res
    }
)

expect_equal(
    take_all(iris, "mean_" = mean, by = Species, suffix = FALSE, .SDcols = -(1:2)),
    {
        res = dt_iris[,lapply(.SD, mean), by = Species, .SDcols = -(1:2)]
        names(res)[-1] = paste0("mean_", names(res)[-1])
        res
    }
)

expect_equal(
    take_all(iris, "mean_" = mean, keyby = Species, suffix = FALSE, .SDcols = -(1:2)),
    {
        res = dt_iris[,lapply(.SD, mean), keyby = Species, .SDcols = -(1:2)]
        names(res)[-1] = paste0("mean_", names(res)[-1])
        res
    }
)
