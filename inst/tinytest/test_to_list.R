cat("\nContext:","to_list", "\n")

expect_identical(
    to_list(1:3),
    as.list(1:3)
)

data(iris)
expect_identical(
    to_list(iris, if(is.numeric(.value)) .value),
    as.list(iris[1:4])
)


expect_identical(
    to_list(iris, if(is.numeric(.value)) .value, skip_null = FALSE),
    c(as.list(iris[1:4]), list(Species = NULL))
)


expect_identical(
    to_list(iris, if(is.numeric(.x)) .x, skip_null = FALSE),
    c(as.list(iris[1:4]), list(Species = NULL))
)

expect_identical(
    to_list(iris, if(is.numeric(.value)) mean(.value) else uniqueN(.value)),
    c(as.list(colMeans(iris[1:4])), list(Species = uniqueN(iris$Species)))
)

expect_identical(
    to_list(1:5, .index^2 + .index),
    as.list((1:5)^2 + 1:5)

)


expect_identical(
    to_list((1:5)*100, sqrt),
    lapply((1:5)*100, sqrt)
)

expect_error(
    to_list((1:5)*10, .index)
)

expect_identical(
    to_list((1:5)*10, (.index)),
    as.list(1:5)
)


expect_identical(
    to_list((1:5)*10, .name),
    as.list(rep("", 5))
)

expect_identical(
    to_list(iris, if(grepl("Sepal", .name)) .value),
    as.list(iris[,c("Sepal.Length", "Sepal.Width")])
)


########
cat("\nContext:","to_vec", "\n")
expect_identical(
    to_vec(iris, if(is.numeric(.value)) mean(.value) else uniqueN(.value)),
    c(colMeans(iris[1:4]), Species = uniqueN(iris$Species))
)


expect_identical(
    to_vec(iris, if(is.numeric(.value)) mean(.value) else uniqueN(.value), use.names = FALSE),
    unname(c(colMeans(iris[1:4]), Species = uniqueN(iris$Species)))
)

expect_identical(
    to_vec(iris, is.numeric),
    sapply(iris, is.numeric)
)

cat("\nContext:","to_df", "\n")

data("mtcars")
expect_identical(
    to_df(mtcars, list(var = .name, mean = mean(.x), sd = sd(.x))),
    data.table(var = names(mtcars), mean = colMeans(mtcars), sd = sapply(mtcars, sd))
)


expect_identical(
    to_dfr(mtcars, list(mean = mean(.x), sd = sd(.x)), idcol = "var"),
    data.table(var = names(mtcars), mean = colMeans(mtcars), sd = sapply(mtcars, sd))
)

expect_identical(
    to_dfc(mtcars, c( mean(.x), sd(.x))),
    as.data.table(rbind(colMeans(mtcars), sd = sapply(mtcars, sd)))
)
