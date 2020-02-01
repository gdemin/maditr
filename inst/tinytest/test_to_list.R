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



if(getOption("covr", FALSE)){
    cat("\nContext:","progress_bars", "\n")
    expect_equal(
        to_list(1:100, identity, trace = TRUE),
        as.list(1:100)
    )
    expect_equal(
        to_vec(setNames(1:26, letters), identity, trace = TRUE),
        setNames(1:26, letters)
    )
    expect_equal(
        to_vec(setNames(1:26, letters), identity, trace = quote(cat("current name:", .name, "\n"))),
        setNames(1:26, letters)
    )
    expect_equal(
        to_list(1:100, function(x) {
            Sys.sleep(.01)
            x^2
            }, trace = "pb"),
        as.list((1:100)^2)
    )
    expect_equal(
        to_vec(1:100, function(x) {
            Sys.sleep(.01)
            x^2
        }, trace = "pb", trace_step = 10),
        ((1:100)^2)
    )
    expect_equal(
        to_vec(1:100, function(x) {
            Sys.sleep(.01)
            x^2
        }, trace = quote(cat(.x, "^ 2 =", .res, "\n")), trace_step = 10),
        ((1:100)^2)
    )
    expect_equal(
        to_list(1:100, function(x) {
            Sys.sleep(.01)
            x^2
        }, trace = TRUE, trace_step = 10),
        as.list((1:100)^2)
    )
    expect_equal(
        to_dfc(mtcars, `+`, 1, trace = TRUE),
        as.data.table(mtcars + 1)

    )

    expect_equal(
        to_list(iris, grepl, pattern = "versi", trace = "pb"),
        lapply(iris, grepl, pattern = "versi")

    )
}
