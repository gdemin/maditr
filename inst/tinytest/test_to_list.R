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
    to_list(iris, if(is.numeric(.x)) .x, skip_null = FALSE),
    c(as.list(iris[1:4]), list(Species = NULL))
)

expect_identical(
    iris %>% to_list(if(is.numeric(.x)) .x, skip_null = FALSE),
    c(as.list(iris[1:4]), list(Species = NULL))
)

expect_identical(
    to_list(iris, if(is.numeric(.x)) mean(.x) else uniqueN(.x)),
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



expect_identical(
    to_list((1:5)*10, (.index)),
    as.list(1:5)
)


expect_identical(
    to_list((1:5)*10, (.name)),
    as.list(rep("", 5))
)

expect_identical(
    to_list(iris, if(grepl("Sepal", .name)) .x),
    as.list(iris[,c("Sepal.Length", "Sepal.Width")])
)


########
cat("\nContext:","to_vec", "\n")
expect_identical(
    to_vec(iris, if(is.numeric(.x)) mean(.x) else uniqueN(.x)),
    c(colMeans(iris[1:4]), Species = uniqueN(iris$Species))
)


expect_identical(
    to_vec(iris, if(is.numeric(.x)) mean(.x) else uniqueN(.x), use.names = FALSE),
    unname(c(colMeans(iris[1:4]), Species = uniqueN(iris$Species)))
)

expect_identical(
    to_vec(iris, is.numeric),
    sapply(iris, is.numeric)
)

cat("\nContext:","to_df", "\n")

data("mtcars")

expect_identical(
    to_df(list(1:2, 2:3, 3:4)),
    data.table(V1 = 1:3, V2 = 2:4)
)
expect_identical(
    to_df(mtcars, list(var = .name, mean = mean(.x), sd = sd(.x))),
    data.table(var = names(mtcars), mean = colMeans(mtcars), sd = sapply(mtcars, sd))
)

expect_identical(
    to_df(mtcars, list(mean = mean(.x), sd = sd(.x)), idvalue = .name, idname = "var"),
    data.table(mean = colMeans(mtcars), sd = sapply(mtcars, sd), var = names(mtcars))
)

expect_identical(
    to_df(unname(as.list(mtcars)), list(mean = mean(.x), sd = sd(.x)), idvalue = .name, idname = "var"),
    data.table(mean = colMeans(mtcars), sd = sapply(mtcars, sd), var = "")
)

expect_identical(
    to_dfr(mtcars, list(mean = mean(.x), sd = sd(.x)), idvalue = .index),
    data.table(mean = colMeans(mtcars), sd = sapply(mtcars, sd), item_id = seq_along(mtcars))
)

vec = c("a", "b", "c")
expect_identical(
    to_dfr(vec, paste0(.x, 1:3), idvalue = .x),
    data.table(V1 = paste0(vec, 1),
               V2 = paste0(vec, 2),
               V3 = paste0(vec, 3),
               item_id = vec)
)



expect_identical(
    to_dfr(vec, paste0(.x, 1:3), idvalue = .x),
    data.table(V1 = paste0(vec, 1),
               V2 = paste0(vec, 2),
               V3 = paste0(vec, 3),
               item_id = vec)
)


expect_identical(
    to_dfr(vec, paste0(.x, 1:3), idvalue = paste0(.x, .index)),
    data.table(V1 = paste0(vec, 1),
               V2 = paste0(vec, 2),
               V3 = paste0(vec, 3),
               item_id = paste0(vec, 1:3))
)

expect_identical(
    to_dfc(mtcars, c( mean(.x), sd(.x))),
    as.data.table(rbind(colMeans(mtcars), sd = sapply(mtcars, sd)))
)

cat("\nContext:","to_list scoping", "\n")

my_fun = function(x) {
    d = 2
    to_vec(x, .x*d)
}

expect_equal(
    my_fun(1:3),
    2*(1:3)
)

cat("\nContext:","to_list nested", "\n")

a = c(a = 1, b = 2, c = 3)
b = c(aa = 10, bb = 20, cc = 30)

expect_identical(
    to_list(a,
            paste(.index, .name, .x, to_vec(b, paste(.index, .name, .x)))
    ),
    list(a = c("1 a 1 1 aa 10", "1 a 1 2 bb 20", "1 a 1 3 cc 30"),
         b = c("2 b 2 1 aa 10", "2 b 2 2 bb 20", "2 b 2 3 cc 30"),
         c = c("3 c 3 1 aa 10", "3 c 3 2 bb 20", "3 c 3 3 cc 30"))
)
if(getOption("covr", TRUE)){
    cat("\nContext:","progress_bars", "\n")
    expect_equal(
        to_list(1:10, identity, trace = TRUE),
        as.list(1:10)
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
            Sys.sleep(0.01)
            x^2
            }, trace = "pb"),
        as.list((1:100)^2)
    )
    expect_equal(
        to_list(1:100,{
            Sys.sleep(.01)
            .x^2
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
        to_vec(1:100, {
            Sys.sleep(.01)
            .x^2
        }, trace = TRUE, trace_step = 10),
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
