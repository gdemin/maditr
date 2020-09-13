cat("\nContext:","let_all", "\n")

scale2 = function(x) c(scale(x))
data(iris)
data(mtcars)
dt_iris = as.data.table(iris)
expect_equal(
    let_all(iris, if(is.numeric(.x)) scale2(.x)),
    dt_iris[, names(dt_iris[,-5]) := lapply(.SD, scale2), .SDcols = -5]
)

dt_iris = as.data.table(iris)
expect_equal(
    let_all(iris, scaled = if(is.numeric(.x)) scale2(.x)),
    dt_iris[, paste0(names(dt_iris[,-5]), "_scaled") := lapply(.SD, scale2), .SDcols = -5]
)

dt_iris = as.data.table(iris)
expect_equal(
    let_all(iris, scaled = if(is.numeric(.x)) c(scale2(.x)), by = Species),
    dt_iris[, paste0(names(dt_iris[,-5]), "_scaled") := lapply(.SD, function(x) c(scale2(x))), by = Species]
)

dt_iris = as.data.table(iris)
res = dt_iris[, paste0(names(dt_iris[,-5]), "_scaled") := lapply(.SD, function(x) c(scale2(x))), by = Species]
dt_iris = as.data.table(iris)
expect_equal(
    let_all(dt_iris, scaled = if(is.numeric(.x)) c(scale2(.x)), by = Species),
    res
)

dt_iris = as.data.table(iris)
res = dt_iris[, paste0(c("Sepal.Width", "Petal.Length"), "_scaled") := lapply(.SD[,c("Sepal.Width", "Petal.Length")],
                                                                              function(x) c(scale2(x))), by = Species]
dt_iris = as.data.table(iris)
expect_equal(
    let_all(dt_iris, scaled = if(.index %in% 2:3) c(scale2(.x)), by = Species),
    res
)

dt_iris = as.data.table(iris)
res = dt_iris[, paste0(c("Sepal.Width", "Petal.Length"), "_scaled") := lapply(.SD[,c("Sepal.Width", "Petal.Length")],
                                                                              function(x) c(scale2(x))), keyby = Species]
dt_iris = as.data.table(iris)
expect_equal(
    let_all(dt_iris, scaled = if(.index %in% 2:3) c(scale2(.x)), keyby = Species),
    res
)


dt_iris = as.data.table(iris)
expect_equal(
    let_all(iris[FALSE, ], scaled = if(is.numeric(.x)) scale2(.x)),
    dt_iris[FALSE, ][,paste0(names(dt_iris[,-5]), "_scaled") := lapply(.SD, scale2), .SDcols = -5]
)

dt_iris = as.data.table(iris)
expect_equal(
    let_all(iris, scaled = if(is.numeric(.x)) scale2(.x), uniqueN, by = Species),
    dt_iris[, c(paste0(names(dt_iris[,-5]), "_scaled"), paste0(names(dt_iris[,-5]), "_uniqueN")) := c(lapply(.SD, scale2), lapply(.SD, uniqueN)),
            by = Species]
)

dt_iris = as.data.table(iris)
expect_equal(
    let_all(iris, scaled = if(.index %in% 2:3) scale2(.x), scaled = uniqueN, by = Species),
    dt_iris[, c(paste0(c("Sepal.Width", "Petal.Length"), "_scaled"), paste0(names(dt_iris[,-5]), "_scaled.1")) := c(lapply(.SD[,2:3, with = TRUE], scale2), lapply(.SD, uniqueN)),
            by = Species]
)

dt_iris = as.data.table(iris)
expect_equal(
    let_all(iris, scaled = if(.index %in% 2:3) scale2(.x), scaled = uniqueN),
    dt_iris[, c(paste0(c("Sepal.Width", "Petal.Length"), "_scaled"), paste0(names(dt_iris), "_scaled.1")) := c(lapply(.SD[,2:3, with = TRUE], scale2), lapply(.SD, uniqueN))]
)

dt_iris = as.data.table(iris)
expect_equal(
    let_all(dt_iris, scaled = if(is.numeric(.x)) scale2(.x), uniqueN, by = Species),
    dt_iris
)

dt_iris = as.data.table(iris)
expect_equal(
    let_all(iris, scaled = if(is.numeric(.x)) scale2(.x), uniqueN),
    dt_iris[, c(paste0(names(dt_iris[,-5]), "_scaled"), paste0(names(dt_iris), "_uniqueN")) := c(lapply(.SD[,-5], scale2), lapply(.SD, uniqueN))]
)

dt_iris = as.data.table(iris)
expect_equal(
    let_all(iris, scaled = if(is.numeric(.x)) scale2(.x), i = FALSE),
    dt_iris[FALSE, paste0(names(dt_iris[,-5]), "_scaled") := lapply(.SD, scale2), .SDcols = -5]
)

dt_iris = as.data.table(iris)
expect_equal(
    let_all(iris, mean = mean(.x), by = Species, suffix = FALSE),
    dt_iris[, paste0("mean_", names(dt_iris)[-5]) := lapply(.SD, mean), by = Species]
)

dt_iris = as.data.table(iris)
expect_equal(
    let_all(iris, mean = mean(.x), by = Species, sep = ""),
    dt_iris[, paste0(names(dt_iris)[-5], "mean") := lapply(.SD, mean), by = Species]
)


dt_iris = as.data.table(iris)
expect_equal(
    let_all(iris, mean, sd, length, by = Species, sep = "_"),
    dt_iris[, c(paste0(names(dt_iris)[-5], "_mean"), paste0(names(dt_iris)[-5], "_sd"), paste0(names(dt_iris)[-5], "_length")) :=
                c(lapply(.SD, mean), lapply(.SD, sd), lapply(.SD, length)), by = Species]
)

dt_iris = as.data.table(iris)
my_name = "N"
expect_equal(
    let_all(iris, mean, sd, (my_name) := length, by = Species, sep = "_"),
    dt_iris[, c(paste0(names(dt_iris)[-5], "_mean"), paste0(names(dt_iris)[-5], "_sd"), paste0(names(dt_iris)[-5], "_N")) :=
                c(lapply(.SD, mean), lapply(.SD, sd), lapply(.SD, length)), by = Species]
)

data(iris)
dt_iris = as.data.table(iris)
my_fun = function(x){
    if(is.numeric(x)){
        mean(x)
    } else {
        NULL
    }
}

expect_equal(
    let_all(iris, mean = my_fun),
    dt_iris[,paste0(names(dt_iris)[-5], "_mean") := lapply(.SD, mean), .SDcols = -"Species"]
)

dt_iris = as.data.table(iris)
expect_equal(
    let_all(iris, mean = function(x){
        if(is.numeric(x)){
            mean(x)
        } else {
            NULL
        }
    }),
    dt_iris[,paste0(names(dt_iris)[-5], "_mean") := lapply(.SD, mean), .SDcols = -"Species"]
)

dt_iris = as.data.table(iris)
expect_equal(
    let_all(iris, mean = mean, .SDcols = -"Species"),
    dt_iris[,paste0(names(dt_iris)[-5], "_mean") := lapply(.SD, mean), .SDcols = -"Species"]
)


#### with etable

etab = data.frame(a = 1:2, b = 3:4)
class(etab) = c("etable", class(etab))
res = etab
res$a_new = res$a + 1
res$b_new = res$b + 1

etab2 = let_all(etab, new = .x + 1)
expect_identical(etab2, res)

