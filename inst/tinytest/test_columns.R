cat("\nContext: columns\n")

mt_dt = as.data.table(mtcars)
res1 = columns(mt_dt, -cyl, -am)
res3 = mt_dt[, -c(2,9), with = FALSE]
expect_identical(res1, res3)

res1 = columns(mtcars, cyl:wt, am)
res3 = mtcars[, c(2:6, 9)]
expect_identical(res1, res3)

mt_dt = as.data.table(mtcars)
res1 = columns(mt_dt, -(cyl:wt), -am)
res3 = mt_dt[, -c(2:6, 9), with = FALSE]
expect_identical(res1, res3)

data(iris)
dt_iris = as.data.table(iris)
res1 = columns(dt_iris, Species, "^Sepal")
expect_identical(res1, dt_iris[,.(Species, Sepal.Length, Sepal.Width)])
my_sepal = "^Sepal"
res1 = columns(dt_iris, Species, my_sepal)
expect_identical(res1, dt_iris[,.(Species, Sepal.Length, Sepal.Width)])

res1 = columns(dt_iris, Species, "^.")
expect_identical(res1, dt_iris[,c(5, 1:4)])

res1 = columns(dt_iris, Species, "^Sepal", "Width$")
expect_identical(res1, dt_iris[,c(5, 1:2, 4)])

res1 = columns(dt_iris, -"^Sepal", -"Width$")
expect_identical(res1, dt_iris[,c(3,5)])

res1 = columns(dt_iris,  "^.+\\.")
expect_identical(res1, dt_iris[,c(1:4)])

res1 = columns(dt_iris,  Species, Species)
expect_identical(res1, dt_iris[,c(5)])

res1 = columns(dt_iris,  "Species")
expect_identical(res1, dt_iris[,c(5)])

etab = data.frame(a = 1:2, b = 3:4)
class(etab) = c("etable", class(etab))
res = etab[,2, drop = FALSE]
expect_identical(res, columns(etab, "b"))

mt_dt = as.data.table(mtcars)

expect_identical(
    columns(mt_dt, mpg:hp, carb),
    columns(mt_dt, mpg %to% hp, carb)
)


expect_identical(
    columns(mtcars, mpg:hp, carb),
    columns(mtcars, mpg %to% hp, carb)
)
###
dims = c("Width", "Length")
expect_identical(
    columns(iris, "Petal.{dims}"),
    iris[, paste0("Petal.",dims)]
)


expect_identical(
    columns(iris, -"Petal.{dims}"),
    iris[, -(3:4)]
)
###
my_columns = function(my_dims){
    columns(iris, "Petal.{my_dims}")
}

expect_identical(
    my_columns(dims),
    iris[, paste0("Petal.",dims)]
)

###
my_columns = function(my_dims){
    columns(iris, -"Petal.{my_dims}")
}

expect_identical(
    my_columns(dims),
    iris[, -(3:4)]
)

###

my_columns = function(my_dims){
    columns(iris, "Petal.{dims}")
}

expect_identical(
    my_columns(NULL),
    iris[, paste0("Petal.",dims)]
)


my_vars = c("Petal.Length", "Petal.Width")
expect_identical(
    columns(iris, my_vars, Species),
    iris[,3:5]
)

expect_identical(
    columns(iris, -my_vars, -Species),
    iris[,-(3:5)]
)

expect_identical(
    columns(iris, 5-1),
    iris[,4, drop = FALSE]
)


expect_error(
    columns(iris, -my_vars, Species)
)
