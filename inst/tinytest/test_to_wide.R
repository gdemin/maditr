cat("\nContext:", "to_long", "\n")

set.seed(45)
DT = data.table(
    i_1 = c(1:5, NA),
    i_2 = c(NA,6,7,8,9,10),
    f_1 = factor(sample(c(letters[1:3], NA), 6, TRUE)),
    f_2 = factor(c("z", "a", "x", "c", "x", "x"), ordered=TRUE),
    c_1 = sample(c(letters[1:3], NA), 6, TRUE),
    d_1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"),
    d_2 = as.Date(6:1, origin="2012-01-01"))
# add a couple of list cols
DT[, l_1 := DT[, list(c=list(rep(i_1, sample(5,1)))), by = i_1]$c]
DT[, l_2 := DT[, list(c=list(rep(c_1, sample(5,1)))), by = i_1]$c]

# id, measure as character/integer/numeric vectors
expect_identical(
    to_long(DT, f_1, keep = 1:2),
    melt(DT, id=1:2, measure="f_1")
)

expect_identical(
    to_long(DT, f_1, keep = c(i_1, i_2)),
    melt(DT, id=1:2, measure="f_1")
)

expect_identical(
    to_long(DT, f_1, keep = cols(i_1:i_2)),
    melt(DT, id=1:2, measure="f_1")
)

expect_identical(
    to_long(DT, f_1, keep = cols(i_1:i_2), names_factor = FALSE),
    melt(DT, id=1:2, measure="f_1", variable.factor = FALSE)
)

expect_identical(
    to_long(DT, f_1, keep = cols("i_{1:2}")),
    melt(DT, id=1:2, measure="f_1")
)

expect_identical(
    to_long(DT, f_1, keep = cols("^i_")),
    melt(DT, id=1:2, measure="f_1")
)

expect_identical(
    to_long(DT, f_1, keep = cols("^i_"), names_in = "var", values_in = "val"),
    melt(DT, id=1:2, measure="f_1", variable.name = "var", value.name = "val")
)



col_var = "^i_"
expect_identical(
    to_long(DT, 3, keep = cols(col_var)),
    melt(DT, id=1:2, measure="f_1")
)

expect_identical(
    to_long(DT, cols("^f_"), keep = cols("^i_"), value_factor = TRUE),
    melt(DT, id=1:2, measure=3:4, value.factor=TRUE) # 'value' is *ordered* factor
)

data("mtcars")
expect_identical(
    to_long(mtcars),
    suppressWarnings(melt(mtcars))
)

data("mtcars")
expect_identical(
    to_long(mtcars, keep = am),
    melt(mtcars, id.vars = "am")
)

data("mtcars")
expect_identical(
    to_long(mtcars, columns = c(am, vs, mpg)),
    melt(mtcars, measure.vars = c("am", "vs", "mpg"))
)


data("mtcars")
expect_identical(
    to_long(mtcars, columns = c(am, vs, mpg), keep = FALSE),
    melt(mtcars, measure.vars = c("am", "vs", "mpg"), id = integer(0))
)

# on na.rm=TRUE. NAs are removed efficiently, from within C
expect_identical(
    suppressWarnings(to_long(DT, keep = f_1, columns = c(i_1, i_2), drop_na = TRUE)),
    suppressWarnings(melt(DT, id="f_1", measure=c("i_1", "i_2"), na.rm=TRUE)) # remove NA
)

expect_identical(
    suppressWarnings(to_long(DT, keep = f_1, columns = c(i_1, i_2), drop_na = FALSE)),
    suppressWarnings(melt(DT, id="f_1", measure=c("i_1", "i_2"), na.rm = FALSE))
)

expect_identical(
    to_long(DT, keep=1:2, columns = list(cols("^f_"), cols("^d_")), value_factor=TRUE),
    melt(DT, id=1:2, measure=patterns("^f_", "^d_"), value.factor=TRUE)
)


cat("\nContext:", "to_wide", "\n")

data("ChickWeight")
names(ChickWeight) = tolower(names(ChickWeight))
DT = to_long(ChickWeight, keep=2:4) # calls melt.data.table

expect_identical(
    to_wide(DT, keep = time, fun = mean),
    dcast(DT, time ~ variable, fun=mean) # using partial matching of argument
)

expect_identical(
    to_wide(DT, keep = FALSE, fun = mean),
    dcast(DT, . ~ variable, fun=mean) # using partial matching of argument
)

expect_identical(
    to_wide(DT, keep = diet, fun = mean),
    dcast(DT, diet ~ variable, fun=mean)
)
expect_identical(
    to_wide(DT, keep = c(diet, chick), names_in = time, missing_comb = "all"),
    dcast(DT, diet+chick ~ time, drop=FALSE)
)
expect_identical(
    to_wide(DT, keep = c(diet, chick), names_in = time, missing_comb = "all", fill = 0),
    dcast(DT, diet+chick ~ time, drop=FALSE, fill=0)
)

expect_equal(
    to_wide(DT, chick, time, fun = mean),
    dcast(DT, chick ~ time, fun=mean)
)

DT <- data.table(v1 = c(1.1, 1.1, 1.1, 2.2, 2.2, 2.2),
                 v2 = factor(c(1L, 1L, 1L, 3L, 3L, 3L), levels=1:3),
                 v3 = factor(c(2L, 3L, 5L, 1L, 2L, 6L), levels=1:6),
                 v4 = c(3L, 2L, 2L, 5L, 4L, 3L))
# drop=TRUE

expect_error(to_wide(DT, c(v1, v2), v3, missing_comb = "none"))
expect_identical(
    to_wide(DT, c(v1, v2), v3, values_in = v4, missing_comb = "none"),
    dcast(DT, v1 + v2 ~ v3, value.var = "v4")  # default is drop=TRUE
)
expect_identical(
    to_wide(DT, c(v1, v2), v3, values_in = v4, missing_comb = "all"),
    dcast(DT, v1 + v2 ~ v3, value.var = "v4", drop=FALSE)          # all missing combinations of both LHS and RHS
)
expect_identical(
    to_wide(DT, c(v1, v2), v3, values_in = v4, missing_comb = "rows"),
    dcast(DT, v1 + v2 ~ v3, value.var = "v4", drop=c(FALSE, TRUE)) # all missing combinations of only LHS
)
expect_identical(
    to_wide(DT, c(v1, v2), v3, values_in = v4, missing_comb = "columns"),
    dcast(DT, v1 + v2 ~ v3, value.var = "v4", drop=c(TRUE, FALSE)) # all missing combinations of only RHS
)

# using . and ...
DT <- data.table(v1 = rep(1:2, each = 6),
                 v2 = rep(rep(1:3, 2), each = 2),
                 v3 = rep(1:2, 6),
                 v4 = rnorm(6))
expect_identical(
    to_wide(DT, names_in = v3, values_in = v4),
    dcast(DT, ... ~ v3, value.var = "v4") #same as v1 + v2 ~ v3, value.var = "v4"
)
expect_identical(
    to_wide(DT, cols(v1 %to% v3), names_in = FALSE, values_in = v4),
    dcast(DT, v1 + v2 + v3 ~ ., value.var = "v4")
)

## for each combination of (v1, v2), add up all values of v4
expect_identical(
    to_wide(DT, cols("^v(1|2)"), names_in = FALSE, values_in = v4, fun = sum),
    dcast(DT, v1 + v2 ~ ., value.var = "v4", fun.aggregate = sum)
)

expect_identical(
    to_wide(DT, cols("v{1:2}"), names_in = FALSE, values_in = v4, fun = sum),
    dcast(DT, v1 + v2 ~ ., value.var = "v4", fun.aggregate = sum)
)

my_keep = "v{1:2}"
my_names = FALSE
my_values = "v4"

expect_error(to_wide(DT, my_keep, names_in = FALSE, values_in = v4, fun = sum))


expect_identical(
    to_wide(DT, cols(my_keep), names_in = my_names, values_in = my_values, fun = sum),
    dcast(DT, v1 + v2 ~ ., value.var = "v4", fun.aggregate = sum)
)

# multiple value.var and multiple fun.aggregate
DT = data.table(x=sample(5,20,TRUE), y=sample(2,20,TRUE),
                z=sample(letters[1:2], 20,TRUE), d1 = runif(20), d2=1L)
# multiple value.var
expect_identical(
    to_wide(DT, c(x, y), z, c(d1, d2), fun = sum, fill = 0),
    dcast(DT, x + y ~ z, fun=sum, value.var=c("d1","d2"))
)
# multiple fun.aggregate
expect_identical(
    to_wide(DT, c(x, y), z, d1, fun = list(sum = sum, mean = mean), fill = NULL),
    dcast(DT, x + y ~ z, fun=list(sum, mean), value.var="d1")
)


my_fun = list(sum = sum, mean = mean)
expect_identical(
    to_wide(DT, c(x, y), z, d1, fun = my_fun),
    dcast(DT, x + y ~ z, fun=my_fun, value.var="d1", fill = NA)
)
# multiple fun.agg and value.var (all combinations)
expect_identical(
    to_wide(DT, c(x, y), z, c(d1, d2), fun = my_fun),
    dcast(DT, x + y ~ z, fun=my_fun, value.var=c("d1", "d2"), fill = NA)
)
# multiple fun.agg and value.var (one-to-one)
expect_identical(
    to_wide(DT, c(x, y), z, list(d1, d2), fun = my_fun),
    dcast(DT, x + y ~ z, fun=my_fun, value.var=list("d1", "d2"), fill = NA)
)
