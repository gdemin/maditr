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
    to_long(DT, f_1, keep = cols("^i_"), names_column = "var", values_column = "val"),
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
