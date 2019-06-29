cat("\nContext:","cube, rollup and etc", "\n")
n = 24L
set.seed(25)
DT = data.table(
    color = sample(c("green","yellow","red"), n, TRUE),
    year = as.Date(sample(paste0(2011:2015,"-01-01"), n, TRUE)),
    status = as.factor(sample(c("removed","active","inactive","archived"), n, TRUE)),
    amount = sample(1:5, n, TRUE),
    value = sample(c(3, 3.5, 2.5, 2), n, TRUE)
)

DF = as.data.frame(DT)
# rollup
expect_equal(
rollup(DT, j = sum(value), by = c("color","year","status")),
rollup(DF, j = sum(value), by = c("color","year","status"))
)


# cube
expect_equal(
cube(DT, j = lapply(.SD, sum), by = c("color","year","status"), id=TRUE, .SDcols="value"),
cube(DF, j = lapply(.SD, sum), by = c("color","year","status"), id=TRUE, .SDcols="value")
)
# groupingsets
expect_equal(
groupingsets(DT, j = c(list(count=.N), lapply(.SD, sum)), by = c("color","year","status"),
             sets = list("color", c("year","status"), character()), id=TRUE),
groupingsets(DF, j = c(list(count=.N), lapply(.SD, sum)), by = c("color","year","status"),
             sets = list("color", c("year","status"), character()), id=TRUE)
)
