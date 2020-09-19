cat("\nContext:", "selectors", "\n")

data(mtcars)
dt_mt = as.data.table(mtcars)
expect_identical(
    take(mtcars, sum(..(disp:drat)), by = am),
    dt_mt[,.('sum(..(disp:drat))' = sum(cbind(disp, hp, drat))), by = am]
)

expect_identical(
    take(mtcars, sum(columns(disp %to% drat)), by = am),
    dt_mt[,.('sum(columns(disp %to% drat))' = sum(cbind(disp, hp, drat))), by = am]
)


expect_identical(
    take(mtcars, my_sum = sum(columns(disp %to% drat)), by = am),
    dt_mt[,.(my_sum = sum(cbind(disp, hp, drat))), by = am]
)

expect_identical(
    take(mtcars, my_sum = sum(disp %to% drat), by = am),
    dt_mt[,.(my_sum = sum(cbind(disp, hp, drat))), by = am]
)

expect_identical(
    take(mtcars, my_sum = sum(columns("^disp|hp|drat")), by = am),
    dt_mt[,.(my_sum = sum(cbind(disp, hp, drat))), by = am]
)

my_vars = c("disp", "hp", "drat")

expect_identical(
    take(mtcars, my_sum = sum(columns(my_vars)), by = am),
    dt_mt[,.(my_sum = sum(cbind(disp, hp, drat))), by = am]
)

expect_identical(
    take(mtcars, my_sum = sum(columns('{my_vars}')), by = am),
    dt_mt[,.(my_sum = sum(cbind(disp, hp, drat))), by = am]
)

expect_identical(
    take(mtcars, my_sum = sum(columns("^disp", "^hp", "^drat")), by = am),
    dt_mt[,.(my_sum = sum(cbind(disp, hp, drat))), by = am]
)

expect_identical(
    take(mtcars, my_sum = sum(columns("^disp", "hp", drat)), by = am),
    dt_mt[,.(my_sum = sum(cbind(disp, hp, drat))), by = am]
)


expect_identical(
    take(mtcars, my_sum = sum(columns("^.")), by = am),
    dt_mt[,.(my_sum = sum(.SD)), by = am]
)

expect_identical(
    take(mtcars, my_sum = sum(columns(-disp)), by = am),
    dt_mt[,.(my_sum = sum(.SD[,-3])), by = am]
)
