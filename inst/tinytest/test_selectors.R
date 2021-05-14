cat("\nContext:", "take selectors", "\n")

data(mtcars)
dt_mt = as.data.table(mtcars)
expect_identical(
    take(mtcars, sum(columns(disp:drat)), by = am),
    dt_mt[,.('sum(columns(disp:drat))' = sum(cbind(disp, hp, drat))), by = am]
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

a = "disp"
b = "drat"

expect_error(take(mtcars, my_sum = sum(columns(aaa %to% bbb)), by = am))

expect_identical(
    take(mtcars, my_sum = sum(columns(a %to% b)), by = am),
    dt_mt[,.(my_sum = sum(cbind(disp, hp, drat))), by = am]
)

expect_identical(
    take(mtcars, my_sum = sum(columns((a) %to% (b))), by = am),
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
    take(mtcars, my_sum = sum(columns('{my_vars}')), my_mean = mean(as.matrix(columns('{my_vars}'))), by = am),
    dt_mt[,.(my_sum = sum(cbind(disp, hp, drat)), my_mean = mean(cbind(disp, hp, drat))), by = am]
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
    dt_mt[,.(my_sum = sum(cbind(.SD, am))), by = am]
)

expect_identical(
    take(mtcars, my_sum = sum(columns(-disp, -am)), by = am),
    dt_mt[,.(my_sum = sum(.SD[,-3])), by = am]
)

cat("\nContext:", "take_if selectors", "\n")
a = "cyl"
expect_identical(
    take_if(mtcars,
            (columns(a) == 4)[,1],
            my_sum = sum(vs), by = am),
    dt_mt[cyl == 4,.(my_sum = sum(vs)), by = am]
)

expect_identical(
    take_if(dt_mt, (columns("^am") == 0)[,1]),
    dt_mt[am == 0, ]
)


cat("\nContext:", "take_if selectors inside fun", "\n")

data(mtcars)
my_fun = function(data_arg, i_arg, j_arg){
    take_if(data_arg, columns(i_arg)[[1]]==0, res = sum(columns(j_arg)))
}


expect_identical(
    my_fun(mtcars, "am", "vs"),
    take_if(mtcars, am==0, res = sum(vs))
)

expect_identical(
    my_fun(mtcars, "^am$", "^vs$"),
    take_if(mtcars, am==0, res = sum(vs))
)

my_i = "am"
my_j = "vs"
expect_identical(
    my_fun(mtcars, "{my_i}", "{my_j}"),
    take_if(mtcars, am==0, res = sum(vs))
)

my_fun = function(data_arg, i_arg, j_arg){
    take_if(data_arg, columns("{i_arg}")[[1]]==0, res = sum(columns("{j_arg}")))
}

expect_identical(
    my_fun(mtcars, "am", "vs"),
    take_if(mtcars, am==0, res = sum(vs))
)

my_fun = function(data_arg, i_arg, j_arg){
    internal_i = i_arg
    internal_j = j_arg
    take_if(data_arg, columns("{internal_i}")[[1]]==0, res = sum(columns("{internal_j}")))
}

expect_identical(
    my_fun(mtcars, "am", "vs"),
    take_if(mtcars, am==0, res = sum(vs))
)

################################
cat("\n******\nContext: let selectors\n*******\n")


data(mtcars)
dt_mt = as.data.table(mtcars)
expect_identical(
    let(mtcars, my_sum = sum(columns(disp:drat)), by = am),
    dt_mt[,my_sum := sum(cbind(disp, hp, drat)), by = am]
)

dt_mt = as.data.table(mtcars)
expect_identical(
    let(mtcars, my_sum = sum(columns(disp %to% drat)), by = am),
    dt_mt[,my_sum := sum(cbind(disp, hp, drat)), by = am]
)

dt_mt = as.data.table(mtcars)
expect_identical(
    let(mtcars, my_sum = sum(columns(disp %to% drat)), by = am),
    dt_mt[,my_sum := sum(cbind(disp, hp, drat)), by = am]
)

dt_mt = as.data.table(mtcars)
expect_identical(
    let(mtcars, my_sum = sum(disp %to% drat), by = am),
    dt_mt[, my_sum := sum(cbind(disp, hp, drat)), by = am]
)

dt_mt = as.data.table(mtcars)
expect_identical(
    let(mtcars, my_sum = sum(columns("^disp|hp|drat")), by = am),
    dt_mt[,my_sum := sum(cbind(disp, hp, drat)), by = am]
)

my_vars = c("disp", "hp", "drat")

dt_mt = as.data.table(mtcars)
expect_identical(
    let(mtcars, my_sum = sum(columns(my_vars)), by = am),
    dt_mt[,my_sum := sum(cbind(disp, hp, drat)), by = am]
)

dt_mt = as.data.table(mtcars)
expect_identical(
    let(mtcars, my_sum = sum(columns('{my_vars}')), by = am),
    dt_mt[,my_sum := sum(cbind(disp, hp, drat)), by = am]
)

dt_mt = as.data.table(mtcars)
expect_identical(
    let(mtcars, my_sum := sum(columns("^disp", "^hp", "^drat")), by = am),
    dt_mt[,my_sum := sum(cbind(disp, hp, drat)), by = am]
)

dt_mt = as.data.table(mtcars)
expect_identical(
    let(mtcars, my_sum = sum(columns("^disp", "hp", drat)), by = am),
    dt_mt[,my_sum := sum(cbind(disp, hp, drat)), by = am]
)

dt_mt = as.data.table(mtcars)
expect_identical(
    let(mtcars, my_sum = sum(columns("^.")), by = am),
    dt_mt[, my_sum := sum(cbind(.SD, am)), by = am]
)

dt_mt = as.data.table(mtcars)
expect_identical(
    let(mtcars, my_sum = sum(columns(-disp, -am)), by = am),
    dt_mt[, my_sum := sum(.SD[,-3]), by = am]
)

cat("\nContext:", "let_if selectors", "\n")
a = "cyl"
dt_mt = as.data.table(mtcars)
expect_identical(
    let_if(mtcars,
            (columns(a) == 4)[,1],
            my_sum = sum(vs), by = am),
    setindex(dt_mt[cyl == 4, my_sum := sum(vs), by = am], NULL)
)


cat("\nContext:", "let_if selectors inside fun", "\n")

data(mtcars)
my_fun = function(data_arg, i_arg, j_arg){
    let_if(data_arg, columns(i_arg)[[1]]==0, res = sum(columns(j_arg)))
}


expect_identical(
    my_fun(mtcars, "am", "vs"),
    setindex(let_if(mtcars, am==0, res := sum(vs)), NULL)
)

expect_identical(
    my_fun(mtcars, "^am$", "^vs$"),
    setindex(let_if(mtcars, am==0, res = sum(vs)), NULL)
)

my_i = "am"
my_j = "vs"
expect_identical(
    my_fun(mtcars, "{my_i}", "{my_j}"),
    setindex(let_if(mtcars, am==0, res = sum(vs)), NULL)
)

my_fun = function(data_arg, i_arg, j_arg){
    let_if(data_arg, columns("{i_arg}")[[1]]==0, res = sum(columns("{j_arg}")))
}

expect_identical(
    my_fun(mtcars, "am", "vs"),
    setindex(let_if(mtcars, am==0, res = sum(vs)), NULL)
)

my_fun = function(data_arg, i_arg, j_arg){
    internal_i = i_arg
    internal_j = j_arg
    let_if(data_arg, columns("{internal_i}")[[1]]==0, res = sum(columns("{internal_j}")))
}

expect_identical(
    my_fun(mtcars, "am", "vs"),
    setindex(let_if(mtcars, am==0, res = sum(vs)), NULL)
)


# cat("\nContext:", "let selectors LHS", "\n")
#
# data(mtcars)
#
# dt_mt = as.data.table(mtcars)
# dt_mt2 = data.table::copy(dt_mt)
#
#
# expect_error(
#     let(dt_mt,
#         cols(param) := 43
#     )
# )
#
# param = "b{1:3}"
#
# expect_identical(
#     let(dt_mt,
#         cols(param) := 43
#     ),
#     dt_mt2[,text_expand(param) := 43]
# )
#
# aaa = let(mtcars,
#     cols("a{1:5}") := 1
#     )
#
#
# let(aaa,
#     cols("a{1:5}") := 42
# )
