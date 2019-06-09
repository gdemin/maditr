

data(mtcars)

context("errors")
expect_error(take(1:5, fun = sum))
expect_error(let(1:5, new = 1))
expect_error(query(1:5, new := 1))

expect_error(let(mtcars))
expect_error(let(mtcars, am*2))
################
context("let/let_if")
mt_dt = as.data.table(mtcars)
mt_dt2 = data.table::copy(mt_dt)
new_dt = let_if(mt_dt, am==0, mpg_hp = mpg/hp, new = mpg_hp*2)
new_dt2 = let_if(mtcars, am==0, mpg_hp = mpg/hp, new = mpg_hp*2)
mt_dt2[am==0, mpg_hp := mpg/hp][am==0, new := mpg_hp*2]
expect_identical(new_dt, mt_dt)
expect_identical(new_dt, mt_dt2)
expect_identical(new_dt2, mt_dt2)
###############
mt_dt = as.data.table(mtcars)
mt_dt2 = data.table::copy(mt_dt)
new_dt = let_if(mt_dt, am==0, mpg_hp := mpg/hp, "new" := mpg_hp*2)
new_dt2 = let_if(mtcars, am==0, mpg_hp := mpg/hp, "new" := mpg_hp*2)
mt_dt2[am==0, mpg_hp := mpg/hp][am==0, new := mpg_hp*2]
expect_identical(new_dt, mt_dt)
expect_identical(new_dt, mt_dt2)
expect_identical(new_dt2, mt_dt2)
###############
mt_dt = as.data.table(mtcars)
mt_dt2 = data.table::copy(mt_dt)
new_dt = let(mt_dt, mpg_hp = mpg/hp, new = mpg_hp*2)
new_dt2 = let(mtcars, mpg_hp = mpg/hp, new = mpg_hp*2)
mt_dt2[, mpg_hp := mpg/hp][, new := mpg_hp*2]
expect_identical(new_dt, mt_dt)
expect_identical(new_dt, mt_dt2)
expect_identical(new_dt2, mt_dt2)

##########

mt_dt = as.data.table(mtcars)
mt_dt2 = data.table::copy(mt_dt)
let(mt_dt, filt = am==0)
let_if(mt_dt, filt, counter = 5)


mt_dt2[, filt:= am==0]
mt_dt2[(filt), counter :=5]

expect_identical(mt_dt, mt_dt2)
expect_identical(take_if(mt_dt, filt), mt_dt[filt==TRUE,])

###############
context("take/take_if")
mt_dt = as.data.table(mtcars)
res = take(mtcars, fun = mean, by = am)
res2 = take(mt_dt, fun = mean, by = am)
res3 = mt_dt[, lapply(.SD, mean), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)
###############
mt_dt = as.data.table(mtcars)
res = take(mtcars, fun = mean, by = am, .SDcols = c("mpg", "qsec"))
res2 = take(mt_dt, fun = mean, by = am, .SDcols = c("mpg", "qsec"))
res3 = mt_dt[, lapply(.SD, mean), by = am, .SDcols = c("mpg", "qsec")]
expect_identical(res3, res)
expect_identical(res3, res2)
###############
mt_dt = as.data.table(mtcars)
res = take(mtcars, mpg, hp, fun = mean, by = am)
res2 = take(mt_dt, mpg, hp, fun = mean, by = am)
res3 = mt_dt[, lapply(list(mpg = mpg, hp = hp), mean), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)

###############
mt_dt = as.data.table(mtcars)
res = take(mtcars, mpg, just_wow = hp, fun = mean, by = am)
res2 = take(mt_dt, mpg, just_wow = hp, fun = mean, by = am)
res3 = mt_dt[, lapply(list(mpg = mpg, just_wow = hp), mean), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)
###################
###############
mt_dt = as.data.table(mtcars)
res = take(mtcars, mpg, just_wow := hp, fun = mean, by = am)
res2 = take(mt_dt, mpg, just_wow := hp, fun = mean, by = am)
res3 = mt_dt[, lapply(list(mpg = mpg, just_wow = hp), mean), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)
###################
mt_dt = as.data.table(mtcars)
res = take_if(mtcars, am==0, fun = mean, by = am)
res2 = take_if(mt_dt, am==0, fun = mean, by = am)
res3 = mt_dt[am==0, lapply(.SD, mean), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)

#############
mt_dt = as.data.table(mtcars)
res = take(mtcars, agg = mean(mpg), agg2 = mean(hp), by = am)
res2 = take(mt_dt, agg = mean(mpg), agg2 = mean(hp), by = am)
res3 = mt_dt[, list(agg = mean(mpg), agg2 = mean(hp)), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)
#############
mt_dt = as.data.table(mtcars)
res = take(mtcars, agg := mean(mpg), agg2 = mean(hp), by = am)
res2 = take(mt_dt, agg := mean(mpg), agg2 = mean(hp), by = am)
res3 = mt_dt[, list(agg = mean(mpg), agg2 = mean(hp)), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)
#############
mt_dt = as.data.table(mtcars)
res = take(mtcars, agg := mean(mpg), agg2 := mean(hp), by = am)
res2 = take(mt_dt, agg := mean(mpg), agg2 := mean(hp), by = am)
res3 = mt_dt[, list(agg = mean(mpg), agg2 = mean(hp)), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)
##########################
mt_dt = as.data.table(mtcars)
res = take(mtcars, mean(mpg), mean(hp), by = am)
res2 = take(mt_dt, mean(mpg), mean(hp), by = am)
res3 = mt_dt[, list("mean(mpg)" = mean(mpg), "mean(hp)" = mean(hp)), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)
##########################
mt_dt = as.data.table(mtcars)
res = take(mtcars, mean(mpg), mean(hp), by = am, autoname = FALSE)
res2 = take(mt_dt, mean(mpg), mean(hp), by = am, autoname = FALSE)
res3 = mt_dt[, list(mean(mpg), mean(hp)), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)
##########################
mt_dt = as.data.table(mtcars)
res = take_if(mtcars, vs==0, mean(mpg), mean(hp), by = am, autoname = FALSE)
res2 = take_if(mt_dt, vs==0, mean(mpg), mean(hp), by = am, autoname = FALSE)
res3 = mt_dt[vs==0,  list(mean(mpg), mean(hp)), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)
##########################

context("query/query_if")
##########################
mt_dt = as.data.table(mtcars)
res = query(mtcars, list(mean(mpg), mean(hp)), by = am)
res2 = query(mt_dt, list(mean(mpg), mean(hp)), by = am)
res3 = mt_dt[, list(mean(mpg), mean(hp)), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)
##########################
mt_dt = as.data.table(mtcars)
res = query_if(mtcars, vs==0, list(mean(mpg), mean(hp)), by = am)
res2 = query_if(mt_dt, vs==0, list(mean(mpg), mean(hp)), by = am)
res3 = mt_dt[vs==0,  list(mean(mpg), mean(hp)), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)

context("let/take: parametric evaluation")



#############
mt_dt = as.data.table(mtcars)
new_var = "agg"
res = take(mtcars, (new_var) := mean(mpg), agg2 = mean(hp), by = am)
res2 = take(mt_dt, (new_var) := mean(mpg), agg2 = mean(hp), by = am)
res3 = mt_dt[, list(agg = mean(mpg), agg2 = mean(hp)), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)


mt_dt = as.data.table(mtcars)
new_var = "agg"
new_var2 = "agg2"
res = take(mtcars, (new_var) := mean(mpg), (new_var2) := mean(hp), by = am)
res2 = take(mt_dt, (new_var) := mean(mpg), (new_var2) := mean(hp), by = am)
res3 = mt_dt[, list(agg = mean(mpg), agg2 = mean(hp)), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)


expr1 = quote(mean(mpg))
expr2 = quote(mean(hp))
mt_dt = as.data.table(mtcars)
new_var = "agg"
new_var2 = "agg2"
res = take(mtcars, (new_var) := eval(expr1), (new_var2) := eval(expr2), by = am)
res2 = take(mt_dt, (new_var) := eval(expr1), (new_var2) := eval(expr2), by = am)
res3 = mt_dt[, list(agg = mean(mpg), agg2 = mean(hp)), by = am]
expect_equal(res3, res)
expect_equal(res3, res2)
