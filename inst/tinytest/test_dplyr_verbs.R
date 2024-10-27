

data(mtcars)

cat("\nContext:", "verbs errors", "\n")
expect_error(dt_arrange(1:5, am))
expect_error(dt_select(1:5, am))
expect_error(dt_select(iris, "^aaa"))
expect_error(dt_mutate(1:5, new := 1))
expect_error(dt_summarise(1:5, new = mean(mpg)))
expect_error(dt_summarise_all(1:5, by = cyl))
expect_error(dt_summarise_all(mtcars, by = cyl))
expect_error(dt_filter(1:5, cyl==1))
expect_error(dt_filter(mtcars, cyl==2, by = am))
expect_error(dt_filter(mtcars, cyl=1))
expect_error(dt_filter(mtcars, c(TRUE, FALSE)))

################
cat("\nContext:", "dt_mutate", "\n")
mt_dt = as.data.table(mtcars)
mt_dt2 = data.table::copy(mt_dt)
new_dt = dt_mutate(mt_dt, mpg_hp = mpg/hp, new = mpg_hp*2)
new_dt2 = dt_mutate(mtcars, mpg_hp = mpg/hp, new = mpg_hp*2)
mt_dt2[, mpg_hp := mpg/hp][, new := mpg_hp*2]
expect_identical(new_dt, mt_dt)
expect_identical(new_dt, mt_dt2)
expect_identical(new_dt2, mt_dt2)
###############

###############
cat("\nContext:", "summarize/summarize_all", "\n")
mt_dt = as.data.table(mtcars)
res = dt_summarize(mtcars, fun = mean, by = am)
res2 = dt_summarize(mt_dt, fun = mean, by = am)
res3 = mt_dt[, lapply(.SD, mean), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)
###########
res = dt_summarize_all(mtcars, mean, by = am)
res2 = dt_summarize_all(mt_dt, mean, by = am)
res3 = mt_dt[, lapply(.SD, mean), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)
###############
mt_dt = as.data.table(mtcars)
res = dt_summarize(mtcars, mpg, hp, fun = mean, by = am)
res2 = dt_summarize(mt_dt, mpg, hp, fun = mean, by = am)
res3 = mt_dt[, lapply(list(mpg = mpg, hp = hp), mean), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)

###############
mt_dt = as.data.table(mtcars)
res = dt_summarize(mtcars, mpg, just_wow = hp, fun = mean, by = am)
res2 = dt_summarize(mt_dt, mpg, just_wow = hp, fun = mean, by = am)
res3 = mt_dt[, lapply(list(mpg = mpg, just_wow = hp), mean), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)
###################
#############
mt_dt = as.data.table(mtcars)
res = dt_summarize(mtcars, agg = mean(mpg), agg2 = mean(hp), by = am)
res2 = dt_summarize(mt_dt, agg = mean(mpg), agg2 = mean(hp), by = am)
res3 = mt_dt[, list(agg = mean(mpg), agg2 = mean(hp)), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)
#############
mt_dt = as.data.table(mtcars)
res = dt_summarize(mtcars, agg := mean(mpg), agg2 := mean(hp), by = am)
res2 = dt_summarize(mt_dt, agg := mean(mpg), agg2 := mean(hp), by = am)
res3 = mt_dt[, list(agg = mean(mpg), agg2 = mean(hp)), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)

new_var1 = "agg"
new_var2 = "agg2"
expr1 = quote(mean(mpg))
expr2 = quote(mean(hp))
mt_dt = as.data.table(mtcars)
res = dt_summarize(mtcars, (new_var1) := eval(expr1), (new_var2) := eval(expr2), by = am)
res2 = dt_summarize(mt_dt, (new_var1) := eval(expr1), (new_var2) := eval(expr2), by = am)
res3 = mt_dt[, list(agg = mean(mpg), agg2 = mean(hp)), by = am]
expect_equal(res3, res)
expect_equal(res3, res2)



##########################
mt_dt = as.data.table(mtcars)
res = dt_summarize(mtcars, mean(mpg), mean(hp), by = am)
res2 = dt_summarize(mt_dt, mean(mpg), mean(hp), by = am)
res3 = mt_dt[, list("mean(mpg)" = mean(mpg), "mean(hp)" = mean(hp)), by = am]
expect_identical(res3, res)
expect_identical(res3, res2)

##########################
cat("\nContext:", "dt_filter", "\n")
res = dt_filter(mtcars, vs==0, am==0)
res2 = dt_filter(mt_dt, vs==0, am==0)
res3 = mt_dt[vs==0 & am==0, ]
expect_identical(res3, res)
expect_identical(res3, res2)

data(mtcars)
dt_mt = as.data.table(mtcars)
expect_identical(
    rows(mtcars, rowSums(vs %to% am)>0),
    dt_mt[vs>0 | am>0, ]
)

etab = data.frame(a = 1:2, b = 3:4)
class(etab) = c("etable", class(etab))
res = etab[2,, drop = FALSE]
rownames(res) = NULL
expect_equal(res, rows(etab, 2))

etab = data.frame(a = 1:2, b = 3:4)
class(etab) = c("etable", class(etab))
res = etab[1,, drop = FALSE]
rownames(res) = NULL
expect_equal(res, rows(etab, b<4))
##########################

cat("\nContext:", "dt_arrange", "\n")
mt_dt = as.data.table(mtcars)
mt_dt2 = data.table::copy(mt_dt)
new_dt = dt_arrange(mt_dt, -cyl, mpg)
new_dt2 = dt_arrange(as.data.table(mtcars), -cyl, mpg)
res3 = mt_dt2[order(-cyl, mpg), ]
expect_identical(new_dt, mt_dt)
expect_identical(new_dt, res3)
expect_identical(new_dt2, res3)

##########################

cat("\nContext:", "dt_select", "\n")
mt_dt = as.data.table(mtcars)
res1 = dt_select(mt_dt, -cyl, -am)
res3 = mt_dt[, -c(2,9), with = FALSE]
expect_identical(res1, res3)

