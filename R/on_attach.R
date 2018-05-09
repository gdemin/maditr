.onAttach = function(...) {
    hints = c(
        'To select rows from data: take_if(mtcars, am==0)',
        'To select columns from data: take(mtcars, am, vs, mpg)',
        'To aggregate data: take(mtcars, mean_mpg = mean(mpg), by = am)',
        'To aggregate all non-grouping columns: take(mtcars, fun = mean, by = am)',
        'To aggregate several columns with one summary: take(mtcars, mpg, hp, fun = mean, by = am)',
        "To get total summary skip 'by' argument: take(mtcars, fun = mean)",
        "Use magrittr pipe '%>%' to chain several operations:
             mtcars %>%
                 let(mpg_hp = mpg/hp) %>%
                 take(mean(mpg_hp), by = am)
        ",
        'To modify variables or add new variables:
             let(mtcars, new_var = 42, new_var2 = new_var*hp) %>% head()',
        'To drop variable use NULL: let(mtcars, am = NULL) %>% head()'
    )
    curr_hint = sample(hints, 1)
    packageStartupMessage(paste0("\n", curr_hint, "\n"))
}
