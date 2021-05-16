variable = NULL # to pass CRAN check
value = NULL # to pass CRAN check


#' Convert data to long or to wide form
#'
#' `to_long` increases number of rows in the dataset and reduce number of
#' columns. `to_wide` makes invert transformation. You can use [cols] for
#' selecting variables in the arguments. See examples.
#'
#' @param data A data.frame to convert
#' @param columns unquoted names of variables for stacking. When missing, we
#'   will stack all columns outside `keep` columns.
#' @param keep unquoted names of columns which will be kept as is, e. g. only
#'   recycled or deduplicated. If missing, it is all columns except stacked or
#'   unstacked. If `FALSE` then nothing will be kept.
#' @param names_in name of the stacked variable names column. The default name
#'   is 'variable'. It is quoted in the `to_long` and unquoted in `to_wide`. If
#'   `FALSE` in the `to_wide` than nothing will be widening.
#' @param values_in name(-s) of the stacked data values column(s). The default
#'   name is 'value'. Multiple names can be provided here for the case when
#'   `columns` is a list, though note well that the names provided in
#'   `columns` take precedence. It is quoted in the `to_long` and unqoted in `to_wide`
#' @param drop_na If TRUE, NA values will be removed from the stacked data.
#' @param names_factor If TRUE, the column with names will be converted to
#'   factor, else it will be a character column. TRUE by default.
#' @param value_factor If TRUE, the value column will be converted to factor,
#'   else the stacked values type is left unchanged. FALSE by default.
#' @param fun Should the data be aggregated before casting? By default, it is
#'   `identity` - no aggregation. To use multiple aggregation functions, pass a
#'   list; see Examples.
#' @param sep Character vector of length 1, indicating the separating character
#'   in variable names generated during casting. Default is "_".
#' @param fill Value with which to fill missing cells. `NA` by default. If `fun` is
#'   present, takes the value by applying the function on a 0-length vector.
#' @param missing_comb One of "none" (the default), "rows" - include missing
#'   combinations in rows, "columns" - include missing combinations in columns,
#'   and "all" include all missing combinations.
#' @param ... other arguments passed to `data.table::melt`/`data.table::dcast`
#' @return data.table in the wide or long form.
#' @export
#'
#' @examples
#' data(iris)
#'
#' # 'to_long'
#'
#' long_iris = iris %>%
#'     to_long(keep = Species)
#'
#' long_iris
#'
#' iris_with_stat = long_iris %>%
#'     take(mean = mean(value),
#'          sd = sd(value),
#'          n = .N*1.0,
#'          by = .(Species, variable)
#'     ) %>%
#'     to_long(columns = c(mean, sd, n), names_in = "stat")
#'
#' # 'to_wide' - table with multiple stats
#' iris_with_stat %>%
#'     to_wide()
#'
#'
#' iris_with_stat %>%
#'     to_wide(names_in = c(variable, stat))
#'
#' iris_with_stat %>%
#'     to_wide(names_in = c(variable, Species))
#'
#' # 'to_wide' - aggregation function
#' long_iris %>%
#'     to_wide(fun = list(Mean = mean, SD = sd, N = length))
#'
#' # multiple variables
#' iris %>%
#'     to_long(list(Sepal = cols("^Sepal"), Petal = cols("^Petal"))) %>%
#'     let(
#'         variable = factor(variable, levels = 1:2, labels = c("Length", "Width"))
#'     ) %>%
#'     to_wide(values_in = c(Sepal, Petal))
#'
#' # '%to%' selector - example from tidyr::pivot_longer
#'
#' data(anscombe)
#' anscombe %>%
#'     to_long(
#'         list(x = x1 %to% x4, y = y1 %to% y4),
#'         names_in = "set"
#'     )
#'
#' ######################################
#' ## Examples from data.table melt/dcast
#' ######################################
#'
#' set.seed(45)
#' DT = data.table(
#'     i_1 = c(1:5, NA)*1.0,
#'     i_2 = c(NA,6,7,8,9,10)*1.0,
#'     f_1 = factor(sample(c(letters[1:3], NA), 6, TRUE)),
#'     f_2 = factor(c("z", "a", "x", "c", "x", "x"), ordered=TRUE),
#'     c_1 = sample(c(letters[1:3], NA), 6, TRUE),
#'     d_1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"),
#'     d_2 = as.Date(6:1, origin="2012-01-01")
#' )
#'
#' # id, values as character/integer/numeric vectors
#'
#' to_long(DT, f_1, keep = 1:2)
#' to_long(DT, f_1, keep = c(i_1, i_2))
#' to_long(DT, f_1, keep = i_1 %to% i_2)
#' to_long(DT, f_1, keep = cols(i_1:i_2), names_factor = FALSE)
#' to_long(DT, f_1, keep = cols("i_{1:2}"))
#' to_long(DT, f_1, keep = cols("^i_"))
#' to_long(DT, f_1, keep = cols("^i_"), names_in = "var", values_in = "val")
#'
#' col_var = "^i_"
#' to_long(DT, 3, keep = cols(col_var))
#'
#' to_long(DT, cols("^f_"), keep = cols("^i_"), value_factor = TRUE)
#'
#' to_long(mtcars)
#' to_long(mtcars, keep = am)
#' to_long(mtcars, columns = c(am, vs, mpg))
#' to_long(mtcars, columns = c(am, vs, mpg), keep = FALSE)
#' to_long(DT, keep = f_1, columns = c(i_1, i_2), drop_na = TRUE)
#' to_long(DT, keep=1:2, columns = list(cols("^f_"), cols("^d_")), value_factor=TRUE)
#'
#' data("ChickWeight")
#' names(ChickWeight) = tolower(names(ChickWeight))
#' DT = to_long(ChickWeight, keep=2:4)
#'
#' to_wide(DT, keep = time, fun = mean)
#' to_wide(DT, keep = FALSE, fun = mean)
#' to_wide(DT, keep = diet, fun = mean)
#' to_wide(DT, keep = c(diet, chick), names_in = time, missing_comb = "all")
#' to_wide(DT, keep = c(diet, chick), names_in = time, missing_comb = "all", fill = 0)
#' to_wide(DT, chick, time, fun = mean)
#'
#'
#'
#' # using FALSE
#' DT = data.table(v1 = rep(1:2, each = 6),
#'                 v2 = rep(rep(1:3, 2), each = 2),
#'                 v3 = rep(1:2, 6),
#'                 v4 = rnorm(6))
#'
#' ## for each combination of (v1, v2), add up all values of v4
#' to_wide(DT,
#'         cols("^v(1|2)"),
#'         names_in = FALSE,
#'         values_in = v4,
#'         fun = sum
#' )
#'
#' # multiple values_in and multiple fun
#' DT = data.table(x=sample(5,20,TRUE),
#'                 y=sample(2,20,TRUE),
#'                 z=sample(letters[1:2], 20,TRUE),
#'                 d1 = runif(20),
#'                 d2=1L)
#'
#' # multiple values_in
#' to_wide(DT,
#'         keep = c(x, y),
#'         names_in = z,
#'         values_in = c(d1, d2),
#'         fun = sum,
#'         fill = 0)
#'
#' # multiple funs
#' to_wide(DT,
#'         keep = c(x, y),
#'         names_in = z,
#'         values_in = d1,
#'         fun = list(sum = sum, mean = mean),
#'         fill = NULL)
#'
#' # multiple fun and values_in (all combinations)
#' to_wide(DT,
#'         keep = c(x, y),
#'         names_in = z,
#'         values_in = c(d1, d2),
#'         fun = list(sum = sum, mean = mean)
#' )
#'
#' # multiple fun and values_in (one-to-one)
#' to_wide(DT,
#'         keep = c(x, y),
#'         names_in = z,
#'         values_in = list(d1, d2),
#'         fun = list(sum = sum, mean = mean)
#' )
to_long = function(data,
                   columns = NULL,
                   keep = NULL,
                   names_in = "variable",
                   values_in = "value",
                   drop_na = FALSE,
                   names_factor = TRUE,
                   value_factor = FALSE,
                   ...){
    parent_frame = parent.frame()
    data_names = names(data)
    data_names_list = create_list_with_names(data_names)
    columns_expr = substitute(columns)
    columns_expr = replace_column_expr(columns_expr, data_names, parent_frame, type = "names")
    columns = eval(columns_expr, data_names_list, enclos = parent_frame)

    keep_expr = substitute(keep)
    keep_expr = replace_column_expr(keep_expr, data_names, parent_frame, type = "names")
    keep = eval(keep_expr, data_names_list, enclos = parent_frame)
    if(isFALSE(keep)) keep = integer(0)# no keep

    if(is.null(columns) && is.null(keep)) columns = data_names
    maditr::melt(data = data,
                 id.vars = keep,
                 measure.vars = columns,
                 variable.name = names_in,
                 value.name = values_in,
                 na.rm = drop_na,
                 variable.factor = names_factor,
                 value.factor = value_factor,
                 ...
    )
}


#' @export
#' @rdname to_long
to_wide = function(data,
                   keep = NULL,
                   names_in = variable,
                   values_in = value,
                   fun = identity,
                   sep = "_",
                   fill = NA,
                   missing_comb = c("none", "rows", "columns", "all"),
                   ...
){
    parent_frame = parent.frame()
    data_names = names(data)
    data_names_list = create_list_with_names(data_names)
    names_in_expr = substitute(names_in)
    names_in_expr = replace_column_expr(names_in_expr, data_names, parent_frame, type = "names")
    names_in = eval(names_in_expr, data_names_list, enclos = parent_frame)
    if(isFALSE(names_in)) names_in = "." # no widening

    values_in_expr = substitute(values_in)
    values_in_expr = replace_column_expr(values_in_expr, data_names, parent_frame, type = "names")
    values_in = eval(values_in_expr, data_names_list, enclos = parent_frame)

    keep_expr = substitute(keep)
    keep_expr = replace_column_expr(keep_expr, data_names, parent_frame, type = "names")
    keep = eval(keep_expr, data_names_list, enclos = parent_frame)
    if(isFALSE(keep)) keep = "."   # no keep
    if(is.null(keep)) keep = "..." # keep all



    formula = stats::as.formula(paste(paste(keep, collapse = "+"), "~", paste(names_in, collapse = "+")))
    missing_comb = match.arg(missing_comb)
    drop = switch(missing_comb,
                  "none" = TRUE,
                  "rows" = c(FALSE, TRUE),
                  "columns" = c(TRUE, FALSE),
                  "all"  = c(FALSE, FALSE)
    )
    maditr::dcast(data = data,
                  formula,
                  fun.aggregate = fun,
                  sep = sep,
                  fill = fill,
                  drop = drop,
                  value.var = values_in,
                  ...
    )
}


