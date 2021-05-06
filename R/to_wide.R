#' Convert data to long or to wide form
#'
#' @param data A data.frame object to convert
#' @param columns Variables for stacking. When missing, we will stack all columns outside `keep` columns.
#' @param keep columns which will be kept as is, e. g. only recycled or
#'   deduplicated. If missing, it is all columns except stacked or unstacked.
#' @param names_in name of the stacked variable names column. The default name is 'variable'.
#' @param values_in name of the stacked data values column(s). The default
#'   name is 'value'. Multiple names can be provided here for the case when
#'   `columns` is a list, though note well that the names provided in
#'   `columns` take precedence.
#' @param drop_na If TRUE, NA values will be removed from the stacked data.
#' @param names_factor If TRUE, the column with names will be converted to
#'   factor, else it will be a character column. TRUE by default.
#' @param value_factor If TRUE, the value column will be converted to factor,
#'   else the stacked values type is left unchanged. FALSE by default.
#' @param fun Function.
#' @param sep A character.
#' @param fill = NA,
#' @param missing_comb A logical. TRUE by default.
#' @param ... other arguments passed to `data.table::melt`/`data.table::dcast`
#' @return data.table in the wide or long form.
#' @export
#'
#' @examples
#' 1
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
    columns_expr = substitute(columns)
    columns_expr = replace_column_expr(columns_expr, data_names, parent_frame, combine = quote(c))
    columns = eval(columns_expr, setNames(as.list(data_names), data_names), enclos = parent_frame)

    keep_expr = substitute(keep)
    keep_expr = replace_column_expr(keep_expr, data_names, parent_frame, combine = quote(c))
    keep = eval(keep_expr, setNames(as.list(data_names), data_names), enclos = parent_frame)
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
    names_in_expr = substitute(names_in)
    names_in_expr = replace_column_expr(names_in_expr, data_names, parent_frame, combine = quote(c))
    names_in = eval(names_in_expr, setNames(as.list(data_names), data_names), enclos = parent_frame)
    if(isFALSE(names_in)) names_in = "." # no widening

    values_in_expr = substitute(values_in)
    values_in_expr = replace_column_expr(values_in_expr, data_names, parent_frame, combine = quote(c))
    values_in = eval(values_in_expr, setNames(as.list(data_names), data_names), enclos = parent_frame)

    keep_expr = substitute(keep)
    keep_expr = replace_column_expr(keep_expr, data_names, parent_frame, combine = quote(c))
    keep = eval(keep_expr, setNames(as.list(data_names), data_names), enclos = parent_frame)
    if(isFALSE(keep)) keep = "."   # no keep
    if(is.null(keep)) keep = "..." # keep all



    formula = as.formula(paste(paste(keep, collapse = "+"), "~", paste(names_in, collapse = "+")))
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


