#' Convert data to long or to wide form
#'
#' @param data A data.frame object to convert
#' @param columns Variables for stacking. When missing, we will stack all columns outside `keep` columns.
#' @param names_column name of the stacked variable names column. The default name is 'variable'.
#' @param values_column name of the stacked data values column(s). The default
#'   name is 'value'. Multiple names can be provided here for the case when
#'   `columns` is a list, though note well that the names provided in
#'   `columns` take precedence.
#' @param keep columns which will be kept as is, e. g. only recycled or
#'   deduplicated. If missing, it is all columns except stacked or unstacked.
#' @param drop_na If TRUE, NA values will be removed from the stacked data.
#' @param names_factor If TRUE, the column with names will be converted to
#'   factor, else it will be a character column. TRUE by default.
#' @param value_factor If TRUE, the value column will be converted to factor,
#'   else the stacked values type is left unchanged. FALSE by default.
#' @param fun Function.
#' @param sep A character.
#' @param fill = NA,
#' @param drop A logical. TRUE by default.
#' @param ... other arguments passed to `data.table::melt`/`data.table::dcast`
#' @return data.table in the wide or long form.
#' @export
#'
#' @examples
#' 1
to_long = function(data,
                   columns,
                   names_column = "variable",
                   values_column = "value",
                   keep,
                   drop_na = FALSE,
                   names_factor = TRUE,
                   value_factor = FALSE,
                   ...){
    parent_frame = parent.frame()
    data_names = names(data)
    columns_expr = substitute(columns)
    if(!missing(columns_expr)){
        columns_expr = replace_column_expr(columns_expr, data_names, parent_frame, combine = quote(c))
        columns = eval(columns_expr, setNames(as.list(data_names), data_names), enclos = parent_frame)
    }
    keep_expr = substitute(keep)
    if(!missing(keep_expr)){
        keep_expr = replace_column_expr(keep_expr, data_names, parent_frame, combine = quote(c))
        keep = eval(keep_expr, setNames(as.list(data_names), data_names), enclos = parent_frame)
    }
    if(missing(columns_expr) && missing(keep_expr)) columns = data_names
    maditr::melt(data = data,
                 id.vars = keep,
                 measure.vars = columns,
                 variable.name = names_column,
                 value.name = values_column,
                 na.rm = drop_na,
                 variable.factor = names_factor,
                 value.factor = value_factor,
                 ...
    )
}


#' @export
#' @rdname to_long
to_wide = function(data,
                   names_column = variable,
                   values_column = value,
                   keep,
                   fun = identity,
                   sep = "_",
                   fill = NA,
                   drop = TRUE,
                   ...
){
    maditr::dcast(data = data,
                  formula,
                  fun.aggregate = fun,
                  sep = sep,
                  fill = fill,
                  drop = drop,
                  value.var = value,
                  ...
    )
}
