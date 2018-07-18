#' Convert data between wide and long forms.
#'
#' The `dcast` formula takes the form `LHS ~ RHS`, ex: `var1 + var2 ~ var3`. The
#' order of entries in the formula is essential. There are two special
#' variables: `.` and `...`. `.` represents no variable; `...` represents all
#' variables not otherwise mentioned in formula. LHS variable values will be in
#' rows. RHS variables values will become column names.
#' `fun.aggregate(value.var)` will be cell values. For details see
#' \link[data.table]{dcast} and \link[data.table]{melt}.
#' @param data A data.table/data.frame. \code{data.frame} will be automatically
#'   converted to data.table.
#' @param formula A formula of the form LHS ~ RHS to cast. For details see \link[data.table]{dcast}.
#' @param fun.aggregate Should the data be aggregated before casting? If the
#'   formula doesn't identify a single observation for each cell, then
#'   aggregation defaults to length with a message.
#' @param sep Character vector of length 1, indicating the separating character
#'   in variable names generated during casting. Default is _ for backwards
#'   compatibility.
#' @param margins For details see \link[data.table]{dcast}.
#' @param subset Specified if casting should be done on a subset of the data.
#' @param fill Value with which to fill missing cells. If fun.aggregate is
#'   present, takes the value by applying the function on a 0-length vector.
#' @param drop FALSE will cast by including all missing combinations. c(FALSE,
#'   TRUE) will only include all missing combinations of formula LHS. And
#'   c(TRUE, FALSE) will only include all missing combinations of formula RHS.
#' @param value.var Name of the column whose values will be filled to cast.
#'   Function 'guess()' tries to, well, guess this column automatically, if none
#'   is provided. It is possible to cast multiple `value.var`` columns
#'   simultaneously. For details see \link[data.table]{dcast}.
#' @param verbose For details see \link[data.table]{dcast}.
#' @param ... Any other arguments that may be passed to the aggregating function.
#'
#'
#' @param id.vars vector of id variables. Can be integer (corresponding id
#'   column numbers) or character (id column names) vector. If missing, all
#'   non-measure columns will be assigned to it. If integer, must be positive;
#'   see Details.
#' @param measure.vars Measure variables for melting. Can be missing, vector,
#'   list, or pattern-based. For details see \link[data.table]{dcast}.
#' @param variable.name name for the measured variable names column. The default name is 'variable'.
#' @param value.name name for the molten data values column(s). The default name
#'   is 'value'. Multiple names can be provided here for the case when
#'   measure.vars is a list, though note well that the names provided in
#'   measure.vars take precedence.
#' @param na.rm If TRUE, NA values will be removed from the molten data.
#' @param variable.factor If TRUE, the variable column will be converted to
#'   factor, else it will be a character column.
#' @param value.factor If TRUE, the value column will be converted to factor,
#'   else the molten value type is left unchanged.
#'
#'
#' @return data.table
#' @author Matt Dowle <mattjdowle@gmail.com>
#' @export
#'
#' @examples
#' # examples from 'tidyr' package
#' stocks = data.frame(
#'     time = as.Date('2009-01-01') + 0:9,
#'     X = rnorm(10, 0, 1),
#'     Y = rnorm(10, 0, 2),
#'     Z = rnorm(10, 0, 4)
#' )
#' stocksm = stocks %>%
#'     melt(id.vars = "time", variable.name = "stock", value.name = "price")
#' stocksm %>% dcast(time ~ stock)
#' stocksm %>% dcast(stock ~ time)
#'
#' # dcast and melt are complements
#' df = data.frame(x = c("a", "b"), y = c(3, 4), z = c(5, 6))
#' df %>%
#'     dcast(z ~ x, value.var = "y") %>%
#'     melt(id.vars = "z", variable.name = "x", value.name = "y", na.rm = TRUE)
dcast = function(data, formula, fun.aggregate = NULL, sep = "_",
                 ..., margins = NULL, subset = NULL, fill = NULL,
                 drop = TRUE, value.var = guess(data),
                 verbose = getOption("datatable.verbose")){
    curr_call = sys.call()
    curr_call[[1]] = quote(data.table::dcast.data.table)
    if(!is.data.table(data)){
        curr_call[[2]] = substitute(as.data.table(data))
    }
    eval.parent(curr_call)
}



#' @export
#' @rdname dcast
melt = function(data, id.vars, measure.vars,
                variable.name = "variable", value.name = "value",
                ..., na.rm = FALSE, variable.factor = TRUE,
                value.factor = FALSE,
                verbose = getOption("datatable.verbose")){
    curr_call = sys.call()
    curr_call[[1]] = quote(data.table::melt.data.table)
    if(!is.data.table(data)){
        curr_call[[2]] = substitute(as.data.table(data))
    }
    eval.parent(curr_call)
}


#' @rdname dcast
#' @export
guess = function (data){
    if ("value" %chin% names(data))
        return("value")
    if ("(all)" %chin% names(data))
        return("(all)")
    var = names(data)[ncol(data)]
    message("Using '", var, "' as value column. Use 'value.var' to override")
    return(var)
}
