#' Additional functions
#'
#' @param data data.table/data.frame data.frame will be automatically converted
#'   to data.table.
#' @param ... variables to group by.
#' @param weight. optional. Unquoted variable name. If provided result will be the sum of this variable by groups.
#' @param sort logical. If TRUE result will be sorted in desending order by resulting variable.
#' @param name character. Name of resulting variable.
#'
#' @return data.table
#' @export
#'
#' @examples
#' 1
dt_count = function(data, ..., weight = NULL, sort = FALSE, name = "n"){
    name = as.symbol(name)
    weight_expr = substitute(weight)
    if(is.null(weight_expr)){
        res = eval.parent(substitute(take(data, name := .N, by = .(...))))
    } else {
        res = eval.parent(substitute(take_if(data, !is.na(weight), name := sum(weight), by = .(...))))
    }
    if(sort) {
        res = eval(substitute(sort_by(res, -name), list(name = name)))

    }
    res
}

#' @export
dt_add_count = function(data, ..., weight = NULL, sort = FALSE, name = "n"){
    name = as.symbol(name)
    weight_expr = substitute(weight)
    if(is.null(weight_expr)){
        res = eval.parent(substitute(let(data, name := .N, by = .(...))))
    } else {
        res = eval.parent(substitute(let_if(data, !is.na(weight), name := sum(weight), by = .(...))))
    }
    if(sort) {
        res = eval(substitute(sort_by(res, -name), list(name = name)))

    }
    res
}
