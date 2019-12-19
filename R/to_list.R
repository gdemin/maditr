#' Title
#'
#' @param data data.frame/list/vector
#' @param expr expression
#' @param skip_null logical Should we skip NULL's from result? Default is TRUE
#'
#' @return list
#' @export
#'
#' @examples
#' 1
to_list = function(data, expr = NULL, skip_null = TRUE){
    expr = substitute(expr)
    if(is.null(expr)) return(as.list(data))

    expr = substitute_symbols(expr, list(
        '.item' = quote(data[[.index]]),
        '.x' = quote(data[[.index]]),
        '.name' = quote(._names[[.index]]),
        '.i' = quote(.index)
    ))
    ._indexes = seq_along(data)
    ._names = names(data)
    names(._indexes) = ._names
    if(is.null(._names)) ._names = rep("", length(data))
    if(is.symbol(expr) && !identical(expr, quote(.index))){
        fun = match.fun(eval(expr))
        res = lapply(data, fun)
    } else {
        fun = eval(substitute_symbols(quote(function(.index) expr), list(expr = expr)))
        res = lapply(._indexes, fun)
    }

    if(skip_null){
        nulls = vapply(res, is.null, FUN.VALUE = logical(1), USE.NAMES = FALSE)
        res = res[!nulls]
    }
    res
}

#' @rdname to_list
#' @export
to_vec = function(data, expr = NULL, skip_null = TRUE, recursive = TRUE, use.names = TRUE){
     res = eval.parent(substitute(to_list(data, expr, skip_null = skip_null)))
     unlist(res, recursive = recursive, use.names = use.names)

}


#' @rdname to_list
#' @export
to_df = function(data, expr = NULL, idcol = NULL){
    res = eval.parent(substitute(to_list(data, expr, skip_null = TRUE)))
    rbindlist(res, use.names=TRUE, fill=TRUE, idcol = idcol)

}

#' @rdname to_list
#' @export
to_dfr = to_df

#' @rdname to_list
#' @export
to_dfc = function(data, expr = NULL){
    res = eval.parent(substitute(to_list(data, expr, skip_null = TRUE)))
    as.data.table(res)
}

# expr - expression as after 'substitute'
# symbols - named list  - names will be substituted with values
substitute_symbols = function(substitute_result, symbols) {
    eval(bquote(substitute(.(substitute_result), symbols)))
}


