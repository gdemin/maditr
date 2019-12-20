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
to_list = function(data, expr = NULL, skip_null = TRUE,
                   pb = FALSE,
                   info = NULL,
                   pb_step = 1
                   ){
    expr = substitute(expr)
    (pb && info) && stop("'to_list': both 'pb' and 'info' are set to TRUE. We can show only one of them - progress bar or info.")
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
    ## progress bar and info
    if(isTRUE(pb)){
        pbar = txtProgressBar(min = 0, max = length(data), style = 3)
        ._counter = 0
        on.exit(close(pbar))
    }
    if(!is.null(info)){
        if(isTRUE(info)){
            info_expr = quote(cat(Sys.time(), ": ",
                                  .index, ": ",
                                  .name, " ",
                                  if(is.atomic(.item) && length(.item)==1 && object.size(.item)<400) .item,
                                  "\n", sep = ""))
        }
    }
    if(is.symbol(expr) && !identical(expr, quote(.index))){
        fun = eval(substitute_symbols(quote(function(.index)
        {
            expr(data[[.index]])
        })), list(expr = expr))
    } else {
        fun = eval(substitute_symbols(quote(function(.index) expr), list(expr = expr)))

    }
    res = lapply(._indexes, fun)
    if(skip_null){
        nulls = vapply(res, is.null, FUN.VALUE = logical(1), USE.NAMES = FALSE)
        res = res[!nulls]
    }
    res
}

#' @rdname to_list
#' @export
to_vec = function(data, expr = NULL, skip_null = TRUE,
                  pb = FALSE,
                  info = NULL,
                  pb_step = 1,
                  recursive = TRUE,
                  use.names = TRUE){
     res = eval.parent(substitute(to_list(data, expr, skip_null = skip_null, pb = pb, info = info, pb_step = pb_step)))
     unlist(res, recursive = recursive, use.names = use.names)

}


#' @rdname to_list
#' @export
to_df = function(data, expr = NULL,
                 pb = FALSE,
                 info = NULL,
                 pb_step = 1,
                 idcol = NULL){
    res = eval.parent(substitute(to_list(data, expr, skip_null = TRUE, pb = pb, info = info, pb_step = pb_step)))
    rbindlist(res, use.names=TRUE, fill=TRUE, idcol = idcol)

}

#' @rdname to_list
#' @export
to_dfr = to_df

#' @rdname to_list
#' @export
to_dfc = function(data, expr = NULL,
                  pb = FALSE,
                  info = NULL,
                  pb_step = 1){
    res = eval.parent(substitute(to_list(data, expr, skip_null = TRUE, pb = pb, info = info, pb_step = pb_step)))
    as.data.table(res)
}



