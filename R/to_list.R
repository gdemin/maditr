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
to_list = function(data,
                   expr = NULL,
                   ...,
                   skip_null = TRUE,
                   trace = FALSE,
                   trace_step = 1L
){
    expr_expr = substitute(expr)
    if(is.null(expr_expr)) return(as.list(data))
    is.numeric(trace_step) || stop("'to_list': non-numeric 'trace_step' argument.")
    trace_step>=1 || stop("'to_list': 'trace_step' argument should be greater or equal to one.")
    expr_expr = substitute_symbols(expr_expr, list(
        '.value' = quote(data[[.index]]),
        '.x' = quote(data[[.index]]),
        '.name' = quote(._names[[.index]])
    ))
    ._indexes = seq_along(data)
    ._names = names(data)
    names(._indexes) = ._names
    if(is.null(._names)) ._names = rep("", length(data))

    ## progress bar and trace
    if(identical(trace, "pb")){
        # progress bar
        ._data_length = length(data)
        pbar = txtProgressBar(min = 0, max = ._data_length, style = 3)
        on.exit(close(pbar))

        # progress bar
        trace_expr = quote({
            setTxtProgressBar(pbar, min(.index, ._data_length))
        })


    } else if(isTRUE(trace)){
        # info
        trace_expr = quote(cat(as.character(Sys.time()), " ",
                               .index, ": ",
                               .name, " ",
                               if(is.atomic(.x) && length(.x)==1 && object.size(.x)<400) .x,
                               "\n", sep = ""))

    } else if(is.null(trace) || isFALSE(trace)){

        # no tracing
        trace_expr = NULL

    } else {
        # custom tracing
        trace_expr = trace
    }

    if(!is.null(trace_expr)){
        trace_expr = substitute_symbols(trace_expr, list(
            '.value' = quote(data[[.index]]),
            '.x' = quote(data[[.index]]),
            '.name' = quote(._names[[.index]])
        ))
        if(trace_step>1)
            trace_expr = substitute({
                if(.index %% trace_step == 0) trace_expr
            },
            list(trace_expr = trace_expr, trace_step = trace_step)
            )
    }

    ### main expression
    if(is.symbol(expr_expr) ||
       (length(expr_expr)>1 && identical(as.character(expr_expr[[1]]), "function"))
       ){
        if(is.null(trace_expr)){
            # simple lapply case
            res = lapply(data, expr, ...)
        } else {
            # simple lapply case with trace
            expr_expr = eval(substitute({function(.index, ...)
            {
                .res = expr(data[[.index]], ...)
                trace_expr
                .res
            }}))
            res = lapply(._indexes, expr_expr, ...)

        }
    } else {
        # expression
        expr_expr = eval(substitute({function(.index, ...)
        {
            .res = expr_expr
            trace_expr
            .res
        }}))
        res = lapply(._indexes, expr_expr, ...)
    }

    #######
    if(skip_null){
        nulls = vapply(res, is.null, FUN.VALUE = logical(1), USE.NAMES = FALSE)
        res = res[!nulls]
    }
    res
}

#' @rdname to_list
#' @export
to_vec = function(data,
                  expr = NULL,
                  ...,
                  skip_null = TRUE,
                  trace = FALSE,
                  trace_step = 1L,
                  recursive = TRUE,
                  use.names = TRUE){
    res = eval.parent(substitute(to_list(data, expr, ..., skip_null = skip_null, trace = trace, trace_step = trace_step)))
    unlist(res, recursive = recursive, use.names = use.names)

}


#' @rdname to_list
#' @export
to_df = function(data,
                 expr = NULL,
                 ...,
                 trace = FALSE,
                 trace_step = 1L,
                 idcol = NULL){
    res = eval.parent(substitute(to_list(data, expr, ..., trace = trace, trace_step = trace_step)))
    # TODO need to think about assigning id_col for case of reading multiple files
    rbindlist(res, use.names=TRUE, fill=TRUE, idcol = idcol)

}

#' @rdname to_list
#' @export
to_dfr = to_df

#' @rdname to_list
#' @export
to_dfc = function(data,
                  expr = NULL,
                  ...,
                  trace = FALSE,
                  trace_step = 1){
    res = eval.parent(substitute(to_list(data, expr, ..., trace = trace, trace_step = trace_step)))
    as.data.table(res)
}



