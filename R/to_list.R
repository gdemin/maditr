#' Apply an expression to each element of a list or vector
#'
#' - `to_list` always returns a list, each element of which is the
#' result of expression `expr` on the elements of data. By
#' default, NULL's will be removed from the result. You can change this behavior
#' with `skip_null` argument.
#' - `to_vec` is the same as `to_list` but tries to convert its result
#' to vector via [unlist][base::unlist].
#' - `to_df` and `to_dfr` try to combine its results to data.table by rows.
#' - `to_dfc` tries to combine its result to data.table by columns.
#' ```
#' ```
#' Expression can use predefined variables: '.x' is a value of current list
#' element, '.name' is a name of the element and '.index' is sequential number
#' of the element.
#'
#' @param data data.frame/list/vector
#' @param expr expression or function. Expression can use predefined variables:
#'   '.x' is a value of current list element, '.name' is a name of the element
#'   and '.index' is sequential number of the element.
#' @param skip_null logical Should we skip NULL's from result? Default is TRUE
#' @param trace FALSE by default. Should we report progress during execution?
#'   Possible values are TRUE, FALSE, "pb" (progress bar) or custom expression in 'quote', e. g. 'quote(print(.x))'.
#'   Expression can contain '.x', '.name', and '.index' variables.
#' @param ... further arguments provided if 'expr' is function.
#' @param trace_step integer. 1 by default.  Step for reporting progress. Ignored if 'trace' argument is equal to FALSE.
#' @param recursive logical. Should unlisting be applied to list components of x? For details see [unlist][base::unlist].
#' @param use.names logical. TRUE by default. Should names of source list be
#'   preserved? Setting it to FALSE in some cases can greatly increase
#'   performance. For details see [unlist][base::unlist].
#' @param idvalue expression for calculation id column. Usually it is just
#'   unquoted symbols: one of the '.name', '.index' or '.x'.
#' @param idname character, 'item_id' by default. Name for the id column.
#'
#' @return 'to_list' returns list, 'to_vec' tries to return vector and other functions return data.table
#' @export
#'
#' @examples
#' 1:5 %>%
#'     to_list(rnorm(n = 3, .x))
#'
#' # or in 'lapply' style
#' 1:5 %>%
#'     to_list(rnorm, n = 3) %>%
#'     to_vec(mean)
#'
#' # or use an anonymous function
#' 1:5 %>%
#'     to_list(function(x) rnorm(3, x))
#'
#' # Use to_vec() to reduce output to a vector instead
#' # of a list:
#' # filtering - return only even numbers
#' to_vec(1:10, if(.x %% 2 == 0) .x)
#'
#' # filtering - calculate mean only on the numeric columns
#' to_vec(iris, if(is.numeric(.x)) mean(.x))
#'
#' # mean for numerics, number of distincts for others
#' to_vec(iris, if(is.numeric(.x)) mean(.x) else uniqueN(.x))
#'
#' # means for Sepal
#' to_vec(iris, if(startsWith(.name, "Sepal")) mean(.x))
#'
#' # A more realistic example: split a data frame into pieces, fit a
#' # model to each piece, summarise and extract R^2
#' mtcars %>%
#'     split(.$cyl) %>%
#'     to_list(summary(lm(mpg ~ wt, data = .x))) %>%
#'     to_vec(.x$r.squared)
#'
#' # If each element of the output is a data frame, use
#' # to_df to row-bind them together:
#' mtcars %>%
#'     split(.$cyl) %>%
#'     to_list(lm(mpg ~ wt, data = .x)) %>%
#'     to_df(c(cyl = .name, coef(.x)))
#'
#' \dontrun{
#' # read all csv files in "data" to data.frame
#' all_files = dir("data", pattern = "csv$", full.names = TRUE) %>%
#'     to_df(fread,
#'           idvalue = basename(.x),
#'           idname = "filename",
#'           trace = "pb"
#'           )
#' }
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

    ## progress bar and trace
    if(identical(trace, "pb")){
        # progress bar
        ._data_length = length(data)
        pbar = utils::txtProgressBar(min = 0, max = ._data_length, style = 3)
        on.exit({
            utils::setTxtProgressBar(pbar, ._data_length)
            close(pbar)
            })

        # progress bar
        trace_expr = substitute({
            utils::setTxtProgressBar(pbar, min(.index - 1, ._data_length))
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

    if(!is.null(trace_expr) && (trace_step>1)){
        trace_expr = substitute(
            {
                if(.index %% trace_step == 0) trace_expr
            },
            list(trace_expr = trace_expr, trace_step = trace_step)
        )
    }

    ### main expression
    ._indexes = seq_along(data)
    ._names = names(data)
    names(._indexes) = ._names
    if(is.null(._names)) ._names = rep("", length(data))
    data = force(data) # for new version of magrittr
    if(is.symbol(expr_expr) || (length(expr_expr)>1 && identical(as.character(expr_expr[[1]]), "function"))){
        if(is.null(trace_expr)){
            # simple lapply case
            res = lapply(data, expr, ...)
        } else {
            # simple lapply case with trace
            expr_expr = substitute(expr(.x, ...))
            expr_expr = eval.parent(substitute({function(.index, ...)
            {
                .x = data[[.index]]
                .value = data[[.index]]
                .name = ._names[[.index]]
                trace_expr
                expr_expr
            }}))
            res = lapply(._indexes, expr_expr, ...)
        }
    } else {
        # expression
        expr_expr = eval.parent(substitute({function(.index, ...)
        {
            .x = data[[.index]]
            .value = data[[.index]]
            .name = ._names[[.index]]
            trace_expr
            expr_expr
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
    res = eval.parent(substitute(maditr::to_list(data, expr, ..., skip_null = skip_null, trace = trace, trace_step = trace_step)))
    unlist(res, recursive = recursive, use.names = use.names)

}


#' @rdname to_list
#' @export
to_df = function(data,
                 expr = NULL,
                 ...,
                 trace = FALSE,
                 trace_step = 1L,
                 idvalue = NULL,
                 idname = "item_id"){
    res = eval.parent(
        substitute(
            maditr::to_list(data,
                    expr,
                    ...,
                    trace = trace,
                    trace_step = trace_step,
                    skip_null = FALSE)
        )
    )
    for(i in seq_along(res)){
        if(!is.null(res[[i]]) && !is.list(res[[i]])){
            res[[i]] = as.data.table(as.list(res[[i]]))
        }
    }
    idvalue_expr = substitute(idvalue)
    if(!is.null(idvalue_expr)){
        idvalue = eval.parent(substitute(maditr::to_list(data, expr = (idvalue), skip_null = FALSE)))
        for(i in seq_along(data)){
            if(!is.null(res[[i]]) && !is.null(idvalue[[i]])){
                res[[i]][[idname]] = idvalue[[i]]
            }
        }
    }
    rbindlist(res, use.names=TRUE, fill=TRUE)
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
    res = eval.parent(
        substitute(
            maditr::to_list(data,
                    expr,
                    ...,
                    trace = trace,
                    trace_step = trace_step)
            )
        )
    as.data.table(res)
}



