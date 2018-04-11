#' Title
#'
#' @param data
#' @param i
#' @param j
#' @param by
#' @param keyby
#' @param with
#' @param nomatch
#' @param mult
#' @param roll
#' @param rollends
#' @param which
#' @param .SDcols
#' @param verbose
#' @param allow.cartesian
#' @param drop
#' @param on
#'
#' @return
#'
#' @examples
#'
#' @export
query = function(data,
                 j,
                 by,
                 keyby,
                 with = TRUE,
                 nomatch = getOption("datatable.nomatch"),
                 mult = "all",
                 roll = FALSE,
                 rollends = if (roll=="nearest") c(TRUE,TRUE)
                 else if (roll>=0) c(FALSE,TRUE)
                 else c(TRUE,FALSE),
                 which = FALSE,
                 .SDcols,
                 verbose = getOption("datatable.verbose"),                   # default: FALSE
                 allow.cartesian = getOption("datatable.allow.cartesian"),   # default: FALSE
                 drop = NULL,
                 on = NULL){
    call_expr = sys.call()
    call_expr[[1]] = as.symbol("query")
    call_expr = insert_empty_i(call_expr)
    eval.parent(call_expr)
}

#' @rdname query
#' @export
query_if = function(data,
                    i,
                    j,
                    by,
                    keyby,
                    with = TRUE,
                    nomatch = getOption("datatable.nomatch"),
                    mult = "all",
                    roll = FALSE,
                    rollends = if (roll=="nearest") c(TRUE,TRUE)
                                else if (roll>=0) c(FALSE,TRUE)
                                else c(TRUE,FALSE),
                    which = FALSE,
                    .SDcols,
                    verbose = getOption("datatable.verbose"),                   # default: FALSE
                    allow.cartesian = getOption("datatable.allow.cartesian"),   # default: FALSE
                    drop = NULL,
                    on = NULL){
    if(!is.data.frame(data)) stop("query/query_if: 'data' should be data.frame or data.table")
    if(!is.data.table(data)){
        eval.parent(substitute(setDT(data)))
    }
    call_expr = sys.call()
    call_expr[[1]] = as.symbol("[")
    eval.parent(call_expr)
}


