#' Title
#'
#' @param data sa
#' @param i asa
#' @param ... sa
#' @param by as
#' @param keyby as
#' @param with as
#' @param nomatch as
#' @param mult as
#' @param roll as
#' @param rollends as
#' @param which as
#' @param .SDcols as
#' @param verbose as
#' @param allow.cartesian as
#' @param drop as
#' @param on as
#' @param autoname as
#' @param fun as
#'
#' @return sa
#' @export
#'
#' @examples
#' 1
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
    call_expr[[1]] = as.symbol("query_if")
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
    call_expr = sys.call()
    if(!is.data.table(data)){
        data = as.data.table(data)
        call_expr[[2]] = as.symbol("data")
        curr_eval = eval
    } else {
        curr_eval = eval.parent
    }
    call_expr[[1]] = as.symbol("[")
    curr_eval(call_expr)
}


