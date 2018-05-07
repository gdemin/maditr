#' @export
dt_select = function(data, ...){
    if(!is.data.frame(data)) stop("dt_select: 'data' should be data.frame or data.table")
    if(!is.data.table(data)){
        data = as.data.table(data)
    }
    var_list = substitute(list(...))
    all_indexes = as.list(seq_along(data))
    names(all_indexes) = colnames(data)
    var_indexes = eval(substitute(select), all_indexes, parent.frame())
    data[, var_indexes, with = FALSE, drop = FALSE]
}

#' @export
dt_mutate = function(data, ..., by, keyby){
    eval.parent(substitute(let(data, ...,
                               by = by,
                               keyby = keyby,
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
                               on = NULL))
                )
}

#' @export
dt_mutate_if = function(data, i, ..., by, keyby){
    eval.parent(substitute(let_if(data, i, ...,
                               by = by,
                               keyby = keyby,
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
                               on = NULL))
    )
}

#' @export
dt_summarize = function(data, ..., by, keyby, fun = NULL){
    eval.parent(substitute(take(data, ...,
                               by = by,
                               keyby = keyby,
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
                               on = NULL))
    )
}

#' @export
dt_arrange = function(data, ..., na.last = FALSE){
    if(!is.data.frame(data)) stop("dt_arrange: 'data' should be data.frame or data.table")
    if(!is.data.table(data)){
        data = as.data.table(data)
    }
    setorder(data, ...)
    data
}

#' @export
dt_filter = function(data, ...){
    if(!is.data.frame(data)) stop("dt_arrange: 'data' should be data.frame or data.table")
    if(!is.data.table(data)){
        data = as.data.table(data)
    }
}
