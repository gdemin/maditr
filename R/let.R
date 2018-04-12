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
take = function(data,
                ...,
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
                on = NULL,
                autoname = TRUE,
                fun = NULL
){
    call_expr = sys.call()
    call_expr[[1]] = as.symbol("take_if")
    call_expr = insert_empty_i(call_expr)
    eval.parent(call_expr)
}

#' @rdname take
#' @export
let = function(data,
               ...,
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
               on = NULL
){
    call_expr = sys.call()
    call_expr[[1]] = as.symbol("let_if")
    call_expr = insert_empty_i(call_expr)
    eval.parent(call_expr)
}

#' @rdname take
#' @export
take_if = function(data,
                   i,
                   ...,
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
                   on = NULL,
                   autoname = TRUE,
                   fun = NULL
){
    if(!is.data.frame(data)) stop("take/take_if: 'data' should be data.frame or data.table")
    call_expr = sys.call()
    if(!is.data.table(data)){
        data = as.data.table(data)
        call_expr[[2]] = as.symbol("data")
        curr_eval = eval
    } else {
        curr_eval = eval.parent
    }
    call_expr[[1]] = as.symbol("[")

    ### remove autoname argument
    call_list = as.list(call_expr)
    if(any(c("autoname", "fun") %in% names(call_list))) {
        call_list = call_list[!(names(call_list) %in% c("autoname", "fun"))]
    }
    ###########
    j_expr = substitute(list(...))
    j_length = length(j_expr) - 1
    i_position = 3
    if(j_length>0){
        if(autoname){
            j_expr = add_names_to_quoted_list(j_expr)
        }
        if(is.null(fun)){
            call_list = call_list[-(seq_len(j_length) + i_position)]
            call_list = c(call_list[1:3], list(j_expr), call_list[-(1:i_position)])
            call_expr = as.call(call_list)
        } else {
            call_list = call_list[-(seq_len(j_length) + i_position)]
            call_list = c(call_list[1:3], list(substitute(lapply(j_expr, fun))), call_list[-(1:i_position)])
            call_expr = as.call(call_list)
        }
    } else {
        if(!is.null(fun)){
            call_list = c(call_list[1:3], list(substitute(lapply(.SD, fun))), call_list[-(1:3)])
            call_expr = as.call(call_list)
        }
    }
    curr_eval(call_expr)
}


#' @rdname take
#' @export
let_if = function(data,
                  i,
               ...,
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
               on = NULL
){
    if(!is.data.frame(data)) stop("let/let_if: 'data' should be data.frame or data.table")
    call_expr = sys.call()
    if(!is.data.table(data)){
        data = as.data.table(data)
        call_expr[[2]] = as.symbol("data")
        curr_eval = eval
    } else {
        curr_eval = eval.parent
    }
    call_expr[[1]] = as.symbol("[")
    call_list = as.list(call_expr)
    j_list = as.list(substitute(list(...)))[-1]
    j_length = length(j_list)
    if(j_length==0) stop("let/let_if: please, provide at least one expression.")
    if(is.null(names(j_list))){
        names(j_list) = rep("", j_length )
    }
    # check for names
    all_names = names(j_list)
    for(i in seq_along(j_list)){
        if(all_names[i]==""){
            if(!identical(j_list[[i]][[1]], as.symbol(":="))){
                stop(sprintf("let/let_if: '%s' - incorrect expression. All unnamed expressions should use ':='.", safe_deparse(j_list[[i]])))
            }
        }
    }
    i_position = 3
    call_list = call_list[-(seq_len(j_length) + i_position)]
    call_list = c(call_list[1:3], list(NULL), call_list[-(1:i_position)]) # reserve position for j
    for(i in seq_along(j_list)){
        if(all_names[i]==""){
            call_list[[i_position + 1]] = j_list[[i]]
        } else {
            curr_name = all_names[i]
            curr_expr = j_list[[i]]
            call_list[[i_position + 1]] = substitute(`:=`(curr_name, curr_expr))
        }
        call_expr = as.call(call_list)
        curr_eval(call_expr)
    }

    data
}
