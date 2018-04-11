#' Title
#'
#' @param data
#' @param i
#' @param ...
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
#' @param autoname
#' @param fun
#'
#' @return
#' @export
#'
#' @examples
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
                fun
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
                   fun
){
    if(!is.data.frame(data)) stop("take/take_if: 'data' should be data.frame or data.table")
    if(!is.data.table(data)){
        eval.parent(substitute(setDT(data)))
    }
    call_expr = sys.call()
    call_expr[[1]] = as.symbol("[")
    ### remove autoname argument
    call_list = as.list(call_expr)
    if("autoname" %in% names(call_list)) {
        call_list = call_list[!(names(call_list) %in% "autoname")]
    }
    ###########
    j_expr = substitute(list(...))
    j_length = length(j_expr) - 1
    if(j_length>0){
        if(autoname){
            j_expr = add_names_to_quoted_list(j_expr)
        }
        i_position = 3
        call_list = call_list[-(seq_len(j_length) + i_position)]
        call_list = c(call_list[1:3], list(j_expr), call_list[-(1:i_position)])
        call_expr = as.call(call_list)
    }
    eval.parent(call_expr)
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
    if(!is.data.table(data)){
        eval.parent(substitute(setDT(data)))
    }
    call_expr = sys.call()
    call_expr[[1]] = as.symbol("[")
    call_list = as.list(call_expr)
    j_list = as.list(substitute(list(...)))[-1]
    j_length = length(j_list)
    if(j_length>0){
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
            eval.parent(call_expr)
        }

    } else {
        eval.parent(call_expr)
    }
    data
}
