

safe_deparse = function(expr){
    res = deparse(expr, width.cutoff = 500)
    do.call(paste0, as.list(res))
}

add_names_to_quoted_list = function(expr_list){
    all_names = names(expr_list)
    if(is.null(all_names)){
        names(expr_list) = c("", vapply(expr_list[-1], safe_deparse, FUN.VALUE = character(1)))
        return(expr_list)
    }
    for(i in seq_along(expr_list)){
        if(all_names[i]==""){
            names(expr_list)[i] = safe_deparse(expr_list[[i]])
        }
    }

    expr_list
}


# expr - expression as after 'substitute'
# symbols - named list  - names will be substituted with values
substitute_symbols = function(substitute_result, symbols) {
    eval(bquote(substitute(.(substitute_result), symbols)))
}


substitute_symbols = function(substitute_result, symbols) {
    eval(bquote(substitute(.(substitute_result), symbols)))
}

is_regex = function(txt){
    startsWith(txt, "^") | endsWith(txt, "$")
}

# 'expr' is result of substitute with assumption
# that first argument is data.frame.
# When we have huge constant instead of
# variable in the expression, then, in the case of the error
# we have long lag to print this error in the console.
# This function is workaround for this issue.
eval_in_parent_frame = function(data, expr, frame){
    `._***data***` = NULL # to pass CRAN check
    if(!is.data.table(data)){
        data = as.data.table(data)
    }
    assign("._***data***", data, envir = frame)
    on.exit({
        rm(`._***data***`, envir = frame)
    })
    expr[[2]] = quote(`._***data***`)
    eval(expr, envir = frame)
}

# j_expr - list from j-expression
# envir - environement where we will evaluate lhs of :=
add_names_from_walrus_assignement = function(j_expr, envir){
    j_length = length(j_expr)
    if(is.null(names(j_expr))){
        names(j_expr) = rep("", j_length)
    }
    # parse walrus (a := b) assignement
    for(k in seq_len(j_length)){
        if(is.call(j_expr[[k]]) && identical(j_expr[[k]][[1]], as.symbol(":="))){
            name_expr = j_expr[[k]][[2]]
            j_expr[[k]] = j_expr[[k]][[3]]
            if(is.call(name_expr)){
                names(j_expr)[k] = eval(name_expr, envir = envir)
            } else {
                if(is.character(name_expr)){
                    names(j_expr)[k] = name_expr
                } else {
                    names(j_expr)[k] = safe_deparse(name_expr)
                }
            }
        }
    }

    j_expr
}

add_names_from_single_symbol = function(j_expr){
    if(length(j_expr)>1){
        for(k in seq_along(j_expr)){
            if(names(j_expr)[k]=="" && is.symbol(j_expr[[k]])){
                names(j_expr)[k] = as.character(j_expr[[k]])
            }
        }
    }
    j_expr
}

# @param lst list or vector
# @param elem single number of replaced element
# @param new_elems list or vector which should be inserted
# substitute_list_elem = function(lst, elem, new_elems){
#         c(
#             lst[seq_len(elem - 1)],
#             new_elems,
#             lst[-seq_len(elem)]
#         )
# }

# @param lst list or vector
# @param elem single number of replaced element
# @param new_elems list or vector which should be inserted
# insert_list_elem_after = function(lst, elem, new_elems){
#     c(
#         lst[seq_len(elem)],
#         new_elems,
#         lst[-seq_len(elem)]
#     )
# }

