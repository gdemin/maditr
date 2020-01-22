insert_empty_i = function(expr){
    call_list = as.list(expr)
    i_position = which(names(call_list) %in% "i")
    if(length(i_position)>0){
        call_list =  c(call_list[1:2], call_list[i_position], call_list[-c(1:2, i_position)])
    } else {
        call_list =  c(call_list[1:2], list(substitute()), call_list[-(1:2)])
    }


    as.call(call_list)
}

safe_deparse = function(expr){
    res = deparse(expr, width.cutoff = 500)
    do.call(paste0, as.list(res))
}

add_names_to_quoted_list = function(expr){
    expr_list = as.list(expr)
    all_names = names(expr_list)
    if(is.null(all_names)){
        names(expr_list) = c("", vapply(expr_list[-1], safe_deparse, FUN.VALUE = character(1)))
    } else {
        for(i in seq_along(expr_list)[-1]){
            if(all_names[i]==""){
                names(expr_list)[i] = safe_deparse(expr_list[[i]])
            }
        }
    }
    as.call(expr_list)
}


# expr - expression as after 'substitute'
# symbols - named list  - names will be substituted with values
substitute_symbols = function(substitute_result, symbols) {
    eval(bquote(substitute(.(substitute_result), symbols)))
}

add_brackets_to_i = function(expr){
    if(!identical(expr[[3]], substitute())){
        expr[[3]] = bquote((.(expr[[3]])))
    }
    expr
}

is_regex = function(txt){
    startsWith(txt, "^") | endsWith(txt, "$")
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

