insert_empty_i = function(expr){
    call_list = as.list(expr)
    call_list =  c(call_list[1:2], list(substitute()), call_list[-(1:2)])
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
