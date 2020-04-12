###################### let_all #################

#' @export
#' @rdname let_if
let_all = function(data,
                   ...,
                   by,
                   keyby,
                   .SDcols,
                   suffix = TRUE,
                   sep = "_",
                   i
){
    UseMethod("let_all")
}

#' @export
let_all.data.frame = function(data,
                           ...,
                           by,
                           keyby,
                           .SDcols,
                           suffix = TRUE,
                           sep = "_",
                           i
){
    j_expr = substitute(list(...))
    j_expr = as.list(j_expr)[-1]
    (length(j_expr) == 0) && stop("'let_all' - missing expressions. You should provide at least one expression.")


    j_expr = add_names_from_walrus_assignement(j_expr, envir = parent.frame())
    j_expr = add_names_from_single_symbol(j_expr)



    #################
    ## naming
    ## suffix = FALSE - prefix, if TRUE it will be suffix
    ## no names
    ## if single symbol expr all names will be left as is
    ## if multiple symbol expr names will be prefixed/suffixed with this symbol "_"
    ## if complex expr and there is no names then original names will be left as is
    ## duplicated names will be made unique
    # we need to know resulting names
    # this is simplest method to escape complexities with by, keyby and SDCols interaction
    one_row = as.data.table(data[1,, drop = FALSE])
    ._orig_names = eval.parent(substitute(one_row[, list(._res_names = names(.SD)),
                                                 by = by,
                                                 keyby = keyby,
                                                 .SDcols = .SDcols
                                                 ]))[["._res_names"]]
    j_expr_names = names(j_expr)
    j_expr_names[!(j_expr_names %in% "")] = make.unique(j_expr_names[!(j_expr_names %in% "")])
    ._all_names = lapply(j_expr_names, function(curr_name){
        if(curr_name == "") return(._orig_names)
        if(suffix){
            paste(._orig_names, curr_name, sep = sep)
        } else {
            paste(curr_name, ._orig_names, sep = sep)
        }
    })


    to_drop = new.env()
    ###
    j_expr = lapply(seq_along(j_expr), function(._j_expr_index){
        # for further substitution
        ._all_names = ._all_names
        to_drop = to_drop
        ._j_expr_index_internal = ._j_expr_index
        ####################
        expr = j_expr[[._j_expr_index]]
        expr = substitute_symbols(expr, list(
            '.value' = quote(.SD[[.name]]),
            '.x' = quote(.SD[[.name]]),
            '.index' = quote(match(.name, ._all_names))
        ))

        if((is.symbol(expr) && !identical(expr, quote(.name))) ||
           (length(expr)>1 && as.character(expr[[1]]) == "function")){
            # let_all(data, scale)
            # let_all(data, function(x) scale(x))
            expr = substitute(lapply(.SD, expr))
        } else {
            # let_all(data, scale(.x))
            expr = substitute(lapply(names(.SD), function(.name) expr))
        }

        # for let_all(iris, if(startsWith(.name, "Sepal")) mean(.x), if (startsWith(.name, "Petal")) uniqueN(.x))
        # we need to get something like this:
        # all_names := c({
        #     res = lapply(names(.SD), function(.name) if (startsWith(.name,
        #                                                             "Sepal")) mean(.SD[[.name]]))
        #     empty_results = lengths(res)==0
        #     if(any(empty_results)){
        #         res[empty_results] = .SD[empty_results]
        #     }
        #     res
        # }, {
        #     res = lapply(names(.SD), function(.name) if (startsWith(.name,
        #                                                             "Petal")) uniqueN(.SD[[.name]]))
        #     empty_results = lengths(res)==0
        #     if(any(empty_results)){
        #         res[empty_results] = .SD[empty_results]
        #     }
        #     res
        # })
        if(identical(._all_names[[._j_expr_index]], ._orig_names)){
            substitute({
                res = expr

                # if expression return NULL we leave this variable unchanged
                empty_results = which(vapply(res, is.null, FUN.VALUE = logical(1)))
                if(length(empty_results)>0){
                    res[empty_results] = .SD[,empty_results, with = FALSE]
                }
                res
            })
        } else {
            substitute({
                res = expr

                # if expression return NULL we leave this variable unchanged
                empty_results = which(vapply(res, is.null, FUN.VALUE = logical(1)))
                if(length(empty_results)>0){
                    # empty = ""
                    #class(empty) = "to_drop"
                    res[empty_results] = NA
                    lapply(._all_names[[._j_expr_index_internal]][empty_results], assign, value = NA, envir = to_drop)
                }
                res
            })
        }

    })

    ####
    if(length(j_expr)>1){
        j_expr = as.call(c(list(quote(c)), j_expr))
    } else {
        j_expr = j_expr[[1]]
    }
    ####
    ._all_names = make.unique(unlist(._all_names, use.names = FALSE, recursive = TRUE))
    if(is.data.table(data)){
        expr = substitute(data[i, (._all_names) := j_expr,
                               by = by,
                               keyby = keyby,
                               .SDcols = .SDcols])
    } else {
        expr = substitute(as.data.table(data)[i, (._all_names) := j_expr,
                                              by = by,
                                              keyby = keyby,
                                              .SDcols = .SDcols])

    }
    print(expr)
    res = eval.parent(expr)
    if(length(to_drop)>0){
        res[,(names(to_drop)):=NULL]
    }
    res
}



###################### take_all #################

#' @export
#' @rdname let_if
take_all = function(data,
                    ...,
                    by,
                    keyby,
                    .SDcols,
                    suffix = TRUE,
                    sep = "_",
                    i
){
    UseMethod("take_all")
}

#' @export
take_all.data.frame = function(data,
                               ...,
                               by,
                               keyby,
                               .SDcols,
                               suffix = TRUE,
                               sep = "_",
                               i
){
    j_expr = substitute(list(...))
    j_expr = as.list(j_expr)[-1]
    (length(j_expr) == 0) && stop("'take_all' - missing expressions. You should provide at least one expression.")


    j_expr = add_names_from_walrus_assignement(j_expr, envir = parent.frame())
    j_expr = add_names_from_single_symbol(j_expr)


    #################
    ## naming
    ## suffix = FALSE - prefix, if TRUE it will be suffix
    ## no names
    ## if single symbol expr all names will be left as is
    ## if multiple symbol expr names will be prefixed/suffixed with this symbol "_"
    ## if complex expr and there is no names then original names will be left as is
    ## duplicated names will be made unique (??)


    ._all_names = names(data)
    j_names = names(j_expr)
    names(j_expr) = NULL
    for(j in seq_along(j_expr)){
        expr = j_expr[[j]]
        expr = substitute_symbols(expr, list(
            '.value' = quote(.SD[[.name]]),
            '.x' = quote(.SD[[.name]]),
            '.index' = substitute(match(.name, ._all_names))
        ))

        if((is.symbol(expr) && !identical(expr, quote(.name))) ||
           (length(expr)>1 && as.character(expr[[1]]) == "function")){
            # take_all(data, mean)
            # take_all(data, function(x) mean(x, na.rm = TRUE))
            expr = substitute(lapply(.SD, expr))
        } else {
            # take_all(data, mean(.x))
            expr = substitute(lapply(names(.SD), function(.name) expr))
        }
        curr_name = j_names[j]
        if(curr_name != ""){
            if(suffix){
                name_expr = substitute(paste(names(.SD), curr_name, sep = sep))
            } else {
                name_expr = substitute(paste(curr_name, names(.SD), sep = sep))
            }
        } else {
            name_expr = substitute(names(.SD))
        }
        #########################
        # we need to get something like this:
        # c({
        #     res = lapply(names(.SD), function(.name) if (startsWith(.name,
        #                                                             "Sepal")) mean(.SD[[.name]]))
        #     names(res) = paste0("", names(.SD))
        #     res = res[lengths(res) > 0]
        #     res
        # }, {
        #     res = lapply(names(.SD), function(.name) if (startsWith(.name,
        #                                                             "Petal")) uniqueN(.SD[[.name]]))
        #     names(res) = paste0("", names(.SD))
        #     res = res[lengths(res) > 0]
        #     res
        # })
        ##########################
        j_expr[[j]] = substitute({
            res = expr
            names(res) = name_expr
            empty_results = which(vapply(res, is.null, FUN.VALUE = logical(1)))
            if(length(empty_results)>0) res[,(empty_results):=NULL]
            res
        })

    }
    if(length(j_expr)>1){
        j_expr = as.call(c(list(quote(c)), j_expr))
    } else {
        j_expr = j_expr[[1]]
    }
    ####

    expr = substitute(query_if(data, i, j_expr,
                               by = by,
                               keyby = keyby,
                               .SDcols = .SDcols)
    )
    # print(expr)
    res = eval.parent(expr)
    setnames(res, make.unique(names(res)))
    res
}



