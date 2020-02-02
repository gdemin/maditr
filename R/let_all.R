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
let_all.default = function(data,
                           ...,
                           by,
                           keyby,
                           .SDcols,
                           suffix = FALSE,
                           sep = "_",
                           i
){
    is.data.frame(data) || stop("'let_all': 'data' should be data.frame or data.table")


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
    ._all_names = eval.parent(substitute(one_row[, list(._res_names = names(.SD)),
                                                 by = by,
                                                 keyby = keyby,
                                                 .SDcols = .SDcols
                                                 ]))[["._res_names"]]
    ._all_names = lapply(names(j_expr), function(curr_name){
        if(suffix){
            paste(._all_names, curr_name, sep = sep)
        } else {
            paste(curr_name, ._all_names, sep = sep)
        }
    })
    ._all_names = make.unique(unlist(._all_names, use.names = FALSE, recursive = TRUE))


    ###
    j_expr = lapply(seq_along(j_expr), function(j){
        expr = j_expr[[j]]
        expr = substitute_symbols(expr, list(
            '.value' = quote(.SD[[.name]]),
            '.x' = quote(.SD[[.name]]),
            # '.j' = quote(.SD[[.name]]),
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
        substitute({
            res = expr

            # if expression return NULL we leave this variable unchanged
            empty_results = lengths(res)==0
            if(any(empty_results)){
                res[empty_results] = .SD[,empty_results, with = FALSE]
            }
            res
        })

    })
    ####
    if(length(j_expr)>1){
        j_expr = as.call(c(list(quote(c)), j_expr))
    } else {
        j_expr = j_expr[[1]]
    }
    ####

    if(is.data.table(data)){
        expr = substitute(data[i, ._all_names := j_expr,
                               by = by,
                               keyby = keyby,
                               .SDcols = .SDcols])
    } else {
        expr = substitute(as.data.table(data)[i, ._all_names := j_expr,
                                              by = by,
                                              keyby = keyby,
                                              .SDcols = .SDcols])

    }
    print(expr)
    eval.parent(expr)
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
take_all.default = function(data,
                            ...,
                            by,
                            keyby,
                            .SDcols,
                            suffix = FALSE,
                            sep = "_",
                            i
){
    is.data.frame(data) || stop("'take_all': 'data' should be data.frame or data.table")
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
    j_expr = lapply(seq_along(j_expr), function(j){
        expr = j_expr[[j]]
        expr = substitute_symbols(expr, list(
            '.value' = quote(.SD[[.name]]),
            '.x' = quote(.SD[[.name]]),
            '.index' = quote(match(.name, ._all_names))
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
        curr_name = names(j_expr)[j]
        if(suffix){
            name_expr = substitute(paste(names(.SD), curr_name, sep = spr))
        } else {
            name_expr = substitute(paste(curr_name, names(.SD), sep = spr))
        }
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
        substitute({
            res = expr
            names(res) = name_expr
            res = res[lengths(res)>0]
            res
        })

    })
    if(length(j_expr)>1){
        j_expr = as.call(c(list(quote(c)), j_expr))
    } else {
        j_expr = j_expr[[1]]
    }
    ####

    if(is.data.table(data)){
        expr = substitute(data[i, j_expr,
                               by = by,
                               keyby = keyby,
                               .SDcols = .SDcols])
    } else {
        expr = substitute(as.data.table(data)[i, j_expr,
                                              by = by,
                                              keyby = keyby,
                                              .SDcols = .SDcols])

    }
    print(expr)
    res = eval.parent(expr)
    setnames(res, make.unique(names(res)))
    res
}



