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
let_all.etable = function(data,
                         ...,
                         by,
                         keyby,
                         .SDcols,
                         suffix = TRUE,
                         sep = "_",
                         i
){
    data_class = class(data)
    data = as.data.table(data)
    res = eval.parent(
        substitute(maditr::let_all(data,
                                  ...,
                                  by = by,
                                  keyby = keyby,
                                  .SDcols = .SDcols,
                                  suffix = suffix,
                                  sep = sep,
                                  i = i
        )
        )
    )
    setDF(res)
    class(res) = data_class
    res
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

    # if data is expression we want to calculate it only once
    calc_data = data

    #################
    ## naming
    ## suffix = FALSE - prefix, if TRUE it will be suffix
    ## no names
    ## if single symbol expr all names will be left as is
    ## if multiple symbol expr names will be prefixed/suffixed with this symbol "_"
    ## if complex expr and there is no names then original names will be left as is
    ## duplicated names will be made unique
    #
    # we need to know resulting names
    # this is simplest method to escape complexities with by, keyby and SDCols interaction
    one_row = as.data.table(calc_data[1,, drop = FALSE])
    ._orig_names = eval.parent(substitute(maditr::query(one_row, list(._res_names = names(.SD)),
                                                        by = by,
                                                        keyby = keyby,
                                                        .SDcols = .SDcols
    )))[["._res_names"]]
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
    ._data_names = names(calc_data)
    names(j_expr) = NULL
    ###
    for(j in seq_along(j_expr)){
        expr = j_expr[[j]]


        if((is.symbol(expr) && !identical(expr, quote(.name)) &&
            !identical(expr, quote(.index)) &&
            !identical(expr, quote(.x)) &&
            !identical(expr, quote(.value)) &&
            !identical(expr, quote(.N)) &&
            !identical(expr, quote(.GRP))) ||
           (length(expr)>1 && as.character(expr[[1]]) == "function")){
            # let_all(data, scale)
            # let_all(data, function(x) scale(x))
            expr = substitute(lapply(.SD, expr))
        } else {
            # let_all(data, scale(.x))
            expr = substitute({
                # TODO possible errors because names(.SD) don't include 'by' variables
                .data_names = names(.SD)
                lapply(.data_names, function(.name) {
                .value = get(.name)
                .x = get(.name)
                .index = match(.name, .data_names)
                expr
            })
            })
        }

        if(identical(._all_names[[j]], ._orig_names)){
            j_expr[[j]] = substitute(
                {

                    res = expr
                    # if expression returns NULL we leave this variable unchanged
                    empty = which(vapply(res, is.null, FUN.VALUE = logical(1)))
                    if(length(empty)>0) {
                        res[empty] = .SD[ ,empty, with = FALSE]
                    }
                    res
                }
            )
            next()
        }

        ##################
        ._new_names = ._all_names[[j]]
        # let_all(data, my_new_name = scale(.x))
        j_expr[[j]] = substitute(
            {
                res = expr
                # if expression returns NULL we should skip this variable
                empty = which(vapply(res, is.null, FUN.VALUE = logical(1)))
                if(length(empty)>0) {
                    res[empty] = NA
                    lapply(._new_names[empty], assign, value = NA, envir = to_drop)
                }
                res
            }
        )

    }

    ####
    if(length(j_expr)>1){
        j_expr = as.call(c(list(quote(c)), j_expr))
    } else {
        j_expr = j_expr[[1]]
    }
    ####
    ._all_names = unlist(._all_names, recursive = TRUE, use.names = FALSE)

    expr = substitute(maditr::query_if(calc_data,
                                       i,
                                       (._all_names) := j_expr,
                                       by = by,
                                       keyby = keyby,
                                       .SDcols = .SDcols
    )
    )
    data_names = names(calc_data)
    parent_frame = parent.frame()
    # expr = eval(substitute(preproc_query_if(data_names, expr, parent_frame)))
    expr = preproc_query_if(data_names, expr, parent_frame)
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

    # if data is expression we want to calculate it only once
    calc_data = data

    ._data_names = names(calc_data)
    j_names = names(j_expr)
    names(j_expr) = NULL
    for(j in seq_along(j_expr)){
        expr = j_expr[[j]]
        expr = substitute_symbols(expr, list(
            '.value' = quote(.SD[[.name]]),
            '.x' = quote(.SD[[.name]]),
            '.index' = substitute(match(.name, ._data_names))
        ))

        if((is.symbol(expr) && !identical(expr, quote(.name)) &&
            !identical(expr, quote(.N)) &&
            !identical(expr, quote(.GRP))) ||
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

        ##########################
        j_expr[[j]] = substitute({
            res = expr
            names(res) = name_expr
            not_empty = which(!vapply(res, is.null, FUN.VALUE = logical(1)))
            res[not_empty]
        })

    }
    if(length(j_expr)>1){
        j_expr = as.call(c(list(quote(c)), j_expr))
    } else {
        j_expr = j_expr[[1]]
    }
    ####

    expr = substitute(maditr::query_if(calc_data, i, j_expr,
                               by = by,
                               keyby = keyby,
                               .SDcols = .SDcols)
    )
    data_names = names(calc_data)
    expr = preproc_query_if(._data_names, expr, parent_frame)
    res = eval.parent(expr)
    setnames(res, make.unique(names(res)))
    res
}



