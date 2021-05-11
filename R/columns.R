#' Selects columns from the data set
#'
#' Variable ranges are supported, e. g. vs:carb. Alternatively, you can use
#' `%to%` instead of colon: `vs %to% carb`. Characters which start with '^' or
#' end with '$' considered as Perl-style regular expression patterns. For
#' example, '^Petal' returns all variables started with 'Petal'. 'Width$'
#' returns all variables which end with 'Width'. Pattern '^.' matches all
#' variables and pattern '^.*my_str' is equivalent to 'contains "my_str"'. See
#' examples.
#'
#' @param data data.table/data.frame data.frame will be automatically converted
#'   to data.table.
#' @param ... List of variables or name-value pairs of summary/modifications
#'   functions. The name will be the name of the variable in the result.
#'   Advantages of `:=` are multiassignment (`c("a", "b") := list(1,2)`)
#'   and parametric assignment (`(a) := 2`)
#'
#' @return data.table
#' @export
#' @examples
#' mtcars %>%
#'     columns(vs:carb, cyl)
#' mtcars %>%
#'     columns(-am, -cyl)
#'
#' # regular expression pattern
#' columns(iris, "^Petal") # variables which start from 'Petal'
#' columns(iris, "Width$") # variables which end with 'Width'
#' # move Species variable to the front.
#' # pattern "^." matches all variables
#' columns(iris, Species, "^.")
#' # pattern "^.*i" means "contains 'i'"
#' columns(iris, "^.*i")
#' columns(iris, 1:4) # numeric indexing - all variables except Species
#'
#' # variable expansion
#' dims = c("Width", "Length")
#' columns(iris, "Petal.{dims}")
#'
columns = function(data, ...){
    UseMethod("columns")
}


#' @export
columns.data.frame = function(data, ...){
    # if data is expression we want to calculate it only once
    data = force(data)
    data_names = colnames(data)
    parent_frame = parent.frame()
    var_indexes = select_columns(...,
                                 data_names = data_names,
                                 frame = parent_frame,
                                 combine = NULL,
                                 new = FALSE)

    if(is.data.table(data)){
        data[, var_indexes, with = FALSE]
    } else {
        data[, var_indexes, drop = FALSE]
    }

}

#' @rdname columns
#' @export
cols = columns

# here we find `columns` expression and replace it with data.table(...) or c(...)
replace_column_expr = function(expr, data_names, frame, combine = quote(data.table), new = FALSE){
    if(missing(expr)) return(missing_arg())
    if(is.call(expr) && length(expr)>1){
        curr = expr[[1]]
        if(identical(curr, quote(columns)) || identical(curr, quote(cols)) || identical(curr, quote(`%to%`))){
            if(identical(curr, quote(`%to%`))){
                expr = bquote(select_columns(.(expr)))# standalone a %to% b, without 'columns'
            } else {
                # 'columns(...)'
                expr[[1]] = quote(select_columns)
            }
            expr = as.call(c(as.list(expr),
                             list(data_names = data_names,
                                  frame = frame,
                                  combine = combine,
                                  new = new))
                           )
            expr = eval(expr)

        } else {
            res = lapply(as.list(expr),
                         replace_column_expr,
                         data_names = data_names,
                         frame = frame,
                         combine = combine,
                         new = new
                         )
            expr = as.call(res)
        }
    }
    expr
}

select_columns = function(..., data_names, frame, combine, new){
    var_list = substitute(list(...))
    all_symbols = create_list_with_symbols(data_names)
    var_names = all.vars(var_list)
    var_symbols = lapply(var_names, as.symbol)
    names(var_symbols) = var_names
    var_symbols = as.call(c(quote(list), var_symbols))

    # resolve all symbols with their value
    var_symbols = eval(var_symbols, envir = all_symbols, enclos = frame)
    var_list = substitute_symbols(var_list, c(var_symbols,
                                              list("%to%" = quote(`:`))
    ))
    var_list = expand_selectors(var_list, data_names, frame, new = new)
    ### we need convert symbols to indexes to make '-' work as exclusion operator
    all_indexes = as.list(seq_along(data_names))
    names(all_indexes) = data_names
    var_indexes = eval(var_list, all_indexes, frame)
    var_indexes = unique(unlist(var_indexes, recursive = TRUE, use.names = FALSE))
    if(is.null(combine)) return(var_indexes)
    res = all_symbols[var_indexes]
    as.call(c(combine, res))

}

expand_selectors = function(selected, data_names, frame, new){
    # expand text and regex
    selected = lapply(selected, function(item){
        # browser()
        if(is_range(item)) return(
            s_range_expand(item, df_names = data_names, frame = frame, new = new)
        )
        if(is.call(item) && length(item)>1) return(
            expand_selectors(item, data_names, frame, new = new)
        )

        item = s_regex_expand(item, data_names)
        item = s_text_expand(item, data_names, frame)
        item
    })
    as.call(selected)
}


###

create_list_with_symbols = function(data_names){
    res = lapply(data_names, as.symbol)
    names(res) = data_names
    res
}

create_list_with_index = function(data_names){
    res = as.list(seq_along(data_names))
    names(res) = data_names
    res
}

create_vec_expression = function(data_names){
    if(length(data_names)==1) return(as.symbol(data_names))
    res = lapply(unlist(data_names, recursive = TRUE, use.names = FALSE),
                 as.symbol)
    as.call(c(quote(c), res))
}

create_index_vec = function(res, df_names){
    match(res, df_names)
}

####
is_regex = function(txt){
    is.character(txt) && any(startsWith(txt, "^") | endsWith(txt, "$"))
}

is_range = function(expr){
    is.call(expr) && (identical(expr[[1]], quote(`:`)) | identical(expr[[1]], quote(`%to%`)))
}

s_regex_expand = function(expr, df_names){
    # TODO here we have possible unexpected behaviour - when expr is vec of several character
    # if one of them is regex all other will be considered as regex
    if(!is_regex(expr)) return(expr)
    res = lapply(expr, grep, x = df_names, perl = TRUE, value = TRUE)
    res = unlist(res, recursive = TRUE, use.names = FALSE)
    (length(res)==0) && stop(paste("'columns' - there are no variables which match regex(-s): ", paste(expr, collapse = ",")))
    create_vec_expression(res)
}

s_text_expand = function(expr, df_names, frame){
    if(!is.character(expr)) return(expr)
    res = eval(substitute(maditr::text_expand(item), list(item = expr)), envir = frame)
    # anyNA(res) && stop(paste("'columns' - variable(-s) not found: ",paste(expr, collapse = ",")))
    create_vec_expression(res)
}

# %to% expansion
# we can rely on indexing a:b -> 10:15, but in this case we cannot
# create new variables from ranges
s_range_expand = function(expr, df_names, frame, new = FALSE){
    if(!is_range(expr)) return(expr)
    from = expr[[2]]
    to = expr[[3]]
    from = as.character(from)
    to = as.character(to)
    first = match(from, df_names)[1]
    last = match(to, df_names)[1]
    if((is.na(first) && is.na(last)) && new){
        patt1 = gsub("^(.+?)([\\d]+)$", "\\1", from, perl = TRUE)
        patt2 = gsub("^(.+?)([\\d]+)$", "\\1", to, perl = TRUE)
        (patt1 == patt2) || stop("'columns range selection': start and end variables begin from different patterns: '", patt1, "', '", patt2,"'.")
        digits1 = gsub("^(.+?)([\\d]+)$", "\\2", from, perl = TRUE)
        digits2 = gsub("^(.+?)([\\d]+)$", "\\2", to, perl = TRUE)
        padding = 0
        if((substr(digits1,1,1)=="0" || substr(digits2,1,1)=="0") &&
           !(substr(digits1,1,1)=="0" && nchar(digits1)==1 && substr(digits2,1,1)!=0)){
            (nchar(digits1) == nchar(digits2)) ||
                   stop("'columns' range selection: invalid use of the '%to%' convention. For zero-padded numbers numeric part of the names must be the same length but: '",
                   digits1, ", '", digits2, "'.")
            padding = nchar(digits1)
        }
        digits1 = as.numeric(digits1)
        digits2 = as.numeric(digits2)
        (digits1<=digits2) || stop("'columns' range selection: name of start variables greater than name of end variables: '", from,"' > '",to,"'.")
        all_digits = digits1:digits2
        if(padding>0) all_digits = formatC(all_digits, width = padding, format = "d", flag = "0")
        res = paste0(patt1, all_digits)
    } else {
        (!is.na(first) &&  !is.na(last)) || stop("'columns' range selection: variables '", from, "' or '", to, "' are not found.")
        (last>=first) || stop( "'columns' range selection: '",to, "' located before '",from,"'. Did you mean '",to," %to% ",from,"'?")
        res = df_names[first:last]
    }
    create_vec_expression(res)
}




########################################################

#' @rdname columns
#' @export
rows = function(data, ...){
    UseMethod("rows")
}


#' @export
rows.data.frame = function(data, ...){
    curr_names = names(substitute(list(...)))
    if(!is.null(curr_names)){
        if(any(c("by", "keyby") %in% curr_names)){
            stop("'rows': you try to use 'by' or 'keyby'. Sorry, but grouped filtering is not yet supported.")
        }
        curr_names = curr_names[curr_names!=""][[1]]
        stop(sprintf("'rows': it seems you use '=' instead of '==': %s.", curr_names))
    }


    # NULL is just a placeholder
    expr = substitute(
        NULL[Reduce(f = '&', list(...)),]
    )
    parent_frame = parent.frame()
    # if data is expression we want to calculate it only once
    data = force(data)
    eval_in_parent_frame(data, expr, frame = parent_frame)

}





#########

