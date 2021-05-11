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
    var_indexes = select_columns(..., data_names = data_names, frame = parent_frame)

    if(is.data.table(data)){
        data[, var_indexes, with = FALSE]
    } else {
        data[, var_indexes, drop = FALSE]
    }

}

#' @rdname columns
#' @export
cols = columns

# here we find `columns` expression and replace it with
# data.table(...) or c(...)
replace_column_expr = function(expr, data_names, frame, combine = quote(data.table)){
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
            expr = as.call(c(as.list(expr), list(data_names = data_names, frame = frame)))
            var_indexes = eval(expr)
            symbols = lapply(data_names[var_indexes], as.symbol)
            expr = as.call(c(combine, symbols))

        } else {
            res = lapply(as.list(expr),
                         replace_column_expr,
                         data_names = data_names,
                         frame = frame,
                         combine = combine
                         )
            expr = as.call(res)
        }
    }
    expr
}

select_columns = function(..., data_names, frame){
    var_list = substitute(list(...))
    var_list = substitute_symbols(var_list, list("%to%" = quote(`:`), "-" = quote(`__.my_custom_not_`)))
    all_indexes = as.list(seq_along(data_names))
    names(all_indexes) = data_names
    "__.my_custom_not_" = function(e1, e2){
        if(missing(e2)){
            if(is.character(e1)){
                var_indexes = expand_selectors(e1, data_names, frame, new = FALSE)
                return(-var_indexes)
            } else {
                return(-e1)
            }
        }
        base::`-`(e1, e2)
    }
    all_indexes = c(all_indexes, c("__.my_custom_not_" = `__.my_custom_not_`))
    var_indexes = expand_selectors(var_list, data_names, frame, new = FALSE)
    var_indexes = eval(var_indexes, all_indexes, frame)
    var_indexes = unique(unlist(var_indexes, use.names = FALSE))
    var_indexes
}

expand_selectors = function(selected, data_names, frame, new = FALSE){
    # expand text and regex
    selected = lapply(as.list(selected), function(item){
        if(is_range(item)) return(
            s_range_expand(item, df_names = data_names, frame = frame, var_index = !new)
        )
        if(length(item)>1) return(
            expand_selectors(item, data_names, frame, new = new)
        )

        item = s_regex_expand(item, data_names, var_index = !new)
        item = s_text_expand(item, data_names, frame, var_index = !new)
        item
    })
    #unlist(selected, recursive = TRUE, use.names = FALSE)
    as.call(selected)
}






####
is_regex = function(txt){
    is.character(txt) && any(startsWith(txt, "^") | endsWith(txt, "$"))
}

is_range = function(expr){
    is.call(expr) && (identical(expr[[1]], quote(`:`)) | identical(expr[[1]], quote(`%to%`)))
}

s_regex_expand = function(expr, df_names, var_index = TRUE){
    if(!is_regex(expr)) return(expr)
    res = lapply(expr, grep, x = df_names, perl = TRUE, value = !var_index)
    any(lengths(res)==0) && stop(paste("'columns' - there are no variables which match regex(-s): ", paste(expr, collapse = ",")))
    res
}

s_text_expand = function(expr, df_names, frame, var_index = TRUE){
    if(!is.character(expr)) return(expr)
    res = eval(substitute(maditr::text_expand(item), list(item = expr)), envir = frame)
    if(!var_index) return(res)
    res = match(res, df_names)
    anyNA(res) && stop(paste("'columns' - variable not found: ",paste(expr, collapse = ",")))
    res
}

# %to% expansion
# we can rely on indexing a:b -> 10:15, but in this case we cannot
# create new variables from ranges
s_range_expand = function(expr, df_names, frame, var_index = TRUE){
    if(!is_range(expr)) return(expr)
    from = expr[[2]]
    to = expr[[3]]
    if(!is.symbol(from)) {
        from = eval(from, envir = frame, enclos = .GlobalEnv)
    } else {
        from = as.character(from)
    }
    if(!is.symbol(to)) {
        to = eval(to, envir = frame, enclos = .GlobalEnv)
    } else {
        to = as.character(to)
    }
    first = match(from, df_names)[1]
    last = match(to, df_names)[1]
    if((is.na(first) && is.na(last)) && !var_index){
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
        if(var_index) res = first:last else res = df_names[first:last]
    }
    return(res)
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

