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
    data_names = colnames(data)
    parent_frame = parent.frame()
    var_indexes = column_selector(..., data_names = data_names, parent_frame = parent_frame)
    # if data is expression we want to calculate it only once
    calc_data = data
    if(is.data.table(calc_data)){
        query(calc_data, var_indexes, with = FALSE)
    } else {
        calc_data[, var_indexes, drop = FALSE]
    }

}

expand_double_dots = function(expr, data_names, parent_frame){
    if(is.call(expr) && length(expr)>1){
        curr = expr[[1]]
        if(identical(curr, quote(columns)) || identical(curr, quote(`%to%`))){
            if(identical(curr, quote(`%to%`))){
                expr = bquote(column_selector(.(expr)))
            } else {
                expr[[1]] = quote(column_selector)
            }
            expr = as.call(c(as.list(expr), list(data_names = data_names, parent_frame = parent_frame)))
            var_indexes = eval(expr)
            symbols = lapply(data_names[var_indexes], as.symbol)
            expr = as.call(c(quote(data.table), symbols))

        } else {
            res = lapply(as.list(expr), expand_double_dots, data_names = data_names, parent_frame = parent_frame)
            expr = as.call(res)
        }
    }
    expr
}

expand_selectors = function(selected, data_names, frame){
    # expand text and regex
    selected = lapply(selected, function(item){
        if(length(item)>1) return(expand_selectors(item, data_names, frame))
        res = item
        if(is.character(item)){
            if(is_regex(item)){
                res = get_names_by_regex(item, data_names)
                any(lengths(res)==0) && stop(paste("'columns' - there are no variables which match regex(-s): ",item))

            } else {
                res = eval(substitute(maditr::text_expand(item), list(item = item)), envir = frame)
                res = match(res, data_names)
                anyNA(res) && stop(paste("'columns' - variable not found: ",item))
            }
        }
        res
    })
    unlist(selected, recursive = TRUE, use.names = TRUE)
}

column_selector = function(..., data_names, parent_frame){
    var_list = substitute(list(...))
    var_list = substitute_symbols(var_list, list("%to%" = quote(`:`), "-" = quote(`__.my_custom_not_`)))
    all_indexes = as.list(seq_along(data_names))
    names(all_indexes) = data_names
    "__.my_custom_not_" = function(e1, e2){
        if(missing(e2)){
            if(is.character(e1)){
                var_indexes = expand_selectors(e1, data_names, parent_frame)
                return(-var_indexes)
            } else {
                return(-e1)
            }
        }
        base::`-`(e1, e2)
    }
    all_indexes = c(all_indexes, c("__.my_custom_not_" = `__.my_custom_not_`))
    var_indexes = eval(var_list, all_indexes, parent_frame)
    var_indexes = expand_selectors(var_indexes, data_names, parent_frame)
    var_indexes = unique(unlist(var_indexes, use.names = FALSE))
    var_indexes
}

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
    # if data is expression we want to calculate it only once
    calc_data = data
    expr = substitute(
        maditr::query_if(calc_data, Reduce(f = '&', list(...)))
    )
    data_names = names(data)
    parent_frame = parent.frame()
    expr = preproc_variable_names(data_names, expr, parent_frame)
    eval.parent(expr)

}




get_names_by_regex = function(regex, df_names){
    lapply(regex, grep, x = df_names, perl = TRUE)
}

# %to% expansion for assignment
.into_helper_ = function(e1, e2){
    var_names = get_current_variables(parent.frame())
    e1 = substitute(list(e1))
    e2 = substitute(list(e2))
    e1 = evaluate_variable_names(e1, envir = parent.frame(), symbols_to_characters = TRUE)
    e2 = evaluate_variable_names(e2, envir = parent.frame(), symbols_to_characters = TRUE)
    stopif(length(e1)>1, "'%to%' - length of name of first variable is greater than one.")
    stopif(length(e2)>1, "'%to%' - length of name of second variable is greater than one.")
    e1 = e1[[1]]
    e2 = e2[[1]]
    first = match(e1, var_names)[1]
    last = match(e2, var_names)[1]
    if(is.na(first) && is.na(last)){
        patt1 = gsub("^(.+?)([\\d]+)$", "\\1", e1, perl = TRUE)
        patt2 = gsub("^(.+?)([\\d]+)$", "\\1", e2, perl = TRUE)
        stopif(patt1!=patt2, "Start and end variables begin from different patterns: '", patt1, "', '", patt2,"'.")
        digits1 = gsub("^(.+?)([\\d]+)$", "\\2", e1, perl = TRUE)
        digits2 = gsub("^(.+?)([\\d]+)$", "\\2", e2, perl = TRUE)
        padding = 0
        if((substr(digits1,1,1)=="0" || substr(digits2,1,1)==0) &&
           !(substr(digits1,1,1)=="0" && nchar(digits1)==1 && substr(digits2,1,1)!=0)){
            stopif(nchar(digits1)!=nchar(digits2),
                   "Invalid use of the %to% convention. For zero-padded numbers numeric part of the names must be the same length but: '",
                   digits1, ", '", digits2, "'.")
            padding = nchar(digits1)
        }
        digits1 = as.numeric(digits1)
        digits2 = as.numeric(digits2)
        stopif(digits1>digits2, "Name of start variables greater than name of end variables: '", e1,"' > '",e2,"'.")
        all_digits = digits1:digits2
        if(padding>0) all_digits = formatC(all_digits, width = padding, format = "d", flag = "0")
        return(paste0(patt1, all_digits))
    } else {
        stopif(is.na(first), "'", e2, "' is found but '", e1, "' is absent.")
        stopif(is.na(last), "'", e1, "' is found but '", e2, "' is absent.")
        stopif(last<first, "'",e2, "' located before '",e1,"'. Did you mean '",e2," %to% ",e1,"'?")
        return(var_names[first:last])
    }
}
