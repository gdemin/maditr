#' Selects columns from the data set
#'
#' Variable ranges are supported, e. g. vs:carb. Characters which start with '^' or
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
    if(is.data.table(data)){
        query(data, var_indexes, with = FALSE)
    } else {
        data[, var_indexes, drop = FALSE]
    }

}

expand_double_dots = function(expr, data_names, parent_frame, use_sd = TRUE){
    if(is.call(expr) && length(expr)>1){
        curr = expr[[1]]
        if(identical(curr, quote(..)) || identical(curr, quote(columns)) || identical(curr, quote(`%to%`))){
            if(identical(curr, quote(`%to%`))){
                expr = bquote(column_selector(.(expr)))
            } else {
                expr[[1]] = quote(column_selector)
            }
            expr = as.call(c(as.list(expr), list(data_names = data_names, parent_frame = parent_frame)))
            var_indexes = eval(expr)
            if(use_sd){
                expr = bquote(.SD[,.(var_indexes)])
            } else {
                symbols = lapply(data_names[var_indexes], as.symbol)
                expr = as.call(c(quote(data.table), symbols))
            }
        } else {
            res = lapply(as.list(expr), expand_double_dots, data_names = data_names, parent_frame = parent_frame, use_sd = use_sd)
            expr = as.call(res)
        }
    }
    expr
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
    eval.parent(substitute(
        maditr::query_if(data, Reduce(f = '&', list(...)))
    ))

}

expand_selectors = function(selected, df_names, frame){
    selected = lapply(selected, function(item){
        if(length(item)>1) return(expand_selectors(item, df_names, frame))
        res = item
        if(is.character(item)){
            if(is_regex(item)){
                res = get_names_by_regex(item, df_names)
                any(lengths(res)==0) && stop(paste("'columns' - there are no variables which match regex(-s): ",item))

            } else {
                res = eval(substitute(maditr::text_expand(item), list(item = item)), envir = frame)
                res = match(res, df_names)
                anyNA(res) && stop(paste("'columns' - variable not found: ",item))
            }
        }
        res
    })
    unlist(selected, recursive = TRUE, use.names = TRUE)
}


get_names_by_regex = function(regex, df_names){
    lapply(regex, grep, x = df_names, perl = TRUE)
}
