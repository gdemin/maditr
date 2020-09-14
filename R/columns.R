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
columns.etable = function(data, ...){
    data_class = class(data)
    res = eval.parent(
        substitute(maditr::columns(data, ...))
    )
    class(res) = data_class
    res
}

#' @export
columns.data.frame = function(data, ...){
    var_list = substitute(list(...))
    var_list = substitute_symbols(var_list, list("%to%" = quote(`:`), "-" = quote(`__.my_custom_not_`)))
    all_indexes = as.list(seq_along(data))
    data_names = colnames(data)
    names(all_indexes) = data_names
    "__.my_custom_not_" = function(e1, e2){
        if(missing(e2)){
            if(is.character(e1)){
                var_indexes = expand_selectors(e1, data_names)
                return(-var_indexes)
            } else {
                return(-e1)
            }
        }
        base::`-`(e1, e2)
    }
    all_indexes = c(all_indexes, c("__.my_custom_not_" = `__.my_custom_not_`))
    var_indexes = eval(var_list, all_indexes, parent.frame())
    var_indexes = expand_selectors(var_indexes, data_names)
    var_indexes = unique(unlist(var_indexes, use.names = FALSE))
    if(is.data.table(data)){
        query(data, var_indexes, with = FALSE)
    } else {
        data[, var_indexes, drop = FALSE]
    }

}


expand_selectors = function(selected, df_names){
    selected = lapply(selected, function(item){
        res = item
        if(is.character(item)){
            if(is_regex(item)){
                res = get_names_by_regex(item, df_names)
                any(lengths(res)==0) && stop(paste("'columns' - there are no variables which match regex(-s): ",item))

            } else {
                res = text_expand(item)
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