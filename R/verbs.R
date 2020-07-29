#' 'dplyr'-like interface for data.table.
#'
#' Subset of 'dplyr' verbs to work with data.table. Note that there is no
#' `group_by` verb - use `by` or `keyby` argument when needed.
#' - `dt_mutate` adds new variables or modify existing variables. If
#' `data` is data.table then it modifies in-place.
#' - `dt_summarize` computes summary statistics. Splits the data into
#' subsets, computes summary statistics for each, and returns the result in the
#' "data.table" form.
#' - `dt_summarize_all` is the same as `dt_summarize` but work over all non-grouping variables.
#' - `dt_filter` selects rows/cases where conditions are true. Rows
#' where the condition evaluates to NA are dropped.
#' - `dt_select` selects column/variables from the data set. Range of
#' variables are supported, e. g. vs:carb. Characters which start with '^' or
#' end with '$' considered as Perl-style regular expression patterns. For
#' example, '^Petal' returns all variables started with 'Petal'. 'Width$'
#' returns all variables which end with 'Width'. Pattern '^.' matches all
#' variables and pattern '^.*my_str' is equivalent to `contains "my_str"`. See
#' examples.
#' - `dt_arrange` sorts dataset by variable(-s). Use '-' to sort in
#' descending order. If `data` is data.table then it modifies in-place.
#'
#' @param data data.table/data.frame data.frame will be automatically converted
#'   to data.table. `dt_mutate`, `dt_mutate_if`, `dt_mutate_if`
#'   modify data.table object in-place.
#' @param ... List of variables or name-value pairs of summary/modifications
#'   functions. The name will be the name of the variable in the result. In the
#'   `mutate` function we can use `a = b` or `a := b` notation.
#'   Advantages of `:=` are multiassignment (`c("a", "b") := list(1,2)`)
#'   and parametric assignment (`(a) := 2`).
#' @param by unquoted name of grouping variable of list of unquoted names of
#'   grouping variables. For details see [data.table][data.table::data.table]
#' @param keyby Same as `by`, but with an additional `setkey()` run on the by
#'   columns of the result, for convenience. It is common practice to use
#'   'keyby=' routinely when you wish the result to be sorted. For details see
#'   [data.table][data.table::data.table].
#' @param fun function which will be applied to all variables in
#'   `dt_summarize` and `dt_summarize_all`.
#' @param na.last logical. FALSE by default. If TRUE, missing values in the data
#'   are put last; if FALSE, they are put first.
#' @export
#' @examples
#' # examples from 'dplyr'
#' # newly created variables are available immediately
#' mtcars  %>%
#'     dt_mutate(
#'         cyl2 = cyl * 2,
#'         cyl4 = cyl2 * 2
#'     ) %>%
#'     head()
#'
#'
#' # you can also use dt_mutate() to remove variables and
#' # modify existing variables
#' mtcars %>%
#'     dt_mutate(
#'         mpg = NULL,
#'         disp = disp * 0.0163871 # convert to litres
#'     ) %>%
#'     head()
#'
#'
#' # window functions are useful for grouped mutates
#' mtcars %>%
#'     dt_mutate(
#'         rank = rank(-mpg, ties.method = "min"),
#'         keyby = cyl) %>%
#'     print()
#'
#'
#' # You can drop variables by setting them to NULL
#' mtcars %>% dt_mutate(cyl = NULL) %>% head()
#'
#' # A summary applied without by returns a single row
#' mtcars %>%
#'     dt_summarise(mean = mean(disp), n = .N)
#'
#' # Usually, you'll want to group first
#' mtcars %>%
#'     dt_summarise(mean = mean(disp), n = .N, by = cyl)
#'
#'
#' # Multiple 'by' - variables
#' mtcars %>%
#'     dt_summarise(cyl_n = .N, by = list(cyl, vs))
#'
#' # Newly created summaries immediately
#' # doesn't overwrite existing variables
#' mtcars %>%
#'     dt_summarise(disp = mean(disp),
#'                   sd = sd(disp),
#'                   by = cyl)
#'
#' # You can group by expressions:
#' mtcars %>%
#'     dt_summarise_all(mean, by = list(vsam = vs + am))
#'
#' # filter by condition
#' mtcars %>%
#'     dt_filter(am==0)
#'
#' # filter by compound condition
#' mtcars %>%
#'     dt_filter(am==0,  mpg>mean(mpg))
#'
#'
#' # select
#' mtcars %>% dt_select(vs:carb, cyl)
#' mtcars %>% dt_select(-am, -cyl)
#'
#' # regular expression pattern
#' dt_select(iris, "^Petal") # variables which start from 'Petal'
#' dt_select(iris, "Width$") # variables which end with 'Width'
#' # move Species variable to the front.
#' # pattern "^." matches all variables
#' dt_select(iris, Species, "^.")
#' # pattern "^.*i" means "contains 'i'"
#' dt_select(iris, "^.*i")
#' dt_select(iris, 1:4) # numeric indexing - all variables except Species
#'
#' # sorting
#' dt_arrange(mtcars, cyl, disp)
#' dt_arrange(mtcars, -disp)
dt_mutate = function(data, ..., by){
    eval.parent(substitute(let(data, ...,
                               by = by))
    )
}

# \code{dt_mutate_if} completely different from 'dplyr' \code{mutate_if}.
# It modifies subset of rows rather than subset of columns as in 'dplyr'
# \item{\code{dt_mutate_if} }{makes the same thing as \code{dt_mutate} but
# conditionally on subset of rows.}
# @param i integer/logical vector. Supposed to use to
#   modify subset of rows in \code{data}.
# @rdname dt_mutate
# @export
# dt_mutate_if = function(data, i, ..., by, keyby){
#     if(!is.data.frame(data)) stop("dt_mutate_if: 'data' should be data.frame or data.table")
#     if(!is.data.table(data)){
#         data = as.data.table(data)
#     }
#     filt__ = eval.parent(
#         substitute(data[, i, by = by, keyby = keyby])
#     )
#     if(!(NROW(filt___)==1 || NROW(filt___)==NROW(data))) {
#         stop(sprintf("dt_mutate_if: incorrect number of rows in condition: %s. 'data' have %s rows.", NROW(filt___), NROW(filt___)))
#     }
#     if(is.data.table(filt__)){
#         filt___ = Reduce(f = `&`, filt___)
#     }
#     eval.parent(substitute(let_if(data,
#                                   filt__,
#                                   ...,
#                                   by = by,
#                                   keyby = keyby)
#     ))
# }

#' @rdname dt_mutate
#' @export
dt_summarize = function(data, ..., by, keyby, fun = NULL){
    eval.parent(substitute(take(data, ...,
                                by = by,
                                keyby = keyby,
                                fun = fun))
    )
}

#' @rdname dt_mutate
#' @export
dt_summarize_all = function(data, fun, by, keyby){
    !missing(fun) || stop("'dt_summarize_all': argument 'fun' is missing.")
    eval.parent(substitute(take(data,
                                by = by,
                                keyby = keyby,
                                fun = fun))
    )
}

#' @rdname dt_mutate
#' @export
dt_summarise = dt_summarize

#' @rdname dt_mutate
#' @export
dt_summarise_all = dt_summarize_all

#' @rdname dt_mutate
#' @export
dt_select = function(data, ...){
    UseMethod("dt_select")
}

#' @export
dt_select.default = function(data, ...){
    is.data.frame(data) || stop("dt_select: 'data' should be data.frame or data.table")
    if(!is.data.table(data)){
        data = as.data.table(data)
    }
    var_list = substitute(list(...))

    all_indexes = as.list(seq_along(data))
    names(all_indexes) = colnames(data)
    var_indexes = eval(var_list, all_indexes, parent.frame())
    var_indexes = expand_selectors(var_indexes, colnames(data))
    var_indexes = unique(unlist(var_indexes, use.names = FALSE))
    data[, var_indexes, with = FALSE, drop = FALSE]
}

#' @rdname dt_mutate
#' @export
dt_filter = function(data, ...){
    UseMethod("dt_filter")
}


#' @export
dt_filter.default = function(data, ...){
    is.data.frame(data) || stop("dt_filter: 'data' should be data.frame or data.table")
    curr_names = names(substitute(list(...)))
    if(!is.null(curr_names)){
        if(any(c("by", "keyby") %in% curr_names)){
            stop("'dt_filter': you try to use 'by' or 'keyby'. Sorry, but by now grouped filtering is not supported.")
        }
        curr_names = curr_names[curr_names!=""][[1]]
        stop(sprintf("'dt_filter': it seems you use '=' instead of '==': %s.", curr_names))
    }
    if(!is.data.table(data)){
        eval.parent(substitute(
            as.data.table(data)[Reduce(f = '&', list(...)), ]
        ))
    } else {
        eval.parent(substitute(
            data[Reduce(f = '&', list(...)), ]
        ))
    }

}

#' @rdname dt_mutate
#' @export
dt_arrange = sort_by




expand_selectors = function(selected, df_names){
    to_expand = which(vapply(selected, function(x) is.character(x) && is_regex(x), FUN.VALUE = TRUE))
    if(length(to_expand)==0) return(selected)
    expanded = get_names_by_regex(selected[to_expand], df_names)
    not_found = lengths(expanded)==0
    any(not_found) && stop(paste("'dt_select' - there are no variables which match regex(-s): ",
                                 paste(selected[to_expand][not_found], collapse = ", ")
    )
    )
    selected[to_expand] = expanded
    unlist(selected, recursive = TRUE, use.names = TRUE)
}


get_names_by_regex = function(regex, df_names){
    lapply(regex, grep, x = df_names, perl = TRUE)
}
