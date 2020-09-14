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
#'   to data.table. `dt_mutate` modify data.table object in-place.
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
#' @return data.table
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
    eval.parent(substitute(maditr::let(data, ...,
                               by = by))
    )
}


#' @rdname dt_mutate
#' @export
dt_summarize = function(data, ..., by, keyby, fun = NULL){
    eval.parent(substitute(maditr::take(data, ...,
                                by = by,
                                keyby = keyby,
                                fun = fun))
    )
}

#' @rdname dt_mutate
#' @export
dt_summarize_all = function(data, fun, by, keyby){
    !missing(fun) || stop("'dt_summarize_all': argument 'fun' is missing.")
    eval.parent(substitute(maditr::take(data,
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
dt_select = columns



#' @rdname dt_mutate
#' @export
dt_filter = function(data, ...){
    UseMethod("dt_filter")
}


#' @export
dt_filter.data.frame = function(data, ...){
    curr_names = names(substitute(list(...)))
    if(!is.null(curr_names)){
        if(any(c("by", "keyby") %in% curr_names)){
            stop("'dt_filter': you try to use 'by' or 'keyby'. Sorry, but grouped filtering is not yet supported.")
        }
        curr_names = curr_names[curr_names!=""][[1]]
        stop(sprintf("'dt_filter': it seems you use '=' instead of '==': %s.", curr_names))
    }
    eval.parent(substitute(
        maditr::take_if(data, Reduce(f = '&', list(...)))
    ))

}

#' @rdname dt_mutate
#' @export
dt_arrange = sort_by




