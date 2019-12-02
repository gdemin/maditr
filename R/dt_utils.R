#' Additional useful functions
#'
#' \itemize{
#' \item{dt_count}{ calculate number of cases by groups, possibly
#' weighted. \code{dt_add_count} adds number of cases to existing dataset}
#' \item{dt_top_n}{ returns top n rows from each group.}
#' }
#'
#' @param data data.table/data.frame data.frame will be automatically converted
#'   to data.table.
#' @param ... variables to group by.
#' @param weight optional. Unquoted variable name. If provided result will be the sum of this variable by groups.
#' @param sort logical. If TRUE result will be sorted in desending order by resulting variable.
#' @param name character. Name of resulting variable.
#' @param n numeric. number of top cases. If n is negative then bottom values will be returned.
#' @param by list or vector of grouping variables
#' @param order_by unquoted variable name by which result will be sorted. If not
#'   specified, defaults to the last variable in the dataset.
#' @return data.table
#' @export
#'
#' @examples
#'
#' data(mtcars)
#'
#' # dt_count
#' dt_count(mtcars, am, vs)
#' dt_add_count(mtcars, am, vs, name = "am_vs")[] # [] for autoprinting
#'
#' # dt_top_n
#' dt_top_n(mtcars, 2, by  = list(am, vs))
#' dt_top_n(mtcars, 2, order_by = mpg, by  = list(am, vs))
dt_count = function(data, ..., weight = NULL, sort = FALSE, name = "n"){
    name = as.symbol(name)
    weight_expr = substitute(weight)
    if(is.null(weight_expr)){
        res = eval.parent(substitute(take(data, name := .N, by = .(...))))
    } else {
        res = eval.parent(substitute(take_if(data, !is.na(weight), name := sum(weight), by = .(...))))
    }
    if(sort) {
        res = eval(substitute(sort_by(res, -name), list(name = name)))

    }
    res
}

#' @export
#' @rdname dt_count
dt_add_count = function(data, ..., weight = NULL, sort = FALSE, name = "n"){
    name = as.symbol(name)
    weight_expr = substitute(weight)
    if(is.null(weight_expr)){
        res = eval.parent(substitute(let(data, name := .N, by = .(...))))
    } else {
        res = eval.parent(substitute(let_if(data, !is.na(weight), name := sum(weight), by = .(...))))
    }
    if(sort) {
        res = eval(substitute(sort_by(res, -name), list(name = name)))

    }
    res
}

#' @export
#' @rdname dt_count
dt_top_n = function(data, n, by, order_by = NULL){
    order_by_expr = substitute(order_by)
    if(is.null(order_by_expr)){
        order_by = as.symbol(names(data)[ncol(data)])
    }
    if(n>0){
        eval.parent(substitute({
            query(sort_by(data, -order_by), head(.SD, n), by = by)
        }))
    } else {
        eval.parent(substitute({
            query(sort_by(data, -order_by), tail(.SD, -n), by = by)
        }))
    }

}

# @export
# @rdname dt_count
# dt_sample_n = function(data, n, by, replace = FALSE, weight = NULL){
#     sort_by_expr = substitute(sort_by)
#     if(is.null(sort_by_expr)){
#         sort_by = as.symbol(names(data)[ncol(data)])
#     }
#     if(n>0){
#         eval.parent(substitute({
#             query(sort_by(data, -sort_by), head(.SD, n), by = by)
#         }))
#     } else {
#         eval.parent(substitute({
#             query(sort_by(data, -sort_by), tail(.SD, -n), by = by)
#         }))
#     }
#
# }

