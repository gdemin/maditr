#' Return first non-missing element
#'
#' It is an alias for data.table \code{fcoalesce}. For details see \link[data.table]{fcoalesce}
#' @param ... vectors
#'
#' @return A vector the same length as the first ... argument with NA
#'   values replaced by the first non-missing value.
#' @export
#'
#' @examples
#' # examples from dplyr
#' x = sample(c(1:5, NA, NA, NA))
#' coalesce(x, 0L)
#'
#' y = c(1, 2, NA, NA, 5)
#' z = c(NA, NA, 3, 4, 5)
#' coalesce(y, z)
coalesce = data.table::fcoalesce
