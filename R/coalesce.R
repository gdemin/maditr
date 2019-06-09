#' Return first non-missing element
#'
#' @author Martin Morgan https://stackoverflow.com/users/547331/martin-morgan
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
coalesce = function(...) {
    # Martin Morgan https://stackoverflow.com/users/547331/martin-morgan
    # https://stackoverflow.com/questions/19253820/how-to-implement-coalesce-efficiently-in-r
    # https://stackoverflow.com/a/19257945/387408
    args = list(...)
    ans = args[[1]]
    length_ans = length(ans)
    for (elt in args[-1]) {
        elem_length = length(elt)
        elem_length == length_ans || elem_length == 1 ||
            stop("'coalesce': all arguments should be the same length as first element or length one.", call. = FALSE)

        if(elem_length==1){
            elt = rep_len(elt, length.out = length_ans)
        }
        i = which(is.na(ans))
        ans[i] = elt[i]
    }
    ans
}
