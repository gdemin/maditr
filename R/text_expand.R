#' Evaluate expressions in curly brackets inside strings
#'
#' `text_expand` is simple string interpolation function. It searches in its
#' arguments expressions in curly brackets `{expr}`, evaluate them and substitute with
#' the result of evaluation. See examples.
#'
#' @param ... character vectors
#' @param delim character vector of length 2 - pair of opening and closing
#'   delimiters for the templating tags. By default it is curly brackets. Note
#'   that `delim` will be used in the perl-style regular expression so you
#'   need to escape special characters, e. g. use "\\\\\{" instead of
#'   "\{".
#' @return Vector of characters
#' @examples
#' i = 1:5
#' text_expand("q{i}")
#'
#' i = 1:3
#' j = 1:3
#' text_expand("q1_{i}_{j}")
#'
#' data(iris)
#' text_expand("'iris' has {nrow(iris)} rows.")
#' @export
text_expand = function(..., delim = c("\\{", "\\}")){
    length(delim)!=2 && stop("'text_expand': 'delim' should be vector of length two.")
    left = delim[[1]]
    right = delim[[2]]
    pattern = paste0(left, "(.+?)", right)
    all_vars= c(list(...), recursive = TRUE)
    res = vector(mode = "list", length = length(all_vars))
    for(each_var in seq_along(all_vars)){
        x = all_vars[each_var]
        if(any(grepl(pattern, x, perl = TRUE))){
            positions = gregexpr(pattern, x, perl = TRUE)
            matches = rev(unique(unlist(regmatches(x, positions))))
            var_names = gsub(right, "", gsub(left, "", matches, perl = TRUE), perl = TRUE)

            for(i in seq_along(var_names)){
                evaluated_item = eval(parse(text = var_names[i]),
                                      envir = parent.frame(),
                                      enclos = baseenv())
                x = unlist(lapply(evaluated_item, function(item){
                    gsub(matches[i], item, x, fixed = TRUE)

                }))

            }

        }
        res[[each_var]] = x
    }
    c(res, recursive = TRUE)
}

