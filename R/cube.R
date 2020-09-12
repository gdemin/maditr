#' @export
rollup.data.frame = function(x, ...){
    eval.parent(substitute(data.table::rollup(as.data.table(x), ...)))
}

#' @export
cube.data.frame = function(x, ...){
    eval.parent(substitute(data.table::cube(as.data.table(x), ...)))
}

#' @export
groupingsets.data.frame = function(x, ...){
    eval.parent(substitute(data.table::groupingsets(as.data.table(x), ...)))
}

