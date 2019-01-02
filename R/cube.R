#' @export
rollup.data.frame = function(x, ...){
    eval.parent(substitute(rollup(as.data.table(x), ...)))
}

#' @export
cube.data.frame = function(x, ...){
    eval.parent(substitute(cube(as.data.table(x), ...)))
}

#' @export
groupingsets.data.frame = function(x, ...){
    eval.parent(substitute(groupingsets(as.data.table(x), ...)))
}

