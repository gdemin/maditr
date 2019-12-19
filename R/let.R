#' Modify, aggregate, select or filter data.frame/data.table
#'
#' \code{let} adds new variables or modify existing variables. \code{let_if}
#' make the same thing conditionally. \code{take} aggregates data or select
#' subset of the data by rows or columns. Both functions return
#' \code{data.table}.
#' \itemize{
#' \item{Add new variables: }{\code{let(mtcars, new_var = 42, new_var2 = new_var*hp)}}
#' \item{Filter data: }{\code{take_if(mtcars, am==0)}}
#' \item{Select variables: }{\code{take(mtcars, am, vs, mpg)}}
#' \item{Aggregate data: }{\code{take(mtcars, mean_mpg = mean(mpg), by = am)}}
#' \item{Aggregate all non-grouping columns: }{\code{take(mtcars, fun = mean, by = am)}}
#' }
#'
#' @param data data.table/data.frame data.frame will be automatically converted
#'   to data.table. \code{let} modify data.table object in-place.
#' @param i integer/logical vector. Supposed to use to subset/conditional
#'   modifications of \code{data}. For details see \link[data.table]{data.table}
#' @param ... List of variables or name-value pairs of summary/modifications
#'   functions. The name will be the name of the variable in the result. In the
#'   \code{let} and \code{take} functions we can use \code{a = b} or \code{a :=
#'   b} notation. Advantages of \code{:=} is parametric assignment, e. g.
#'   \code{(a) := 2} create variable with name which are stored in \code{a}. In
#'   \code{let} \code{:=} can be used for multiassignment (\code{c("a", "b")  :=
#'   list(1,2)})
#' @param by unquoted name of grouping variable of list of unquoted names of
#'   grouping variables. For details see \link[data.table]{data.table}
#' @param keyby Same as \code{by}, but with an additional \code{setkey()} run on the by
#'   columns of the result, for convenience. It is common practice to use
#'   'keyby=' routinely when you wish the result to be sorted. For details see
#'   \link[data.table]{data.table}.
#' @param with logical. For details see \link[data.table]{data.table}.
#' @param nomatch Same as nomatch in match. For details see
#'   \link[data.table]{data.table}.
#' @param mult For details see \link[data.table]{data.table}.
#' @param roll For details see \link[data.table]{data.table}.
#' @param rollends For details see \link[data.table]{data.table}.
#' @param which For details see \link[data.table]{data.table}.
#' @param .SDcols Specifies the columns of x to be included in the special
#'   symbol .SD which stands for Subset of data.table. May be character column
#'   names or numeric positions. For details see \link[data.table]{data.table}.
#' @param verbose logical. For details see \link[data.table]{data.table}.
#' @param allow.cartesian For details see \link[data.table]{data.table}.
#' @param drop For details see \link[data.table]{data.table}.
#' @param on For details see \link[data.table]{data.table}.
#' @param autoname logical. TRUE by default. Should we create names for  unnamed expressions in \code{take}?
#' @param fun function which will be applied to all variables in \code{take}. If
#'   there are no variables in \code{take} then it will be applied to all non-grouping
#'   variables in the \code{data}.
#' @param na.last logical. FALSE by default. If TRUE, missing values in the data
#'   are put last; if FALSE, they are put first.
#' @return data.table. \code{let} returns its result invisibly.
#' @export
#'
#' @examples
#' # examples form 'dplyr' package
#' data(mtcars)
#'
#' # Newly created variables are available immediately
#' mtcars %>%
#'     let(
#'         cyl2 = cyl * 2,
#'         cyl4 = cyl2 * 2
#'     ) %>% head()
#'
#' # You can also use let() to remove variables and
#' # modify existing variables
#' mtcars %>%
#'     let(
#'         mpg = NULL,
#'         disp = disp * 0.0163871 # convert to litres
#'     ) %>% head()
#'
#'
#' # window functions are useful for grouped computations
#' mtcars %>%
#'     let(rank = rank(-mpg, ties.method = "min"),
#'         by = cyl) %>%
#'     head()
#'
#' # You can drop variables by setting them to NULL
#' mtcars %>% let(cyl = NULL) %>% head()
#'
#' # keeps all existing variables
#' mtcars %>%
#'     let(displ_l = disp / 61.0237) %>%
#'     head()
#'
#' # keeps only the variables you create
#' mtcars %>%
#'     take(displ_l = disp / 61.0237)
#'
#'
#' # can refer to both contextual variables and variable names:
#' var = 100
#' mtcars %>%
#'     let(cyl = cyl * var) %>%
#'     head()
#'
#' # filter by condition
#' mtcars %>%
#'     take_if(am==0)
#'
#' # filter by compound condition
#' mtcars %>%
#'     take_if(am==0 & mpg>mean(mpg))
#'
#'
#' # A 'take' with summary functions applied without 'by' argument returns an aggregated data
#' mtcars %>%
#'     take(mean = mean(disp), n = .N)
#'
#' # Usually, you'll want to group first
#' mtcars %>%
#'     take(mean = mean(disp), n = .N, by = cyl)
#'
#' # You can group by expressions:
#' mtcars %>%
#'     take(fun = mean, by = list(vsam = vs + am))
#'
#'
#' # parametric evaluation:
#' var = quote(mean(cyl))
#' mtcars %>%
#'     let(mean_cyl = eval(var)) %>%
#'     head()
#' take(mtcars, eval(var))
#'
#' # all together
#' new_var = "mean_cyl"
#' mtcars %>%
#'     let((new_var) := eval(var)) %>%
#'     head()
#' take(mtcars, (new_var) := eval(var))
#'
#' ########################################
#'
#' # examples from data.table
#' dat = data.table(
#'     x=rep(c("b","a","c"), each=3),
#'     y=c(1,3,6),
#'     v=1:9
#' )
#'
#' # basic row subset operations
#' take_if(dat, 2)                         # 2nd row
#' take_if(dat, 3:2)                       # 3rd and 2nd row
#' take_if(dat, order(x))                  # no need for order(dat$x)
#' take_if(dat, y>2)                       # all rows where dat$y > 2
#' take_if(dat, y>2 & v>5)                 # compound logical expressions
#' take_if(dat, !2:4)                      # all rows other than 2:4
#' take_if(dat, -(2:4))                    # same
#'
#' # select|compute columns
#' take(dat, v)                  # v column (as data.table)
#' take(dat, sum(v))             # return data.table with sum of v (column autonamed 'sum(v)')
#' take(dat, sv = sum(v))        # same, but column named "sv"
#' take(dat, v, v*2)             # return two column data.table, v and v*2
#'
#' # subset rows and select|compute
#' take_if(dat, 2:3, sum(v))      # sum(v) over rows 2 and 3
#' take_if(dat, 2:3, sv = sum(v)) # same, but return data.table with column sv
#'
#' # grouping operations
#' take(dat, sum(v), by = x)             # ad hoc by, order of groups preserved in result
#' take(dat, sum(v), keyby = x)          # same, but order the result on by cols
#'
#'
#' # all together now
#' take_if(dat, x!="a", sum(v), by=x)                       # get sum(v) by "x" for each x != "a"
#' take_if(dat, c("b", "c"), sum(v), by = .EACHI, on="x")   # same
#'
#' # more on special symbols, see also ?"data.table::special-symbols"
#' take_if(dat, .N)                           # last row
#' take(dat, .N)                              # total number of rows in DT
#' take(dat, .N, by=x)                        # number of rows in each group
#'
#' take(dat, .I[1], by=x)                     # row number in DT corresponding to each group
#'
#'
#' # add/update/delete by reference
#' # [] at the end of expression is for autoprinting
#' let(dat, grp = .GRP, by=x)[]          # add a group counter column
#' let(dat, z = 42L)[]                   # add new column by reference
#' let(dat, z = NULL)[]                  # remove column by reference
#' let_if(dat, x=="a", v = 42L)[]        # subassign to existing v column by reference
#' let_if(dat, x=="b", v2 = 84L)[]       # subassign to new column by reference (NA padded)
#'
#' let(dat, m = mean(v), by=x)[]         # add new column by reference by group
#'
#' # advanced usage
#' dat = data.table(x=rep(c("b","a","c"), each=3),
#'                  v=c(1,1,1,2,2,1,1,2,2),
#'                  y=c(1,3,6),
#'                  a=1:9,
#'                  b=9:1)
#'
#' take(dat, sum(v), by=list(y%%2))              # expressions in by
#' take(dat, sum(v), by=list(bool = y%%2))       # same, using a named list to change by column name
#' take(dat, fun = sum, by=x)                    # sum of all (other) columns for each group
#' take(dat,
#'      MySum=sum(v),
#'      MyMin=min(v),
#'      MyMax=max(v),
#'      by = list(x, y%%2)               # by 2 expressions
#' )
#'
#' take(dat, seq = min(a):max(b), by=x)  # j is not limited to just aggregations
#' dat %>%
#'     take(V1 = sum(v), by=x) %>%
#'     take_if(V1<20)                    # compound query
#'
#' dat %>%
#'     take(V1 = sum(v), by=x) %>%
#'     sort_by(-V1) %>%                  # ordering results
#'     head()
#'
#'
let_if = function(data,
                  i,
                  ...,
                  by,
                  keyby,
                  with = TRUE,
                  nomatch = getOption("datatable.nomatch"),
                  mult = "all",
                  roll = FALSE,
                  rollends = if (roll=="nearest") c(TRUE,TRUE)
                  else if (roll>=0) c(FALSE,TRUE)
                  else c(TRUE,FALSE),
                  which = FALSE,
                  .SDcols,
                  verbose = getOption("datatable.verbose"),                   # default: FALSE
                  allow.cartesian = getOption("datatable.allow.cartesian"),   # default: FALSE
                  drop = NULL,
                  on = NULL
){
    UseMethod("let_if")
}

#' @export
let_if.default = function(data,
                  i,
                  ...,
                  by,
                  keyby,
                  with = TRUE,
                  nomatch = getOption("datatable.nomatch"),
                  mult = "all",
                  roll = FALSE,
                  rollends = if (roll=="nearest") c(TRUE,TRUE)
                  else if (roll>=0) c(FALSE,TRUE)
                  else c(TRUE,FALSE),
                  which = FALSE,
                  .SDcols,
                  verbose = getOption("datatable.verbose"),                   # default: FALSE
                  allow.cartesian = getOption("datatable.allow.cartesian"),   # default: FALSE
                  drop = NULL,
                  on = NULL
){
    is.data.frame(data) || stop("let/let_if: 'data' should be data.frame or data.table")
    call_expr = sys.call()
    if(!is.data.table(data)){
        data = as.data.table(data)
        call_expr[[2]] = as.symbol("data")
        curr_eval = eval
    } else {
        curr_eval = eval.parent
    }
    call_expr[[1]] = as.symbol("[")
    call_expr = add_brackets_to_i(call_expr)
    call_list = as.list(call_expr)
    j_list = as.list(substitute(list(...)))[-1]
    j_length = length(j_list)
    j_length>0 || stop("let/let_if: please, provide at least one expression.")
    if(is.null(names(j_list))){
        names(j_list) = rep("", j_length )
    }
    # check for names
    all_names = names(j_list)
    for(i in seq_along(j_list)){
        if(all_names[i] == ""){
            if(!is.call(j_list[[i]]) || !identical(j_list[[i]][[1]], as.symbol(":="))){
                stop(sprintf("let/let_if: '%s' - incorrect expression. All expressions should be
                             in the form  'var_name = expression' or 'var_name := expression'.", safe_deparse(j_list[[i]])))
            }
        }
    }
    i_position = 3
    call_list = call_list[-(seq_len(j_length) + i_position)]
    call_list = c(call_list[1:3], list(NULL), call_list[-(1:i_position)]) # reserve position for j
    for(i in seq_along(j_list)){
        if(all_names[i]==""){
            call_list[[i_position + 1]] = j_list[[i]]
        } else {
            curr_name = all_names[i]
            curr_expr = j_list[[i]]
            call_list[[i_position + 1]] = substitute(`:=`(curr_name, curr_expr))
        }
        call_expr = as.call(call_list)
        curr_eval(call_expr)
    }

    data
}

#' @rdname let_if
#' @export
take_if = function(data,
                   i,
                   ...,
                   by,
                   keyby,
                   with = TRUE,
                   nomatch = getOption("datatable.nomatch"),
                   mult = "all",
                   roll = FALSE,
                   rollends = if (roll=="nearest") c(TRUE,TRUE)
                   else if (roll>=0) c(FALSE,TRUE)
                   else c(TRUE,FALSE),
                   which = FALSE,
                   .SDcols,
                   verbose = getOption("datatable.verbose"),                   # default: FALSE
                   allow.cartesian = getOption("datatable.allow.cartesian"),   # default: FALSE
                   drop = NULL,
                   on = NULL,
                   autoname = TRUE,
                   fun = NULL
){
    UseMethod("take_if")
}

#' @export
take_if.default = function(data,
                   i,
                   ...,
                   by,
                   keyby,
                   with = TRUE,
                   nomatch = getOption("datatable.nomatch"),
                   mult = "all",
                   roll = FALSE,
                   rollends = if (roll=="nearest") c(TRUE,TRUE)
                   else if (roll>=0) c(FALSE,TRUE)
                   else c(TRUE,FALSE),
                   which = FALSE,
                   .SDcols,
                   verbose = getOption("datatable.verbose"),                   # default: FALSE
                   allow.cartesian = getOption("datatable.allow.cartesian"),   # default: FALSE
                   drop = NULL,
                   on = NULL,
                   autoname = TRUE,
                   fun = NULL
){
    is.data.frame(data) || stop("take/take_if: 'data' should be data.frame or data.table")
    call_expr = sys.call()
    if(!is.data.table(data)){
        call_expr[[2]] = substitute(as.data.table(data))
    }
    call_expr[[1]] = as.symbol("[")

    ### remove autoname argument
    call_list = as.list(call_expr)
    if(any(c("autoname", "fun") %in% names(call_list))) {
        call_list = call_list[!(names(call_list) %in% c("autoname", "fun"))]
    }
    ###########
    j_expr = substitute(list(...))
    j_length = length(j_expr) - 1
    i_position = 3
    # somedays...
    # if(!missing(fun)){
    #     .Deprecated(msg = "'take'/'take_if': 'fun' argument is deprecated. Use 'take_all(data, fun)' instead.")
    # }
    if(j_length>0){

        ## parse ':=' expression
        j_expr = as.list(j_expr)
        if(is.null(names(j_expr))){
            names(j_expr) = rep("", j_length +1)
        }
        for(i in seq_along(j_expr)[-1]){
            if(is.call(j_expr[[i]]) && identical(j_expr[[i]][[1]], as.symbol(":="))){
                name_expr = j_expr[[i]][[2]]
                j_expr[[i]] = j_expr[[i]][[3]]
                if(is.call(name_expr)){
                    names(j_expr)[i] = eval.parent(name_expr)
                } else {
                    if(is.character(name_expr)){
                        names(j_expr)[i] = name_expr
                    } else {
                        names(j_expr)[i] = safe_deparse(name_expr)
                    }
                }
            }
        }
        j_expr = as.call(j_expr)
        #################
        if(autoname){
            j_expr = add_names_to_quoted_list(j_expr)
        }
        if(is.null(fun)){
            call_list = call_list[-(seq_len(j_length) + i_position)]
            call_list = c(call_list[1:3], list(j_expr), call_list[-(1:i_position)])
            call_expr = as.call(call_list)
        } else {
            call_list = call_list[-(seq_len(j_length) + i_position)]
            call_list = c(call_list[1:3], list(substitute(lapply(j_expr, fun))), call_list[-(1:i_position)])
            call_expr = as.call(call_list)
        }
    } else {
        if(!is.null(fun)){
            call_list = c(call_list[1:3], list(substitute(lapply(.SD, fun))), call_list[-(1:3)])
            call_expr = as.call(call_list)
        }
    }
    call_expr = add_brackets_to_i(call_expr)
    eval.parent(call_expr)
}

#' @rdname let_if
#' @export
take = function(data,
                ...,
                by,
                keyby,
                with = TRUE,
                nomatch = getOption("datatable.nomatch"),
                mult = "all",
                roll = FALSE,
                rollends = if (roll=="nearest") c(TRUE,TRUE)
                else if (roll>=0) c(FALSE,TRUE)
                else c(TRUE,FALSE),
                which = FALSE,
                .SDcols,
                verbose = getOption("datatable.verbose"),                   # default: FALSE
                allow.cartesian = getOption("datatable.allow.cartesian"),   # default: FALSE
                drop = NULL,
                on = NULL,
                autoname = TRUE,
                fun = NULL
){
    UseMethod("take")
}

#' @export
take.default = function(data,
                ...,
                by,
                keyby,
                with = TRUE,
                nomatch = getOption("datatable.nomatch"),
                mult = "all",
                roll = FALSE,
                rollends = if (roll=="nearest") c(TRUE,TRUE)
                else if (roll>=0) c(FALSE,TRUE)
                else c(TRUE,FALSE),
                which = FALSE,
                .SDcols,
                verbose = getOption("datatable.verbose"),                   # default: FALSE
                allow.cartesian = getOption("datatable.allow.cartesian"),   # default: FALSE
                drop = NULL,
                on = NULL,
                autoname = TRUE,
                fun = NULL
){
    call_expr = sys.call()
    call_expr[[1]] = as.symbol("take_if")
    call_expr = insert_empty_i(call_expr)
    eval.parent(call_expr)
}

#' @rdname let_if
#' @export
let = function(data,
               ...,
               by,
               keyby,
               with = TRUE,
               nomatch = getOption("datatable.nomatch"),
               mult = "all",
               roll = FALSE,
               rollends = if (roll=="nearest") c(TRUE,TRUE)
               else if (roll>=0) c(FALSE,TRUE)
               else c(TRUE,FALSE),
               which = FALSE,
               .SDcols,
               verbose = getOption("datatable.verbose"),                   # default: FALSE
               allow.cartesian = getOption("datatable.allow.cartesian"),   # default: FALSE
               drop = NULL,
               on = NULL
){
    UseMethod("let")
}

#' @rdname let_if
#' @export
let.default = function(data,
               ...,
               by,
               keyby,
               with = TRUE,
               nomatch = getOption("datatable.nomatch"),
               mult = "all",
               roll = FALSE,
               rollends = if (roll=="nearest") c(TRUE,TRUE)
               else if (roll>=0) c(FALSE,TRUE)
               else c(TRUE,FALSE),
               which = FALSE,
               .SDcols,
               verbose = getOption("datatable.verbose"),                   # default: FALSE
               allow.cartesian = getOption("datatable.allow.cartesian"),   # default: FALSE
               drop = NULL,
               on = NULL
){
    call_expr = sys.call()
    call_expr[[1]] = as.symbol("let_if")
    call_expr = insert_empty_i(call_expr)
    eval.parent(call_expr)
}




###################### take_all #################

#' @export
#' @rdname let_if
take_all = function(data,
                    expr,
                    by,
                    keyby,
                    .SDcols,
                    verbose = getOption("datatable.verbose"),                   # default: FALSE
                    prefix = NULL,
                    i
){
    UseMethod("take_all")
}

#' @export
take_all.default = function(data,
                           expr,
                           by,
                           ...,
                           keyby,
                           .SDcols,
                           verbose = getOption("datatable.verbose"),                   # default: FALSE
                           prefix = NULL,
                           i
){
    is.data.frame(data) || stop("'take_all': 'data' should be data.frame or data.table")
    ...length()>0 && stop("'take_all': unknown arguments - ", safe_deparse(substitute(list(...))))
    call_expr = sys.call()
    if(!is.data.table(data)){
        call_expr[[2]] = substitute(as.data.table(data))
    }
    call_expr[[1]] = as.symbol("[")
    if(is.null(prefix)) {
        name_expr = quote(names(.SD))
    } else  if(is.function(prefix)) {
        name_expr = substitute(prefix(names(.SD)))
    } else {
        name_expr = substitute(paste0(prefix, names(.SD)))
    }
    ### remove prefix argument
    if(any("prefix" %in% names(call_expr))) call_expr[["prefix"]] = NULL

    expr = substitute(expr)
    expr = substitute_symbols(expr, list(
        '.item' = quote(.SD[[.name]]),
        '.x' = quote(.SD[[.name]])
    ))

    if(is.symbol(expr) && !identical(expr, quote(.name))){
        expr = substitute(expr(.SD[[.name]]))
    }
    j_expr = substitute({
        res = lapply(names(.SD), function(.name) expr)
        names(res) = name_expr
        res = res[lengths(res)>0]
        res
    })

    ####
    call_expr = insert_empty_i(call_expr)
    call_expr = add_brackets_to_i(call_expr)
    call_expr[[4]] = j_expr
    eval.parent(call_expr)
}


#######################################

#' @rdname let_if
#' @export
sort_by = function(data, ..., na.last = FALSE){
    UseMethod("sort_by")
}


#' @export
sort_by.default = function(data, ..., na.last = FALSE){
    is.data.frame(data) || stop("'sort_by': 'data' should be data.frame or data.table")
    if(!is.data.table(data)){
        data = as.data.table(data)
        setorder(data, ..., na.last = na.last)
    } else {
        eval.parent(substitute(setorder(data, ..., na.last = na.last)))
    }
    data
}

