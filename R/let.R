#' Modify, aggregate, select or filter data.frame/data.table
#'
#' - `let` adds new variables or modify existing variables. 'let_if' make
#' the same thing on the subset of rows.
#' - `take/take_if` aggregate data or select subset of the data by rows or
#' columns.
#' - `let_all` applies expressions to all variables in the dataset. It is also
#' possible to modify the subset of the variables.
#' - `take_all` aggregates all variables in the dataset. It is also possible
#' to aggregate the subset of the variables.
#' ```
#' ```
#' All functions return `data.table`. Expression in the 'take_all' and
#' 'let_all' can use predefined variables: '.x' is a value of current variable ,
#' '.name' is a name of the variable and '.index' is sequential number of the
#' variable. '.value' is is an alias to '.x'.
#' - Add new variables: `let(mtcars, new_var = 42, new_var2 = new_var*hp)`
#' - Filter data: `take_if(mtcars, am==0)`
#' - Select variables: `take(mtcars, am, vs, mpg)`
#' - Aggregate data: `take(mtcars, mean_mpg = mean(mpg), by = am)`
#' - Aggregate all non-grouping columns: `take_all(mtcars, mean = mean(.x), sd = sd(.x), n = .N, by = am)`
#' - Aggregate all numeric columns: `take_all(iris, if(is.numeric(.x)) mean(.x))`
#' - To modify all non-grouping variables:
#' ```
#'       iris %>%
#'          let_all(
#'              scaled = (.x - mean(.x))/sd(.x),
#'              by = Species) %>%
#'           head()
#'  ```
#' - Aggregate specific columns: `take_all(iris, if(startsWith(.name, "Sepal")) mean(.x))`
#'
#' @param data data.table/data.frame data.frame will be automatically converted
#'   to data.table. `let` modify data.table object in-place.
#' @param i integer/logical vector. Supposed to use to subset/conditional
#'   modifications of `data`. For details see [data.table][data.table::data.table]
#' @param ... List of variables or name-value pairs of summary/modifications
#'   functions. The name will be the name of the variable in the result. In the
#'   `let` and `take` functions we can use `a = b` or `a :=
#'   b` notation. Advantages of `:=` is parametric assignment, e. g.
#'   `(a) := 2` create variable with name which are stored in `a`. In
#'   `let` `:=` can be used for multiassignment (`c("a", "b")  :=
#'   list(1,2)`). Expression in the 'take_all' and 'let_all' can use predefined
#'   variables: '.x' is a value of current variable, '.name' is a name of
#'   the variable and '.index' is sequential number of the variable. '.value' is
#'   is an alias to '.x'.
#' @param suffix logical TRUE by default. For 'let_all'/'take_all'. If TRUE than
#'   we append summary name to the end of the variable name. If FALSE summary
#'   name will be added at the begining of the variable name.
#' @param sep character. "_" by default. Separator between the old variables
#'   name and prefix or suffix for 'let_all' and 'take_all'.
#' @param by unquoted name of grouping variable of list of unquoted names of
#'   grouping variables. For details see [data.table][data.table::data.table]
#' @param keyby Same as `by`, but with an additional `setkey()` run on the by
#'   columns of the result, for convenience. It is common practice to use
#'   'keyby=' routinely when you wish the result to be sorted. For details see
#'   [data.table][data.table::data.table].
#' @param .SDcols Specifies the columns of x to be included in the special
#'   symbol .SD which stands for Subset of data.table. May be character column
#'   names or numeric positions. For details see [data.table][data.table::data.table].
#' @param autoname logical. TRUE by default. Should we create names for  unnamed expressions in `take`?
#' @param fun Function which will be applied to all variables in `take`. If
#'   there are no variables in `take` then it will be applied to all
#'   non-grouping variables in the `data`.
#' @param na.last logical. FALSE by default. If TRUE, missing values in the data
#'   are put last; if FALSE, they are put first.
#' @return data.table. `let` returns its result invisibly.
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
#'     take_all(mean, by = list(vsam = vs + am))
#'
#' # modify all non-grouping variables in-place
#' mtcars %>%
#'     let_all((.x - mean(.x))/sd(.x), by = am) %>%
#'     head()
#'
#' # modify all non-grouping variables to new variables
#' mtcars %>%
#'     let_all(scaled = (.x - mean(.x))/sd(.x), by = am) %>%
#'     head()
#'
#' # conditionally modify all variables
#' iris %>%
#'     let_all(mean = if(is.numeric(.x)) mean(.x)) %>%
#'     head()
#'
#' # modify all variables conditionally on name
#' iris %>%
#'     let_all(
#'         mean = if(startsWith(.name, "Sepal")) mean(.x),
#'         median = if(startsWith(.name, "Petal")) median(.x),
#'         by = Species
#'     ) %>%
#'     head()
#'
#' # aggregation with 'take_all'
#' mtcars %>%
#'     take_all(mean = mean(.x), sd = sd(.x), n = .N, by = am)
#'
#' # conditionally aggregate all variables
#' iris %>%
#'     take_all(mean = if(is.numeric(.x)) mean(.x))
#'
#' # aggregate all variables conditionally on name
#' iris %>%
#'     take_all(
#'         mean = if(startsWith(.name, "Sepal")) mean(.x),
#'         median = if(startsWith(.name, "Petal")) median(.x),
#'         by = Species
#'     )
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
#' take_all(dat, sum, by=x)                      # sum of all (other) columns for each group
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
                  keyby
){
    UseMethod("let_if")
}

#' @export
let_if.etable = function(data,
                  i,
                  ...,
                  by,
                  keyby
){
   data_class = class(data)
   data = as.data.table(data)
   res = eval.parent(
           substitute(maditr::let_if(data,
                                     i,
                                     ...,
                                     by = by,
                                     keyby = keyby
           )
           )
       )
   setDF(res)
   class(res) = data_class
   res
}

#' @export
let_if.data.frame = function(data,
                  i,
                  ...,
                  by,
                  keyby
){
    # if data is expression we want to calculate it only once
    res = force(data)
    parent_frame = parent.frame()

    j_list = as.list(substitute(list(...)))[-1]
    j_length = length(j_list)
    j_length>0 || stop("let/let_if: please, provide at least one expression.")

    # check for names
    all_names = names(j_list)
    if(is.null(all_names)){
        all_names = rep("", j_length)
    }
    for(each in seq_along(all_names)){
        (all_names[each] == "") &&
            !(is.call(j_list[[each]]) && identical(j_list[[each]][[1]], as.symbol(":="))) &&
            stop(sprintf("let/let_if: '%s' - incorrect expression. All expressions should be
                             in the form  'var_name = expression' or 'var_name := expression'.", safe_deparse(j_list[[each]])))

        if(all_names[each]!=""){
            curr_name = all_names[each]
            expr = j_list[[each]]
            j_list[[each]] = substitute(`:=`(curr_name, expr))
        }

    }
    ####

    # NULL is just a placeholder
    for(expr in j_list){
        data_names = names(res)
        expr[[2]] = replace_column_expr(expr[[2]],
                                        data_names =  data_names,
                                        frame = parent_frame,
                                        new = TRUE)

        expr = substitute(NULL[i, expr, by = by, keyby = keyby])
        res = eval_in_parent_frame(res, expr, frame = parent_frame)
    }
    res

}

#' @rdname let_if
#' @export
take_if = function(data,
                   i,
                   ...,
                   by,
                   keyby,
                   .SDcols,
                   autoname = TRUE,
                   fun = NULL
){
    UseMethod("take_if")
}

#' @export
take_if.data.frame = function(data,
                              i,
                              ...,
                              by,
                              keyby,
                              .SDcols,
                              autoname = TRUE,
                              fun = NULL
){
    j_expr = substitute(list(...))
    data = force(data)
    parent_frame = parent.frame()

    j_expr = as.list(j_expr)[-1]
    j_length = length(j_expr)
    # if data is expression we want to calculate it only once

    # NULL is just a placeholder
    if(j_length == 0){
        # no j-arguments
        if(is.null(fun)){
            expr = substitute(NULL[i, ])


        } else {
            expr = substitute(NULL[i,
                                   lapply(.SD, fun),
                                   by = by,
                                   keyby = keyby,
                                   .SDcols = .SDcols]
            )
        }
    } else {
        # naming
        j_expr = add_names_from_walrus_assignement(j_expr, envir = parent.frame())
        if(autoname){
            j_expr = add_names_to_quoted_list(j_expr)
        }
        j_expr = as.call(c(list(quote(list)), j_expr))

        ###################
        if(!is.null(fun)){
            j_expr = substitute(lapply(j_expr, fun))
        }

        expr = substitute(NULL[
            i,
            j_expr,
            by = by,
            keyby = keyby,
            .SDcols = .SDcols
        ])
    }
    eval_in_parent_frame(data, expr, frame = parent_frame)
}

#' @rdname let_if
#' @export
take = function(data,
                ...,
                by,
                keyby,
                .SDcols,
                autoname = TRUE,
                fun = NULL
){
    UseMethod("take")
}

#' @export
take.data.frame = function(data,
                ...,
                by,
                keyby,
                .SDcols,
                autoname = TRUE,
                fun = NULL,
                i
){
    eval.parent(
        substitute(maditr::take_if(data,
                           i,
                           ...,
                           by = by,
                           keyby = keyby,
                           .SDcols = .SDcols,
                           autoname = autoname,
                           fun = fun
                           )
                   )
    )
}

#' @rdname let_if
#' @export
let = function(data,
               ...,
               by,
               keyby
){
    UseMethod("let")
}

#' @rdname let_if
#' @export
let.data.frame = function(data,
               ...,
               by,
               keyby,
               i
){
    eval.parent(
        substitute(maditr::let_if(data,
                           i,
                           ...,
                           by = by,
                           keyby = keyby
        )
        )
    )
}

#' @rdname let_if
#' @export
let.etable = function(data,
                          ...,
                          by,
                          keyby,
                          i
){
    eval.parent(
        substitute(maditr::let_if(data,
                                  i,
                                  ...,
                                  by = by,
                                  keyby = keyby
        )
        )
    )
}




#######################################

#' @rdname let_if
#' @export
sort_by = function(data, ..., na.last = FALSE){
    UseMethod("sort_by")
}


#' @export
sort_by.data.frame = function(data, ..., na.last = FALSE){
    # all_args = substitute(list(...))
    # sort_order = lapply(all_args, function(item) if(is.call(item) && identical(item[[1]], quote(`-`))) -1 else 1)
    # all_args
    parent_frame = parent.frame()
    # if data is expression we want to calculate it only once
    data = force(data)
    # NULL is just a placeholder
    expr = substitute(data.table::setorder(NULL, ..., na.last = na.last))
    eval_in_parent_frame(data, expr, frame = parent_frame, need_expansion = FALSE)

}

