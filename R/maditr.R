#' maditr: Pipe-Style Interface for 'data.table'
#'
#' Package provides pipe-style interface for \code{data.table}. It preserves
#' all data.table features without significant impact on performance. '\code{let}'
#' and '\code{take}' functions are simplified interfaces for most common data
#' manipulation tasks.
#' \itemize{
#' \item{To select rows from data: }{\code{take_if(mtcars, am==0)}}
#' \item{To select columns from data: }{\code{take(mtcars, am, vs, mpg)}}
#' \item{To aggregate data: }{\code{take(mtcars, mean_mpg = mean(mpg), by = am)}}
#' \item{To aggregate all non-grouping columns: }{\code{take_all(mtcars, mean, by = am)}}
#' \item{To conditionally aggregate all non-grouping columns: }{\code{take_all(iris, if(is.numeric(.x)) mean(.x))}}
#' \item{To aggregate several columns with one summary: }{\code{take(mtcars, mpg, hp, fun = mean, by = am)}}
#' \item{To get total summary skip 'by' argument: }{\code{take_all(mtcars, mean)}}
#' \item{Use magrittr pipe '\%>\%' to chain several operations: }{\preformatted{
#'      mtcars \%>\%
#'         let(mpg_hp = mpg/hp) \%>\%
#'         take(mean(mpg_hp), by = am)
#' }}
#' \item{To modify variables or add new variables: }{\preformatted{
#'       mtcars \%>\%
#'          let(new_var = 42,
#'              new_var2 = new_var*hp) \%>\%
#'           head()}}
#' \item{To modify all non-grouping variables: }{\preformatted{
#'       iris \%>\%
#'          let_all(
#'              scaled = (.x - mean(.x))/sd(.x),
#'              by = Species) \%>\%
#'           head()}}
#' \item{To drop variable assign NULL: }{\code{let(mtcars, am = NULL) \%>\% head()}}
#' \item{To aggregate all variables conditionally on name: }{\preformatted{
#'       iris \%>\%
#'           take_all(
#'               mean = if(startsWith(.name, "Sepal")) mean(.x),
#'               median = if(startsWith(.name, "Petal")) median(.x),
#'               by = Species
#'           )
#'       }}
#' \item{For parametric assignment use ':=': }{\preformatted{
#'      new_var = "my_var"
#'      old_var = "mpg"
#'      mtcars \%>\%
#'          let((new_var) := get(old_var)*2) \%>\%
#'          head()}}
#' \item{For more sophisticated operations see 'query'/'query_if': }{these
#' functions translates its arguments one-to-one to '\code{[.data.table}'
#' method. Additionally there are some conveniences such as automatic
#' 'data.frame' conversion to 'data.table'.}
#' }
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
#' @docType package
#' @name maditr
NULL

#' @import magrittr
#' @import data.table


#' @export
magrittr::`%>%`

#' @export
magrittr::`%<>%`

#' @export
magrittr::`%$%`

#' @export
magrittr::`%T>%`

#' @export
data.table::data.table

#' @export
data.table::copy

#' @export
data.table::as.data.table
#' @export
data.table::is.data.table
#' @export
data.table::last
#' @export
data.table::first
#' @export
data.table::like
#' @export
data.table::`%like%`
#' @export
data.table::between
#' @export
data.table::`%between%`
#' @export
data.table::inrange
#' @export
data.table::`%inrange%`
#' @export
data.table::`:=`
#' @export
data.table::setnames
#' @export
data.table::setcolorder
#' @export
data.table::set
#' @export
data.table::setDT
#' @export
data.table::setDF
#' @export
data.table::setorder
#' @export
data.table::setorderv
#' @export
data.table::setkey
#' @export
data.table::setkeyv
#' @export
data.table::chmatch
#' @export
data.table::`%chin%`
#' @export
data.table::rbindlist
#' @export
data.table::fread
#' @export
data.table::fwrite
#' @export
data.table::foverlaps
#' @export
data.table::shift
#' @export
data.table::transpose
#' @export
data.table::tstrsplit
#' @export
data.table::frank
#' @export
data.table::frankv
#' @export
data.table::.SD
#' @export
data.table::.N
#' @export
data.table::.I
#' @export
data.table::.GRP
#' @export
data.table::.BY
#' @export
data.table::rleid
#' @export
data.table::rleidv
#' @export
data.table::rowid
#' @export
data.table::rowidv
#' @export
data.table::uniqueN
#' @export
data.table::setDTthreads
#' @export
data.table::getDTthreads
#' @export
data.table::fintersect
#' @export
data.table::fsetdiff
#' @export
data.table::funion
#' @export
data.table::fsetequal
#' @export
data.table::shouldPrint
#' @export
data.table::fsort
#' @export
data.table::groupingsets
#' @export
data.table::cube
#' @export
data.table::rollup
#' @export
data.table::nafill
#' @export
data.table::setnafill
#' @export
data.table::frollmean
#' @export
data.table::frollsum
#' @export
data.table::fcoalesce
#' @export
data.table::fifelse
#' @export
data.table::frollapply
