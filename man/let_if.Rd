% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/let.R, R/let_all.R
\name{let_if}
\alias{let_if}
\alias{take_if}
\alias{take}
\alias{let}
\alias{let.data.frame}
\alias{let.etable}
\alias{sort_by}
\alias{let_all}
\alias{take_all}
\title{Modify, aggregate, select or filter data.frame/data.table}
\usage{
let_if(data, i, ..., by, keyby)

take_if(data, i, ..., by, keyby, .SDcols, autoname = TRUE, fun = NULL)

take(data, ..., by, keyby, .SDcols, autoname = TRUE, fun = NULL)

let(data, ..., by, keyby)

\method{let}{data.frame}(data, ..., by, keyby, i)

\method{let}{etable}(data, ..., by, keyby, i)

sort_by(data, ..., na.last = FALSE)

let_all(data, ..., by, keyby, .SDcols, suffix = TRUE, sep = "_", i)

take_all(data, ..., by, keyby, .SDcols, suffix = TRUE, sep = "_", i)
}
\arguments{
\item{data}{data.table/data.frame data.frame will be automatically converted
to data.table. \code{let} modify data.table object in-place.}

\item{i}{integer/logical vector. Supposed to use to subset/conditional
modifications of \code{data}. For details see \link[data.table:data.table]{data.table}}

\item{...}{List of variables or name-value pairs of summary/modifications
functions. The name will be the name of the variable in the result. In the
\code{let} and \code{take} functions we can use \code{a = b} or \code{a := b} notation. Advantages of \verb{:=} is parametric assignment, e. g.
\code{(a) := 2} create variable with name which are stored in \code{a}. In
\code{let} \verb{:=} can be used for multiassignment (\code{c("a", "b")  := list(1,2)}). Expression in the 'take_all' and 'let_all' can use predefined
variables: '.x' is a value of current variable, '.name' is a name of
the variable and '.index' is sequential number of the variable. '.value' is
is an alias to '.x'.}

\item{by}{unquoted name of grouping variable of list of unquoted names of
grouping variables. For details see \link[data.table:data.table]{data.table}}

\item{keyby}{Same as \code{by}, but with an additional \code{setkey()} run on the by
columns of the result, for convenience. It is common practice to use
'keyby=' routinely when you wish the result to be sorted. For details see
\link[data.table:data.table]{data.table}.}

\item{.SDcols}{Specifies the columns of x to be included in the special
symbol .SD which stands for Subset of data.table. May be character column
names or numeric positions. For details see \link[data.table:data.table]{data.table}.}

\item{autoname}{logical. TRUE by default. Should we create names for  unnamed expressions in \code{take}?}

\item{fun}{Function which will be applied to all variables in \code{take}. If
there are no variables in \code{take} then it will be applied to all
non-grouping variables in the \code{data}.}

\item{na.last}{logical. FALSE by default. If TRUE, missing values in the data
are put last; if FALSE, they are put first.}

\item{suffix}{logical TRUE by default. For 'let_all'/'take_all'. If TRUE than
we append summary name to the end of the variable name. If FALSE summary
name will be added at the begining of the variable name.}

\item{sep}{character. "_" by default. Separator between the old variables
name and prefix or suffix for 'let_all' and 'take_all'.}
}
\value{
data.table. \code{let} returns its result invisibly.
}
\description{
\itemize{
\item \code{let} adds new variables or modify existing variables. 'let_if' make
the same thing on the subset of rows.
\item \code{take/take_if} aggregate data or aggregate subset of the data.
\item \code{let_all} applies expressions to all variables in the dataset. It is also
possible to modify the subset of the variables.
\item \code{take_all} aggregates all variables in the dataset. It is also possible
to aggregate the subset of the variables.
}

\if{html}{\out{<div class="sourceCode">}}\preformatted{}\if{html}{\out{</div>}}

All functions return \code{data.table}. Expression in the 'take_all' and
'let_all' can use predefined variables: '.x' is a value of current variable ,
'.name' is a name of the variable and '.index' is sequential number of the
variable. '.value' is is an alias to '.x'.
\itemize{
\item Add new variables: \code{let(mtcars, new_var = 42, new_var2 = new_var*hp)}
\item Select variables: \code{take(mtcars, am, vs, mpg)}
\item Aggregate data: \code{take(mtcars, mean_mpg = mean(mpg), by = am)}
\item Aggregate all non-grouping columns: \code{take_all(mtcars, mean = mean(.x), sd = sd(.x), n = .N, by = am)}
\item Aggregate all numeric columns: \code{take_all(iris, if(is.numeric(.x)) mean(.x))}
\item To modify all non-grouping variables:
}

\if{html}{\out{<div class="sourceCode">}}\preformatted{      iris \%>\%
         let_all(
             scaled = (.x - mean(.x))/sd(.x),
             by = Species) \%>\%
          head()
}\if{html}{\out{</div>}}
\itemize{
\item Aggregate specific columns: \code{take_all(iris, if(startsWith(.name, "Sepal")) mean(.x))}
}

\if{html}{\out{<div class="sourceCode">}}\preformatted{}\if{html}{\out{</div>}}

You can use 'columns' inside expression in the 'take'/'let'. 'columns' will
be replaced with data.table with selected columns. In 'let' in the
expressions with ':=', 'cols' or '\%to\%' can be placed in the left part of the
expression. It is usefull for multiple assignment.
There are four ways of column selection:
\enumerate{
\item Simply by column names
\item By variable ranges, e. g. vs:carb. Alternatively, you can use '\%to\%'
instead of colon: 'vs \%to\% carb'.
\item With regular expressions. Characters which start with '^' or end with '$'
considered as Perl-style regular expression patterns. For example, '^Petal'
returns all variables started with 'Petal'. 'Width$' returns all variables
which end with 'Width'. Pattern '^.' matches all variables and pattern
'^.*my_str' is equivalent to contains "my_str"'.
\item By character variables with interpolated parts. Expression in the curly
brackets inside characters will be evaluated in the parent frame with
\link{text_expand}. For example, \verb{a\{1:3\}} will be transformed to the names 'a1',
'a2', 'a3'. 'cols' is just a shortcut for 'columns'. See examples.
}
}
\examples{
# examples form 'dplyr' package
data(mtcars)
\donttest{
# Newly created variables are available immediately
mtcars \%>\%
    let(
        cyl2 = cyl * 2,
        cyl4 = cyl2 * 2
    ) \%>\% head()

# You can also use let() to remove variables and
# modify existing variables
mtcars \%>\%
    let(
        mpg = NULL,
        disp = disp * 0.0163871 # convert to litres
    ) \%>\% head()


# window functions are useful for grouped computations
mtcars \%>\%
    let(rank = rank(-mpg, ties.method = "min"),
        by = cyl) \%>\%
    head()

# You can drop variables by setting them to NULL
mtcars \%>\% let(cyl = NULL) \%>\% head()

# keeps all existing variables
mtcars \%>\%
    let(displ_l = disp / 61.0237) \%>\%
    head()

# keeps only the variables you create
mtcars \%>\%
    take(displ_l = disp / 61.0237)


# can refer to both contextual variables and variable names:
var = 100
mtcars \%>\%
    let(cyl = cyl * var) \%>\%
    head()


# A 'take' with summary functions applied without 'by' argument returns an aggregated data
mtcars \%>\%
    take(mean = mean(disp), n = .N)

# Usually, you'll want to group first
mtcars \%>\%
    take(mean = mean(disp), n = .N, by = cyl)

# You can group by expressions:
mtcars \%>\%
    take_all(mean, by = list(vsam = vs + am))

# modify all non-grouping variables in-place
mtcars \%>\%
    let_all((.x - mean(.x))/sd(.x), by = am) \%>\%
    head()

# modify all non-grouping variables to new variables
mtcars \%>\%
    let_all(scaled = (.x - mean(.x))/sd(.x), by = am) \%>\%
    head()

# conditionally modify all variables
iris \%>\%
    let_all(mean = if(is.numeric(.x)) mean(.x)) \%>\%
    head()

# modify all variables conditionally on name
iris \%>\%
    let_all(
        mean = if(startsWith(.name, "Sepal")) mean(.x),
        median = if(startsWith(.name, "Petal")) median(.x),
        by = Species
    ) \%>\%
    head()

# aggregation with 'take_all'
mtcars \%>\%
    take_all(mean = mean(.x), sd = sd(.x), n = .N, by = am)

# conditionally aggregate all variables
iris \%>\%
    take_all(mean = if(is.numeric(.x)) mean(.x))

# aggregate all variables conditionally on name
iris \%>\%
    take_all(
        mean = if(startsWith(.name, "Sepal")) mean(.x),
        median = if(startsWith(.name, "Petal")) median(.x),
        by = Species
    )

# parametric evaluation:
var = quote(mean(cyl))
mtcars \%>\%
    let(mean_cyl = eval(var)) \%>\%
    head()
take(mtcars, eval(var))

# all together
new_var = "mean_cyl"
mtcars \%>\%
    let((new_var) := eval(var)) \%>\%
    head()
take(mtcars, (new_var) := eval(var))

########################################

# variable selection

# range selection
iris \%>\%
    let(
        avg = rowMeans(Sepal.Length \%to\% Petal.Width)
    ) \%>\%
    head()

# multiassignment
iris \%>\%
    let(
        # starts with Sepal or Petal
        multipled1 \%to\% multipled4 := cols("^(Sepal|Petal)")*2
    ) \%>\%
    head()


mtcars \%>\%
    let(
        # text expansion
        cols("scaled_{names(mtcars)}") := lapply(cols("{names(mtcars)}"), scale)
    ) \%>\%
    head()

# range selection in 'by'
# range selection  + additional column
mtcars \%>\%
    take(
        res = sum(cols(mpg, disp \%to\% drat)),
        by = vs \%to\% gear
    )

########################################

# examples from data.table
dat = data.table(
    x=rep(c("b","a","c"), each=3),
    y=c(1,3,6),
    v=1:9
)

# basic row subset operations
take_if(dat, 2)                         # 2nd row
take_if(dat, 3:2)                       # 3rd and 2nd row
take_if(dat, order(x))                  # no need for order(dat$x)
take_if(dat, y>2)                       # all rows where dat$y > 2
take_if(dat, y>2 & v>5)                 # compound logical expressions
take_if(dat, !2:4)                      # all rows other than 2:4
take_if(dat, -(2:4))                    # same

# select|compute columns
take(dat, v)                  # v column (as data.table)
take(dat, sum(v))             # return data.table with sum of v (column autonamed 'sum(v)')
take(dat, sv = sum(v))        # same, but column named "sv"
take(dat, v, v*2)             # return two column data.table, v and v*2

# subset rows and select|compute
take_if(dat, 2:3, sum(v))      # sum(v) over rows 2 and 3
take_if(dat, 2:3, sv = sum(v)) # same, but return data.table with column sv

# grouping operations
take(dat, sum(v), by = x)             # ad hoc by, order of groups preserved in result
take(dat, sum(v), keyby = x)          # same, but order the result on by cols


# all together now
take_if(dat, x!="a", sum(v), by=x)                       # get sum(v) by "x" for each x != "a"

# more on special symbols, see also ?"data.table::special-symbols"
take_if(dat, .N)                           # last row
take(dat, .N)                              # total number of rows in DT
take(dat, .N, by=x)                        # number of rows in each group

take(dat, .I[1], by=x)                     # row number in DT corresponding to each group


# add/update/delete by reference
# [] at the end of expression is for autoprinting
let(dat, grp = .GRP, by=x)[]          # add a group counter column
let(dat, z = 42L)[]                   # add new column by reference
let(dat, z = NULL)[]                  # remove column by reference
let_if(dat, x=="a", v = 42L)[]        # subassign to existing v column by reference
let_if(dat, x=="b", v2 = 84L)[]       # subassign to new column by reference (NA padded)

let(dat, m = mean(v), by=x)[]         # add new column by reference by group

# advanced usage
dat = data.table(x=rep(c("b","a","c"), each=3),
                 v=c(1,1,1,2,2,1,1,2,2),
                 y=c(1,3,6),
                 a=1:9,
                 b=9:1)

take(dat, sum(v), by=list(y\%\%2))              # expressions in by
take(dat, sum(v), by=list(bool = y\%\%2))       # same, using a named list to change by column name
take_all(dat, sum, by=x)                      # sum of all (other) columns for each group
take(dat,
     MySum=sum(v),
     MyMin=min(v),
     MyMax=max(v),
     by = list(x, y\%\%2)               # by 2 expressions
)

take(dat, seq = min(a):max(b), by=x)  # j is not limited to just aggregations
dat \%>\%
    take(V1 = sum(v), by=x) \%>\%
    take_if(V1<20)                    # compound query

dat \%>\%
    take(V1 = sum(v), by=x) \%>\%
    sort_by(-V1) \%>\%                  # ordering results
    head()

}
}
