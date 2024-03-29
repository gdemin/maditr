% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{query_if}
\alias{query_if}
\alias{query}
\title{One-to-one interface for data.table '[' method}
\usage{
query_if(
  data,
  i,
  j,
  by,
  keyby,
  with = TRUE,
  nomatch = getOption("datatable.nomatch"),
  mult = "all",
  roll = FALSE,
  rollends = if (roll == "nearest") c(TRUE, TRUE) else if (roll >= 0) c(FALSE, TRUE) else
    c(TRUE, FALSE),
  which = FALSE,
  .SDcols,
  verbose = getOption("datatable.verbose"),
  allow.cartesian = getOption("datatable.allow.cartesian"),
  drop = NULL,
  on = NULL
)

query(
  data,
  j,
  by,
  keyby,
  with = TRUE,
  nomatch = getOption("datatable.nomatch"),
  mult = "all",
  roll = FALSE,
  rollends = if (roll == "nearest") c(TRUE, TRUE) else if (roll >= 0) c(FALSE, TRUE) else
    c(TRUE, FALSE),
  which = FALSE,
  .SDcols,
  verbose = getOption("datatable.verbose"),
  allow.cartesian = getOption("datatable.allow.cartesian"),
  drop = NULL,
  on = NULL
)
}
\arguments{
\item{data}{data.table/data.frame data.frame will be automatically converted
to data.table.}

\item{i}{Integer, logical or character vector, single column numeric matrix,
expression of column names, list, data.frame or data.table. integer and
logical vectors work the same way they do in [.data.frame except logical
NAs are treated as FALSE. expression is evaluated within the frame of the
data.table (i.e. it sees column names as if they are variables) and can
evaluate to any of the other types. For details see
\link[data.table:data.table]{data.table}}

\item{j}{When with=TRUE (default), j is evaluated within the frame of the
data.table; i.e., it sees column names as if they are variables. This
allows to not just select columns in j, but also compute on them e.g., \code{x[,a]}
and \code{x[, sum(a)]} returns \code{x$a} and \code{sum(x$a)} as a vector respectively.
\code{x[, .(a, b)]} and \code{x[, .(sa=sum(a), sb=sum(b))]} returns a two column data.table
each, the first simply selecting columns a, b and the second computing
their sums. For details see \link[data.table:data.table]{data.table}.}

\item{by}{unquoted name of grouping variable of list of unquoted names of
grouping variables. For details see \link[data.table:data.table]{data.table}}

\item{keyby}{Same as by, but with an additional \code{setkey()} run on the by
columns of the result, for convenience. It is common practice to use
'keyby=' routinely when you wish the result to be sorted. For details see
\link[data.table:data.table]{data.table}}

\item{with}{logical. For details see \link[data.table:data.table]{data.table}.}

\item{nomatch}{Same as nomatch in match. For details see
\link[data.table:data.table]{data.table}.}

\item{mult}{For details see \link[data.table:data.table]{data.table}.}

\item{roll}{For details see \link[data.table:data.table]{data.table}.}

\item{rollends}{For details see \link[data.table:data.table]{data.table}.}

\item{which}{For details see \link[data.table:data.table]{data.table}.}

\item{.SDcols}{Specifies the columns of x to be included in the special
symbol .SD which stands for Subset of data.table. May be character column
names or numeric positions. For details see \link[data.table:data.table]{data.table}.}

\item{verbose}{logical. For details see \link[data.table:data.table]{data.table}.}

\item{allow.cartesian}{For details see \link[data.table:data.table]{data.table}.}

\item{drop}{For details see \link[data.table:data.table]{data.table}.}

\item{on}{For details see \link[data.table:data.table]{data.table}.}
}
\value{
It depends. For details see \link[data.table:data.table]{data.table}.
}
\description{
Quote from \link[data.table:data.table]{data.table}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{query(data, j,  by) # + extra arguments
            |   |
            |    -------> grouped by what?
             -------> what to do?
}\if{html}{\out{</div>}}

or,

\if{html}{\out{<div class="sourceCode">}}\preformatted{query_if(data, i,  j,  by) # + extra arguments
               |   |   |
               |   |    -------> grouped by what?
               |    -------> what to do?
                ---> on which rows?
}\if{html}{\out{</div>}}

If you don't need '\code{i}' argument, use '\code{query}'. In this case you can
avoid printing leading comma inside brackets to denote empty '\code{i}'.
}
\examples{
\donttest{
# examples from data.table
dat = data.table(x=rep(c("b","a","c"),each=3), y=c(1,3,6), v=1:9)
dat
# basic row subset operations
query_if(dat, 2)                        # 2nd row
query_if(dat, 3:2)                        # 3rd and 2nd row
query_if(dat, order(x))                   # no need for order(dat$x)
query_if(dat, y>2)                        # all rows where dat$y > 2
query_if(dat, y>2 & v>5)                  # compound logical expressions
query_if(dat, !2:4)                       # all rows other than 2:4
query_if(dat, -(2:4))                     # same

# select|compute columns data.table way
query(dat, v)                        # v column (as vector)
query(dat, list(v))                  # v column (as data.table)
query(dat, sum(v))                   # sum of column v, returned as vector
query(dat, list(sum(v)))             # same, but return data.table (column autonamed V1)
query(dat, list(v, v*2))             # return two column data.table, v and v*2

# subset rows and select|compute data.table way
query_if(dat, 2:3, sum(v))                # sum(v) over rows 2 and 3, return vector
query_if(dat, 2:3, list(sum(v)))             # same, but return data.table with column V1
query_if(dat, 2:3, list(sv=sum(v)))          # same, but return data.table with column sv
query_if(dat, 2:5, cat(v, "\n"))          # just for j's side effect

# select columns the data.frame way
query(dat, 2, with=FALSE)            # 2nd column, returns a data.table always
colNum = 2
query(dat, colNum, with=FALSE)       # same, equivalent to DT[, .SD, .SDcols=colNum]

# grouping operations - j and by
query(dat, sum(v), by=x)             # ad hoc by, order of groups preserved in result
query(dat, sum(v), keyby=x)          # same, but order the result on by cols
query(dat, sum(v), by=x) \%>\%
    query_if(order(x))               # same but by chaining expressions together

# fast ad hoc row subsets (subsets as joins)
# same as x == "a" but uses binary search (fast)
query_if(dat, "a", on="x")
# same, for convenience, no need to quote every column
query_if(dat, "a", on=list(x))
query_if(dat, .("a"), on="x")                          # same
# same, single "==" internally optimised to use binary search (fast)
query_if(dat, x=="a")
# not yet optimized, currently vector scan subset
query_if(dat, x!="b" | y!=3)
# join on columns x,y of 'dat'; uses binary search (fast)
query_if(dat, .("b", 3), on=c("x", "y"))
query_if(dat, .("b", 3), on=list(x, y))                # same, but using on=list()
query_if(dat, .("b", 1:2), on=c("x", "y"))             # no match returns NA
query_if(dat, .("b", 1:2), on=.(x, y), nomatch=0)      # no match row is not returned
# locf, nomatch row gets rolled by previous row
query_if(dat, .("b", 1:2), on=c("x", "y"), roll=Inf)
query_if(dat, .("b", 1:2), on=.(x, y), roll=-Inf)      # nocb, nomatch row gets rolled by next row
# on rows where dat$x=="b", calculate sum(v*y)
query_if(dat, "b", sum(v*y), on="x")

# all together now
query_if(dat, x!="a", sum(v), by=x)                    # get sum(v) by "x" for each i != "a"
query_if(dat, !"a", sum(v), by=.EACHI, on="x")         # same, but using subsets-as-joins
query_if(dat, c("b","c"), sum(v), by=.EACHI, on="x")   # same
query_if(dat, c("b","c"), sum(v), by=.EACHI, on=.(x))  # same, using on=.()

# joins as subsets
X = data.table(x=c("c","b"), v=8:7, foo=c(4,2))
X

query_if(dat, X, on="x")                         # right join
query_if(X, dat, on="x")                         # left join
query_if(dat, X, on="x", nomatch=0)              # inner join
query_if(dat, !X, on="x")                        # not join
# join using column "y" of 'dat' with column "v" of X
query_if(dat, X, on=c(y="v"))
query_if(dat,X, on="y==v")                       # same as above (v1.9.8+)

query_if(dat, X, on = .(y<=foo))                 # NEW non-equi join (v1.9.8+)
query_if(dat, X, on="y<=foo")                    # same as above
query_if(dat, X, on=c("y<=foo"))                 # same as above
query_if(dat, X, on=.(y>=foo))                   # NEW non-equi join (v1.9.8+)
query_if(dat, X, on=.(x, y<=foo))                # NEW non-equi join (v1.9.8+)
query_if(dat, X, .(x,y,x.y,v), on=.(x, y>=foo))  # Select x's join columns as well

query_if(dat, X, on="x", mult="first")           # first row of each group
query_if(dat, X, on="x", mult="last")            # last row of each group
query_if(dat, X, sum(v), by=.EACHI, on="x")      # join and eval j for each row in i
query_if(dat, X, sum(v)*foo, by=.EACHI, on="x")  # join inherited scope
query_if(dat, X, sum(v)*i.v, by=.EACHI, on="x")  # 'i,v' refers to X's v column
query_if(dat, X, on=.(x, v>=v), sum(y)*foo, by=.EACHI) # NEW non-equi join with by=.EACHI (v1.9.8+)


# more on special symbols, see also ?"special-symbols"
query_if(dat, .N)                           # last row
query(dat, .N)                              # total number of rows in DT
query(dat, .N, by=x)                        # number of rows in each group
query(dat, .SD, .SDcols=x:y)                # select columns 'x' and 'y'
query(dat, .SD[1])                          # first row of all columns
query(dat, .SD[1], by=x)                    # first row of 'y' and 'v' for each group in 'x'
query(dat, c(.N, lapply(.SD, sum)), by=x)   # get rows *and* sum columns 'v' and 'y' by group
query(dat, .I[1], by=x)                     # row number in DT corresponding to each group
query(dat, grp := .GRP, by=x) \%>\% head()    # add a group counter column
query(X, query_if(dat, .BY, y, on="x"), by=x)               # join within each group

# add/update/delete by reference (see ?assign)
query(dat, z:=42L) \%>\% head()         # add new column by reference
query(dat, z:=NULL) \%>\% head()        # remove column by reference
query_if(dat, "a", v:=42L, on="x") \%>\% head()  # subassign to existing v column by reference
query_if(dat, "b", v2:=84L, on="x") \%>\% head() # subassign to new column by reference (NA padded)

# NB: postfix [] is shortcut to print()
query(dat, m:=mean(v), by=x)[]              # add new column by reference by group

# advanced usage
dat = data.table(x=rep(c("b","a","c"),each=3),
                 v=c(1,1,1,2,2,1,1,2,2),
                 y=c(1,3,6),
                 a=1:9,
                 b=9:1)
dat
query(dat, sum(v), by=.(y\%\%2))              # expressions in by
query(dat, sum(v), by=.(bool = y\%\%2))       # same, using a named list to change by column name
query(dat, .SD[2], by=x)                    # get 2nd row of each group
query(dat, tail(.SD,2), by=x)               # last 2 rows of each group
query(dat, lapply(.SD, sum), by=x)          # sum of all (other) columns for each group
query(dat, .SD[which.min(v)], by=x)         # nested query by group

query(dat, list(MySum=sum(v),
                MyMin=min(v),
                MyMax=max(v)),
      by=.(x, y\%\%2)
)                    # by 2 expressions

query(dat, .(a = .(a), b = .(b)), by=x)      # list columns
query(dat, .(seq = min(a):max(b)), by=x)     # j is not limited to just aggregations
query(dat, sum(v), by=x) \%>\%
    query_if(V1<20) # compound query
query(dat, sum(v), by=x) \%>\%
    setorder(-V1) \%>\%
    head()          # ordering results
query(dat, c(.N, lapply(.SD,sum)), by=x)     # get number of observations and sum per group

# anonymous lambda in 'j', j accepts any valid
# expression. TO REMEMBER: every element of
# the list becomes a column in result.
query(dat,
      {tmp = mean(y);
      .(a = a-tmp, b = b-tmp)
      },
      by=x)

# using rleid, get max(y) and min of all cols in .SDcols for each consecutive run of 'v'
query(dat,
      c(.(y=max(y)), lapply(.SD, min)),
      by=rleid(v),
      .SDcols=v:b
)
}
\dontrun{
    pdf("new.pdf")
    query(dat, plot(a,b), by=x)                # can also plot in 'j'
    dev.off()
}
}
