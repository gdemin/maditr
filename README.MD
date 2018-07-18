# maditr

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/maditr)](https://cran.r-project.org/package=maditr)
[![](https://cranlogs.r-pkg.org/badges/maditr)](https://cran.rstudio.com/web/packages/maditr/index.html)
[![](https://cranlogs.r-pkg.org/badges/grand-total/maditr)](https://cran.rstudio.com/web/packages/maditr/index.html)
[![Coverage Status](https://img.shields.io/codecov/c/github/gdemin/maditr/master.svg)](https://codecov.io/github/gdemin/maditr?branch=master)

## Overview

Package provides pipe-style interface for [data.table](https://cran.r-project.org/package=data.table) package. It preserves all data.table features without significant impact on performance. `let` and `take` functions are simplified interfaces for most common data manipulation tasks. `query_if` function translates its arguments one-to-one to `[.data.table` method. Additionally there are some conveniences such as automatic `data.frame` conversion to `data.table`.

- To select rows from data: `take_if(mtcars, am==0)`
- To select columns from data: `take(mtcars, am, vs, mpg)`
- To aggregate data: `take(mtcars, mean_mpg = mean(mpg), by = am)`
- To aggregate all non-grouping columns: `take(mtcars, fun = mean, by = am)`
- To aggregate several columns with one summary: `take(mtcars, mpg, hp, fun = mean, by = am)`
- To get total summary skip `by` argument: `take(mtcars, fun = mean)`
- Use magrittr pipe `%>%` to chain several operations: 
```R
     mtcars %>%
        let(mpg_hp = mpg/hp) %>%
        take(mean(mpg_hp), by = am)
```
- To modify variables or add new variables: 
```R
      mtcars %>%
         let(new_var = 42,
             new_var2 = new_var*hp) %>%
          head()
```          
- To drop variable assign NULL: `let(mtcars, am = NULL) %>% head()`
- For parametric assignment use `:=`: 
```R
     new_var = "my_var"
     old_var = "mpg"
     mtcars %>%
         let((new_var) := get(old_var)*2) %>%
         head()
         
    # or,  
    expr = quote(mean(cyl))
    mtcars %>% 
        let((new_var) := eval(expr)) %>% 
        head()
        
    # the same with `take` 
    by_var = "vs,am"
    take(mtcars, (new_var) := eval(expr), by = by_var)
```         


### Links

- [maditr on CRAN](https://cran.r-project.org/package=maditr)
- [maditr on Github](https://github.com/gdemin/maditr)
- [Issues](https://github.com/gdemin/maditr/issues)

### Installation

`maditr` is on CRAN, so for installation you can print in the console
`install.packages("maditr")`.

### Some examples

We will use for demonstartion well-known `mtcars` dataset and some examples from `dplyr` package. 

```R
library(maditr)

data(mtcars)

# Newly created variables are available immediately
mtcars %>%
    let(
        cyl2 = cyl * 2,
        cyl4 = cyl2 * 2
    ) %>% head()

# You can also use let() to remove variables and
# modify existing variables
mtcars %>%
    let(
        mpg = NULL,
        disp = disp * 0.0163871 # convert to litres
    ) %>% head()


# window functions are useful for grouped computations
mtcars %>%
    let(rank = rank(-mpg, ties.method = "min"),
        by = cyl) %>%
    head()

# You can drop variables by setting them to NULL
mtcars %>%
    let(cyl = NULL) %>%
    head()

# keeps all existing variables
mtcars %>%
    let(displ_l = disp / 61.0237) %>%
    head()

# keeps only the variables you create
mtcars %>%
    take(displ_l = disp / 61.0237)


# can refer to both contextual variables and variable names:
var = 100
mtcars %>%
    let(cyl = cyl * var) %>%
    head()

# filter by condition
mtcars %>%
    take_if(am==0)

# filter by compound condition
mtcars %>%
    take_if(am==0 & mpg>mean(mpg))


# A 'take' with summary functions applied without 'by' argument returns an aggregated data
mtcars %>%
    take(mean = mean(disp), n = .N)

# Usually, you'll want to group first
mtcars %>%
    take(mean = mean(disp), n = .N, by = am)

# grouping by multiple variables
mtcars %>%
    take(mean = mean(disp), n = .N, by = list(am, vs))
    
# You can group by expressions:
mtcars %>%
    take(
        fun = mean,
        by = list(vsam = vs + am)
    )

# parametric evaluation:
var = quote(mean(cyl))
mtcars %>% 
    let(mean_cyl = eval(var)) %>% 
    head()
take(mtcars, eval(var))

# all together
new_var = "mean_cyl"
mtcars %>% 
    let((new_var) := eval(var)) %>% 
    head()
take(mtcars, (new_var) := eval(var))

```

## 'dplyr'-like interface for data.table.

There are a small subset of 'dplyr' verbs to work with data.table. Note that there is no `group_by`
verb - use `by` or `keyby` argument when needed.

- `dt_mutate` adds new variables or modify existing variables. If data is data.table then it modifies in-place.
- `dt_summarize` computes summary statistics. Splits the data into subsets, computes summary statistics for each, and returns the result in the "data.table" form.
- `dt_summarize_all` the same as `dt_summarize` but work over all non-grouping variables.
- `dt_filter` Selects rows/cases where conditions are true. Rows where the condition evaluates to NA are dropped.
- `dt_select` Selects column/variables from the data.set.
- `dt_arrange` sorts dataset by variable(-s). Use '-' to sort in desending order. If data is data.table then it modifies in-place.

```R
# examples from 'dplyr'
# newly created variables are available immediately
mtcars  %>%
    dt_mutate(
        cyl2 = cyl * 2,
        cyl4 = cyl2 * 2
    ) %>%
    head()


# you can also use dt_mutate() to remove variables and
# modify existing variables
mtcars %>%
    dt_mutate(
        mpg = NULL,
        disp = disp * 0.0163871 # convert to litres
    ) %>%
    head()


# window functions are useful for grouped mutates
mtcars %>%
    dt_mutate(
        rank = rank(-mpg, ties.method = "min"),
        keyby = cyl) %>%
    print()


# You can drop variables by setting them to NULL
mtcars %>% dt_mutate(cyl = NULL) %>% head()

# A summary applied without by returns a single row
mtcars %>%
    dt_summarise(mean = mean(disp), n = .N)

# Usually, you'll want to group first
mtcars %>%
    dt_summarise(mean = mean(disp), n = .N, by = cyl)


# Multiple 'by' - variables
mtcars %>%
    dt_summarise(cyl_n = .N, by = list(cyl, vs))

# Newly created summaries immediately
# doesn't overwrite existing variables
mtcars %>%
    dt_summarise(disp = mean(disp),
                  sd = sd(disp),
                  by = cyl)

# You can group by expressions:
mtcars %>%
    dt_summarise_all(mean, by = list(vsam = vs + am))

# filter by condition
mtcars %>%
    dt_filter(am==0)

# filter by compound condition
mtcars %>%
    dt_filter(am==0,  mpg>mean(mpg))


# select
mtcars %>% dt_select(vs:carb, cyl)
mtcars %>% dt_select(-am, -cyl)

# sorting
dt_arrange(mtcars, cyl, disp)
dt_arrange(mtcars, -disp)
```




