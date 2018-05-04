## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#       mtcars %>%
#          let(mpg_hp = mpg/hp) %>%
#          take(mean(mpg_hp), by = am)

## ---- eval=FALSE---------------------------------------------------------
#        mtcars %>%
#           let(new_var = 42,
#               new_var2 = new_var*hp) %>%
#            head()

## ---- eval=FALSE---------------------------------------------------------
#       new_var = "my_var"
#       old_var = "mpg"
#       mtcars %>%
#           let((new_var) := get(old_var)*2) %>%
#           head()

## ------------------------------------------------------------------------
library(maditr)

data(mtcars)

# Newly created variables are available immediately
mtcars %>% let(
    cyl2 = cyl * 2,
    cyl4 = cyl2 * 2
) %>% head()

# You can also use let() to remove variables and
# modify existing variables
mtcars %>% let(
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
    take(displ_l = disp / 61.0237) %>% 
    head()


# can refer to both contextual variables and variable names:
var = 100
mtcars %>%
    let(cyl = cyl * var) %>%
    head()

# filter by condition
mtcars %>%
    take_if(am==0) %>% 
    head()

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

# parametric evaluation:
var = quote(mean(cyl))
take(mtcars, eval(var))


# You can group by expressions:
mtcars %>%
    take(
        fun = mean,
        by = list(vsam = vs + am)
    )

