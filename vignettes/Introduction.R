## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
data.table::setDTthreads(2)

## ----eval=FALSE---------------------------------------------------------------
#       mtcars %>%
#          let(mpg_hp = mpg/hp) %>%
#          take(mean(mpg_hp), by = am)

## ----eval=FALSE---------------------------------------------------------------
#        mtcars %>%
#           let(new_var = 42,
#               new_var2 = new_var*hp) %>%
#           head()

## ----eval=FALSE---------------------------------------------------------------
#      iris %>%
#        let_all(
#            scaled = (.x - mean(.x))/sd(.x),
#            by = Species) %>%
#         head()

## ----eval=FALSE---------------------------------------------------------------
#      iris %>%
#        take_all(
#            mean = if(startsWith(.name, "Sepal")) mean(.x),
#            median = if(startsWith(.name, "Petal")) median(.x),
#            by = Species
#        )

## ----eval=FALSE---------------------------------------------------------------
#      new_var = "my_var"
#      old_var = "mpg"
#      mtcars %>%
#          let((new_var) := get(old_var)*2) %>%
#          head()
#  
#      # or,
#      expr = quote(mean(cyl))
#      mtcars %>%
#          let((new_var) := eval(expr)) %>%
#          head()
#  
#      # the same with `take`
#      by_var = "vs,am"
#      take(mtcars, (new_var) := eval(expr), by = by_var)

## ----include=FALSE------------------------------------------------------------
library(maditr)

## -----------------------------------------------------------------------------

workers = fread("
    name company
    Nick Acme
    John Ajax
    Daniela Ajax
")

positions = fread("
    name position
    John designer
    Daniela engineer
    Cathie manager
")

# xlookup
workers = let(workers,
  position = xlookup(name, positions$name, positions$position)
)

# vlookup
# by default we search in the first column and return values from second column
workers = let(workers,
  position = vlookup(name, positions, no_match = "Not found")
)

# the same 
workers = let(workers,
  position = vlookup(name, positions, 
                     result_column = "position", 
                     no_match = "Not found") # or, result_column = 2 
)

head(workers)

## -----------------------------------------------------------------------------
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
    take(displ_l = disp / 61.0237) %>% 
    head()


# can refer to both contextual variables and variable names:
var = 100
mtcars %>%
    let(cyl = cyl * var) %>%
    head()

# select rows
mtcars %>%
    rows(am==0) %>% 
    head()

# select rows with compound condition
mtcars %>%
    rows(am==0 & mpg>mean(mpg))

# select columns
mtcars %>% 
    columns(vs:carb, cyl)
    
mtcars %>% 
    columns(-am, -cyl)    

# regular expression pattern
columns(iris, "^Petal") %>% head() # variables which start from 'Petal'
columns(iris, "Width$") %>% head() # variables which end with 'Width'

# move Species variable to the front
# pattern "^." matches all variables
columns(iris, Species, "^.") %>% head()

# pattern "^.*al" means "contains 'al'"
columns(iris, "^.*al") %>% head()

# numeric indexing - all variables except Species
columns(iris, 1:4) %>% head()

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
    take_all(
        mean,
        by = list(vsam = vs + am)
    )

# modify all non-grouping variables in-place
mtcars %>%
    let_all((.x - mean(.x))/sd(.x), by = am) %>%
    head()

# modify all non-grouping variables to new variables
mtcars %>%
    let_all(scaled = (.x - mean(.x))/sd(.x), by = am) %>%
    head()

# conditionally modify all variables
iris %>%
    let_all(mean = if(is.numeric(.x)) mean(.x)) %>%
    head()

# modify all variables conditionally on name
iris %>%
    let_all(
        mean = if(startsWith(.name, "Sepal")) mean(.x),
        median = if(startsWith(.name, "Petal")) median(.x),
        by = Species
    ) %>%
    head()

# aggregation with 'take_all'
mtcars %>%
    take_all(mean = mean(.x), sd = sd(.x), n = .N, by = am)

# conditionally aggregate all variables
iris %>%
    take_all(mean = if(is.numeric(.x)) mean(.x))

# aggregate all variables conditionally on name
iris %>%
    take_all(
        mean = if(startsWith(.name, "Sepal")) mean(.x),
        median = if(startsWith(.name, "Petal")) median(.x),
        by = Species
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



## -----------------------------------------------------------------------------
# range selection
iris %>% 
    let(
        avg = rowMeans(Sepal.Length %to% Petal.Width)
    ) %>% 
    head()

# multiassignment
iris %>% 
    let(
        # starts with Sepal or Petal
        multipled1 %to% multipled4 := cols("^(Sepal|Petal)")*2
    ) %>% 
    head()


mtcars %>% 
    let(
        # text expansion
        cols("scaled_{names(mtcars)}") := lapply(cols("{names(mtcars)}"), scale)
    ) %>% 
    head()

# range selection in 'by'
# selection of range + additional column
mtcars %>% 
    take(
        res = sum(cols(mpg, disp %to% drat)),
        by = vs %to% gear
    )

## -----------------------------------------------------------------------------
workers = fread("
    name company
    Nick Acme
    John Ajax
    Daniela Ajax
")

positions = fread("
    name position
    John designer
    Daniela engineer
    Cathie manager
")

workers
positions

## -----------------------------------------------------------------------------
workers %>% dt_inner_join(positions)
workers %>% dt_left_join(positions)
workers %>% dt_right_join(positions)
workers %>% dt_full_join(positions)

# filtering joins
workers %>% dt_anti_join(positions)
workers %>% dt_semi_join(positions)

## ----eval=FALSE---------------------------------------------------------------
#  workers %>% dt_left_join(positions, by = "name")

## ----eval=FALSE---------------------------------------------------------------
#  positions2 = setNames(positions, c("worker", "position")) # rename first column in 'positions'
#  workers %>% dt_inner_join(positions2, by = c("name" = "worker"))

