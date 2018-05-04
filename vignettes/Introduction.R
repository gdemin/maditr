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

