#' Look up values in dictionary.
#'
#' `vlookup` function is inspired by VLOOKUP spreadsheet
#' function. It looks for a `lookup_value` in the `lookup_column` of
#' the `dict`, and then returns values in the same rows from
#' `result_column`.
#' `xlookup` is simplified version of `vlookup`. It searches for a
#' `lookup_value` in the `lookup_vector` and return values in the same
#' position from the `result_vector`.
#'
#' @param lookup_value Vector of looked up values
#' @param dict data.frame. Dictionary.
#' @param result_column numeric or character. Resulting columns in the
#'   `dict`. Default  value for `result_column` is 2 - for frequent
#'   case of dictionary with keys in the first column and results in the second
#'   column.
#' @param lookup_column Column of `dict` in which lookup value will be
#'   searched. By default, it is the first column of the `dict`.
#' @param lookup_vector vector in which 'lookup_value' will be searched during 'xlookup'.
#' @param result_vector vector with resulting values for 'xlookup'.
#' @param no_match vector of length one. NA by default. Where a valid match is
#'   not found, return the 'no_match' value you supply.
#' @return `xlookup` always return vector, `vlookup` returns vector if
#'   the `result_column` is single value. In the opposite case data.frame will
#'   be returned.
#'
#' @export
#' @examples
#' # with data.frame
#' dict = data.frame(num=1:26, small=letters, cap=LETTERS)
#' vlookup(1:3, dict)
#' vlookup(c(45,1:3,58), dict, result_column='cap')
#' vlookup(c(45,1:3,58), dict, result_column='cap', no_match = "Not found")
#'
#' # the same with xlookup
#' xlookup(1:3, dict$num, dict$small)
#' xlookup(c(45,1:3,58), dict$num, dict$cap)
#' xlookup(c(45,1:3,58), dict$num, dict$cap, no_match = "Not found")
#'
#'
#' # example from base 'merge'
#' authors = data.table(
#'     surname = c("Tukey", "Venables", "Tierney", "Ripley", "McNeil"),
#'     nationality = c("US", "Australia", "US", "UK", "Australia"),
#'     deceased = c("yes", rep("no", 4))
#' )
#'
#' books = data.table(
#'     surname = c("Tukey", "Venables", "Tierney",
#'                 "Ripley", "Ripley", "McNeil", "R Core"),
#'     title = c("Exploratory Data Analysis",
#'               "Modern Applied Statistics ...",
#'               "LISP-STAT",
#'               "Spatial Statistics", "Stochastic Simulation",
#'               "Interactive Data Analysis",
#'               "An Introduction to R")
#' )
#'
#' let(books,
#'      c("author_nationality", "author_deceased") := vlookup(surname,
#'              dict = authors,
#'              result_column = 2:3
#'          )
#' )[]
#'
#' # Just for fun. Examples borrowed from Microsoft Excel.
#' # It is not the R way of doing things.
#'
#' # Example 2
#'
#' ex2 = fread("
#'     Item_ID Item Cost Markup
#'     ST-340 Stroller 145.67  0.30
#'     BI-567 Bib 3.56  0.40
#'     DI-328 Diapers  21.45  0.35
#'     WI-989 Wipes  5.12  0.40
#'     AS-469 Aspirator 2.56  0.45
#' ")
#'
#' # Calculates the retail price of diapers by adding the markup percentage to the cost.
#' vlookup("DI-328", ex2, 3) * (1 + vlookup("DI-328", ex2, 4)) # 28.9575
#'
#' # Calculates the sale price of wipes by subtracting a specified discount from
#' # the retail price.
#' (vlookup("WI-989", ex2, "Cost") * (1 + vlookup("WI-989", ex2, "Markup"))) * (1 - 0.2)  # 5.7344
#'
#' A2 = ex2[["Item_ID"]][1]
#' A3 = ex2[["Item_ID"]][2]
#'
#' # If the cost of an item is greater than or equal to $20.00, displays the string
#' # "Markup is nn%"; otherwise, displays the string "Cost is under $20.00".
#' ifelse(vlookup(A2, ex2, "Cost") >= 20,
#'        paste0("Markup is " , 100 * vlookup(A2, ex2, "Markup"),"%"),
#'        "Cost is under $20.00") # Markup is 30%
#'
#'
#' # If the cost of an item is greater than or equal to $20.00, displays the string
#' # Markup is nn%"; otherwise, displays the string "Cost is $n.nn".
#' ifelse(vlookup(A3, ex2, "Cost") >= 20,
#'        paste0("Markup is: " , 100 * vlookup(A3, ex2, "Markup") , "%"),
#'        paste0("Cost is $", vlookup(A3, ex2, "Cost"))) #Cost is $3.56
#'
#'
#' # Example 3
#'
#' ex3 = fread('
#'     ID  Last_name  First_name  Title Birth_date
#'     1 Davis Sara "Sales Rep."  12/8/1968
#'     2 Fontana Olivier "V.P. of Sales" 2/19/1952
#'     3 Leal Karina "Sales Rep." 8/30/1963
#'     4 Patten Michael "Sales Rep." 9/19/1958
#'     5 Burke Brian "Sales Mgr." 3/4/1955
#'     6 Sousa Luis "Sales Rep."  7/2/1963
#' ')
#'
#' # If there is an employee with an ID of 5, displays the employee's last name;
#' # otherwise, displays the message "Employee not found".
#' vlookup(5, ex3, "Last_name", no_match = "Employee not found") # Burke
#'
#' # Many employees
#' vlookup(1:10, ex3, "Last_name", no_match =  "Employee not found")
#'
#' # For the employee with an ID of 4, concatenates the values of three cells into
#' # a complete sentence.
#' paste0(vlookup(4, ex3, "First_name"), " ",
#'        vlookup(4, ex3, "Last_name"), " is a ",
#'        vlookup(4, ex3, "Title")) # Michael Patten is a Sales Rep.
vlookup = function(lookup_value, dict, result_column = 2, lookup_column = 1, no_match = NA){
    (!is.list(lookup_value) && NCOL(lookup_value)==1)   || stop("'vlookup': 'lookup_value' should be vector.")
    is.data.frame(dict) || stop("'vlookup': 'dict' should be data.frame.")
    length(lookup_column)==1 || stop("'vlookup': 'lookup_column' should be vector of length 1.")
    (length(no_match)==1) || stop("'vlookup': 'no_match' should be length 1.")
    if(length(result_column)==1){
        return(xlookup(lookup_value, dict[[lookup_column]], dict[[result_column]], no_match = no_match))
    }
    ind = fast_match(lookup_value, dict[[lookup_column]])
    if(is.data.table(dict)){
        res = dict[ind, result_column, with = FALSE]
    } else {
        res = dict[ind, result_column, drop = FALSE]
    }
    if(!identical(no_match, NA)) {
        ind = is.na(ind)
        res[ind, ] = no_match
    }
    res
}

#' @rdname vlookup
#' @export
xlookup = function(lookup_value, lookup_vector, result_vector, no_match = NA){
    (!is.list(lookup_value) && NCOL(lookup_value)==1)   || stop("'xlookup': 'lookup_value' should be vector.")
    (!is.list(lookup_vector) && NCOL(lookup_vector)==1) || stop("'xlookup': 'lookup_vector' should be vector.")
    (!is.list(result_vector) && NCOL(result_vector)==1) || stop("'xlookup': 'result_vector' should be vector.")
    (length(no_match)==1) || stop("'xlookup': 'no_match' should be length 1.")
    (NROW(lookup_vector) == NROW(result_vector)) || stop("'xlookup': 'lookup_vector' and 'result_vector' should be the same length.")
    ind = fast_match(lookup_value, lookup_vector)
    res = result_vector[ind]
    if(!identical(no_match, NA)) res[is.na(ind)] = no_match
    res
}

fast_match = function(x, table, nomatch = NA_integer_){
    if(is.character(x) && is.character(table)){
        return(chmatch(x, table, nomatch = nomatch))
    }
    match(x, table, nomatch = nomatch, incomparables = NULL)
}

