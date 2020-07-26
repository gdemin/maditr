cat("vlookup data.frame\n")

dict = data.frame(num=1:26,small=letters,cap=LETTERS, stringsAsFactors = FALSE)

expect_identical(vlookup(1:3,dict), dict[1:3,2])
expect_identical(xlookup(1:3,dict$num, dict$small), dict[1:3,2])
expect_identical(vlookup(1:3,dict, result_column = names(dict)), dict[1:3,])
expect_identical(vlookup(1:3,as.data.table(dict), result_column = names(dict)), as.data.table(dict[1:3,]))

dict2 = data.frame(num = c(1:26, 1:26), small = c(letters, LETTERS), stringsAsFactors = FALSE)
expect_identical(vlookup(1:3,dict2), dict[1:3,2])
expect_error(xlookup(1:3,dict2$num, dict$small))
expect_identical(xlookup(1:3,dict2$num, dict2$small), dict[1:3,2])
expect_identical(vlookup(1:3,dict2, result_column = names(dict2)), dict[1:3,1:2])
expect_identical(vlookup(1:3,as.data.table(dict2), result_column = names(dict2)), as.data.table(dict[1:3,1:2]))





expect_identical(vlookup(c(45,1:3,58,NA),dict,result_column='cap'),
                 c(NA,"A", "B", "C", NA,NA))
expect_identical(vlookup(c(45,1:3,58,NA),dict,result_column='cap', no_match = "Not found"),
                 c("Not found","A", "B", "C", "Not found","Not found"))

expect_identical(vlookup(c(45,1:3,58,NA),dict,result_column=c("small", "cap"), no_match = "Not found"),
 structure(list(small = c("Not found", "a", "b", "c", "Not found",
                          "Not found"), cap = c("Not found", "A", "B", "C", "Not found",
                                                "Not found")), row.names = c("NA", "1", "2", "3", "NA.1", "NA.2"
                                                ), class = "data.frame"))

expect_identical(vlookup(c('z','d','f','d'),dict,lookup_column = 'small', result_column = names(dict)),
                 dict[c(26,4,6,4),])


expect_error(vlookup(iris, mtcars))


cat("vlookup vector\n")
# with vector
dict=seq_along(letters)
names(dict) = letters

expect_identical(xlookup(c(6, 4, 2), dict, names(dict)), c("f","d","b"))
expect_identical(xlookup(c("f","d","b"), names(dict), dict), dict[c(6L, 4L, 2L)])


expect_identical(
    vlookup(c(1, NA, 2), dict = data.frame(a = c(2, NA), b= c(3, 2))),
    c(NA, 2, 3)
)
expect_identical(
    vlookup(c(1, NA, 2), dict = data.table(a = c(2, NA), b= c(3, 2))),
    c(NA, 2, 3)
)

cat("vlookup excel examples ex2\n")

# Just for fun. Examples borrowed from Microsoft Excel.
# It is not the R way of doing things.

# Example 2

ex2 = fread("
    Item_ID Item Cost Markup
    ST-340 Stroller 145.67  0.30
    BI-567 Bib 3.56  0.40
    DI-328 Diapers  21.45  0.35
    WI-989 Wipes  5.12  0.40
    AS-469 Aspirator 2.56  0.45
")

# Calculates the retail price of diapers by adding the markup percentage to the cost.
expect_identical(vlookup("DI-328", ex2, 3) * (1 + vlookup("DI-328", ex2, 4)) ,28.9575)

# Calculates the sale price of wipes by subtracting a specified discount from
# the retail price.
expect_identical((vlookup("WI-989", ex2, "Cost") * (1 + vlookup("WI-989", ex2, "Markup"))) * (1 - 0.2), 5.7344)

A2 = ex2[[1]][1]
A3 = ex2[[1]][2]

# If the cost of an item is greater than or equal to $20.00, displays the string
# "Markup is nn%"; otherwise, displays the string "Cost is under $20.00".
expect_identical(ifelse(vlookup(A2, ex2, "Cost") >= 20,
       paste0("Markup is " , 100 * vlookup(A2, ex2, "Markup"),"%"),
       "Cost is under $20.00"), 'Markup is 30%')


# If the cost of an item is greater than or equal to $20.00, displays the string
# Markup is nn%"; otherwise, displays the string "Cost is $n.nn".
expect_identical(ifelse(vlookup(A3, ex2, "Cost") >= 20,
       paste0("Markup is: " , 100 * vlookup(A3, ex2, "Markup") , "%"),
       paste0("Cost is $", vlookup(A3, ex2, "Cost"))), 'Cost is $3.56')


# Example 3
cat("vlookup excel examples ex3\n")

ex3 = fread('
    ID  Last_name  First_name  Title Birth_date
    1 Davis Sara "Sales Rep."  12/8/1968
    2 Fontana Olivier "V.P. of Sales" 2/19/1952
    3 Leal Karina "Sales Rep." 8/30/1963
    4 Patten Michael "Sales Rep." 9/19/1958
    5 Burke Brian "Sales Mgr." 3/4/1955
    6 Sousa Luis "Sales Rep."  7/2/1963
')

# If there is an employee with an ID of 5, displays the employee's last name;
# otherwise, displays the message "Employee not found".
expect_identical(ifelse(is.na(vlookup(5,ex3,"Last_name")),
       "Employee not found",
       vlookup(5,ex3,"Last_name")), 'Burke')
expect_identical(ifelse(is.na(vlookup(15,ex3,"Last_name")),
       "Employee not found",
       vlookup(15,ex3,"Last_name")), 'Employee not found')

# For the employee with an ID of 4, concatenates the values of three cells into
# a complete sentence.
expect_identical(paste0(vlookup(4,ex3,"First_name"), " ",
       vlookup(4,ex3,"Last_name"), " is a ",
       vlookup(4,ex3,"Title")), 'Michael Patten is a Sales Rep.')

