context("dt_join_*")
workers = fread("
    name company
                Nick Acme
                John Ajax
                Daniela Ajax
                ", stringsAsFactors=FALSE, data.table = FALSE)
positions = fread("
                  name position
                  John designer
                  Daniela engineer
                  Cathie manager
                  ", stringsAsFactors=FALSE, data.table = FALSE)

data("iris")
data("mtcars")
expect_error(dt_left_join(mtcars, iris))
expect_error(dt_left_join(workers, positions, by = "xxx"))
expect_error(dt_anti_join(mtcars, iris))
expect_error(dt_anti_join(workers, positions, by = "xxx"))

res = workers %>% dt_inner_join(positions)
etal = structure(list(name = c("John", "Daniela"), company = c("Ajax",
"Ajax"), position = c("designer", "engineer")), class = c("data.table",
"data.frame"), row.names = c(NA, -2L))
expect_equal(res, etal)

res = workers %>% dt_left_join(positions)
etal = structure(list(name = c("Nick", "John", "Daniela"), company = c("Acme",
"Ajax", "Ajax"), position = c(NA, "designer", "engineer")), class = c("data.table",
"data.frame"), row.names = c(NA, -3L))
expect_equal(res, etal)

res = workers %>% dt_right_join(positions)
etal = structure(list(name = c("John", "Daniela", "Cathie"), company = c("Ajax",
"Ajax", NA), position = c("designer", "engineer", "manager")), row.names = c(NA,
-3L), class = c("data.table", "data.frame"))
expect_equal(res, etal)

res = workers %>% dt_full_join(positions)
etal = structure(list(name = c("Nick", "John", "Daniela", "Cathie"),
company = c("Acme", "Ajax", "Ajax", NA), position = c(NA,
"designer", "engineer", "manager")), row.names = c(NA, -4L
), class = c("data.table", "data.frame"))
expect_equal(res, etal)

res = workers %>% dt_anti_join(positions)
etal = structure(list(name = "Nick", company = "Acme"), class = c("data.table",
"data.frame"), row.names = c(NA, -1L))
expect_equal(res, etal)

res = workers %>% dt_semi_join(positions)
etal = structure(list(name = c("John", "Daniela"), company = c("Ajax",
"Ajax")), class = c("data.table", "data.frame"), row.names = c(NA,
-2L))
expect_equal(res, etal)
# To suppress the message, supply 'by' argument
res = workers %>% dt_left_join(positions, by = "name")
etal = structure(list(name = c("Nick", "John", "Daniela"), company = c("Acme",
"Ajax", "Ajax"), position = c(NA, "designer", "engineer")), class = c("data.table",
"data.frame"), row.names = c(NA, -3L))
expect_equal(res, etal)
# Use a named 'by' if the join variables have different names
positions2 = setNames(positions, c("worker", "position")) # rename first column in 'positions'
res = workers %>% dt_inner_join(positions2, by = c("name" = "worker"))
etal = structure(list(name = c("John", "Daniela"), company = c("Ajax",
"Ajax"), position = c("designer", "engineer")), class = c("data.table",
"data.frame"), row.names = c(NA, -2L))
expect_equal(res, etal)


res = workers %>% dt_anti_join(positions2, by = c("name" = "worker"))
etal = structure(list(name = "Nick", company = "Acme"), class = c("data.table",
"data.frame"), row.names = c(NA, -1L))
expect_equal(res, etal)

res = workers %>% dt_semi_join(positions2, by = c("name" = "worker"))
etal = structure(list(name = c("John", "Daniela"), company = c("Ajax",
"Ajax")), class = c("data.table", "data.frame"), row.names = c(NA,
-2L))
expect_equal(res, etal)


positions3 = positions[c(1,1,1:3), ]
workers2 = workers[c(1, 2,2,2, 3),]
res = workers2 %>% dt_left_join(positions3, by = "name")
etal = structure(list(name = c("Nick", "John", "John", "John", "John",
"John", "John", "John", "John", "John", "Daniela"), company = c("Acme",
"Ajax", "Ajax", "Ajax", "Ajax", "Ajax", "Ajax", "Ajax", "Ajax",
"Ajax", "Ajax"), position = c(NA, "designer", "designer", "designer",
"designer", "designer", "designer", "designer", "designer", "designer",
"engineer")), class = c("data.table", "data.frame"), row.names = c(NA,
-11L))
expect_equal(res, etal)

res = workers2 %>% dt_semi_join(positions3, by = "name")
etal = structure(list(name = c("John", "John", "John", "Daniela"), company = c("Ajax",
"Ajax", "Ajax", "Ajax")), class = c("data.table", "data.frame"
), row.names = c(NA, -4L))
expect_equal(res, etal)

d1 = data.table(a=rep(1:2,each=3), b=1:6, key="a,b")
d2 = data.table(a=0:1, b=0:1, key="a,b")


res = dt_inner_join(d1, d2, by="a")
etal = structure(list(a = c(1L, 1L, 1L), b.x = 1:3, b.y = c(1L, 1L,
1L)), class = c("data.table", "data.frame"), row.names = c(NA,
-3L))
expect_equal(res, etal)
res = dt_inner_join(d1, d2, by="a", suffix=c(".d1", ".d2"))
etal = structure(list(a = c(1L, 1L, 1L), b.d1 = 1:3, b.d2 = c(1L, 1L,
1L)), class = c("data.table", "data.frame"), row.names = c(NA,
-3L))
expect_equal(res, etal)

d1 = data.table(a=rep(1:2,each=3), b=1:6, e = 1)
d2 = data.table(a=0:1, b=0:1, key="a,b", d = 3)

res = dt_left_join(d1, d2, by = c("a", "b"))
etal = structure(list(a = c(1L, 1L, 1L, 2L, 2L, 2L), b = 1:6, e = c(1,
1, 1, 1, 1, 1), d = c(3, NA, NA, NA, NA, NA)), class = c("data.table",
"data.frame"), row.names = c(NA, -6L))
expect_equal(res, etal)

d3 = setnames(d2, c("a", "v", "d"))

res = dt_full_join(d1, d3, by = c("a", "b" = "v"))
etal = structure(list(a = c(1L, 1L, 1L, 2L, 2L, 2L, 0L), b = c(1L, 2L,
3L, 4L, 5L, 6L, 0L), e = c(1, 1, 1, 1, 1, 1, NA), d = c(3, NA,
NA, NA, NA, NA, 3)), row.names = c(NA, -7L), class = c("data.table",
"data.frame"))
expect_equal(res, etal)

res = dt_anti_join(d1, d3, by = c("a", "b" = "v"))
etal =structure(list(a = c(1L, 1L, 2L, 2L, 2L), b = 2:6, e = c(1, 1,
1, 1, 1)), class = c("data.table", "data.frame"), row.names = c(NA,
-5L))
expect_equal(res, etal)
res = dt_semi_join(d1, d3, by = c("a", "b" = "v"))
etal =structure(list(a = 1L, b = 1L, e = 1), class = c("data.table",
"data.frame"), row.names = c(NA, -1L))
expect_equal(res, etal)

