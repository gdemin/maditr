cat("\nContext:","coalesce", "\n")
x = c(1, NA, 2, NA)
res = coalesce(x, 0)
expect_equal( res, c(1, 0, 2, 0))
expect_error(coalesce(x, 1:2))

y = c(1, 2, NA, NA, 5)
z = c(NA, NA, 3, 4, 5)
expect_equal(coalesce(y, z), 1:5)


