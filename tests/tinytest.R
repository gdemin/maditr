if(capabilities('long.double') && requireNamespace("tinytest", quietly=TRUE)){
    library(tinytest)
    options(covr = FALSE)
    test_package("maditr")
}
