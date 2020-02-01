if(capabilities('long.double') && requireNamespace("tinytest", quietly=TRUE)){
    library(tinytest)
    options(covr = TRUE)
    test_package("maditr")
}
