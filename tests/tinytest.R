if(capabilities('long.double') && requireNamespace("tinytest", quietly=TRUE)){
    library(tinytest)
    test_package("maditr")
}
