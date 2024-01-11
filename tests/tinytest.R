if(capabilities('long.double') && requireNamespace("tinytest", quietly=TRUE)){
    library(tinytest)
    options(covr = FALSE)
    data.table::setDTthreads(2)
    test_package("maditr", remove_side_effects=FALSE)
    data.table::setDTthreads(NULL)
}

