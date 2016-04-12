
as.double.SilkyFactor <- function(object) {
    as.integer.SilkyFactor(object)
}

as.integer.SilkyFactor <- function(object) {
    values <- attr(object, "values", TRUE)
    class(object) <- "factor"
    return(values[as.integer(object)])
}

canBeNumeric <- function(object) {
    is.numeric(object) || ! is.null(attr(object, "values", TRUE))
}

