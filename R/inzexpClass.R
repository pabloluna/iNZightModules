## A class defining a "Variable" with transformations etc

##' Default constructor for an `inzexp` object, which is a list of
##' variables with their allocated transformations.
##'
##' @title iNZight Variable Object
##' @param x the name of the variable
##' @param transform the transformation
##' @param arg optional argument to the transformation
##' @param multiple logical, if TRUE, the variable can be used multiple times with the given transformation (e.g., power)
##' @return an inzexp object
##' @author Tom Elliott
inzexp <- function(x,
                   transform =
                       c("none", "power", "log", "square root", "polynomial"),
                   arg = 2) {
    if (missing(x)) return(structure(vector("list"), class = "inzexp.list"))
    transform <- match.arg(transform)
    string <- NULL
    var <- switch(transform,
                  "none" = {
                      transform <- NA
                      arg <- NA
                      x
                  },
                  "power" = {
                      if (!is.numeric(arg) || length(arg) != 1) stop("arg must be a numeric vector of length 1 for power transformations")
                      z <- if (arg > 1) paste(x, arg, sep = "^") else x
                      if (z != x) string <- sprintf("I(%s)", z)
                      z
                  },
                  {
                      arg <- NA
                      paste0(transform, "(", x, ")")
                  })
    if (is.null(string)) string <- var
    obj <- structure(list(string = string, variable = x, transform = transform, arg = arg), class = "inzexp")
    structure(list(obj), .Names = var, class = "inzexp.list")
}

c.inzexp.list <- function(...) {
    structure(do.call(c, lapply(list(...), unclass)), class = "inzexp.list")
}


print.inzexp.list <- function(x, ...)
    print(unclass(x))

print.inzexp <- function(x, ...) {
    cat("        Variable:", x$variable, "\n")
    cat("  Transformation:", x$transform, "\n")
    if (!is.na(x$arg))
        cat("     Argument(s):", x$arg, "\n")
}


tostring <- function(x)
    paste(sapply(x, function(v) v$string), collapse = " + ")
