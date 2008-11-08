library(MASS)
dat <- Aids2
datArg <- quote(Aids2)

spec <- list()
updateSpec <- function(...) {
    spec <<- modifyList(spec, list(...))
    plot.call <- latticistCompose(dat, spec = spec,
                                  datArg = datArg)
    print(plot.call)
    print(eval(plot.call))
}

updateSpec()
updateSpec(xvar = "age", yvar = "status")
updateSpec(yvar = "age", xvar = "diag",
           zvar = "death", doSegments = TRUE)
updateSpec(xvar = "age", yvar = "T.categ",
           zvar = NULL)
updateSpec(xvar = "(death - diag)", yvar = "age",
           doHexbin = TRUE)
updateSpec(cond = "sex")
