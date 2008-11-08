library(DAAG)
dat <- frogs
datArg <- quote(frogs)
dat$pres.abs <- factor(dat$pres.abs)

spec <- list()
updateSpec <- function(...) {
    spec <<- modifyList(spec, list(...))
    plot.call <- latticistCompose(dat, spec = spec,
                                  datArg = datArg)
    print(plot.call)
    print(eval(plot.call))
}

updateSpec()
updateSpec(varSubset = c("altitude", "distance",  "NoOfPools",
           "NoOfSites", "avrain", "meanmin", "meanmax"))
updateSpec(groups = "pres.abs")
updateSpec(defaultPlot = "splom")
updateSpec(xvar = "easting", yvar = "northing")
updateSpec(doLines = FALSE)
updateSpec(zvar = "altitude")
## + rotate, identify...
