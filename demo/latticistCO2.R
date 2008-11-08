dat <- CO2
datArg <- quote(CO2)

spec <- list()
updateSpec <- function(...) {
    spec <<- modifyList(spec, list(...))
    plot.call <- latticistCompose(dat, spec = spec,
                                  datArg = datArg)
    print(plot.call)
    print(eval(plot.call))
}

updateSpec()
updateSpec(yvar = "uptake")
updateSpec(xvar = "conc")
updateSpec(groups = "Treatment")
updateSpec(cond = "Type")
updateSpec(cond = "Plant", groups = NULL)

