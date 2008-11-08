dat <- Employment
datArg <- quote(Employment)

spec <- list()
updateSpec <- function(...) {
    spec <<- modifyList(spec, list(...))
    plot.call <- latticistCompose(dat, spec = spec,
                                  datArg = datArg)
    print(plot.call)
    print(eval(plot.call))
}

updateSpec()
updateSpec(defaultPlot = "parallel") ## actually barchart!
updateSpec(groups = "EmploymentStatus")
updateSpec(xvar = "EmploymentStatus",
           yvar = "EmploymentLength", groups = NULL)
updateSpec(cond = "LayoffCause",
           doSeparateStrata = FALSE)
updateSpec(doSeparateStrata = TRUE)
