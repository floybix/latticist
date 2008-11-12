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
spec <- list()
updateSpec(xvar = "T.categ")
updateSpec(groups = "status")
updateSpec(cond = "T.categ", x.relation = "free",
           y.relation = "free")

spec <- list()
updateSpec(xvar = "status", yvar = "T.categ")

spec <- list(
updateSpec(xvar = "age", yvar = "T.categ")
updateSpec(groups = "status")

spec <- list()
updateSpec(yvar = "jitter(age)", xvar = "diag",
           zvar = "death", doSegments = TRUE)
updateSpec(groups = "diag")
updateSpec(cond = "state")

spec <- list(
updateSpec(xvar = "age", yvar = "(death - diag)",
           subset = "status == 'D'")
updateSpec(doHexbin = TRUE)
updateSpec(cond = "state")
