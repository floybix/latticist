## latticist: a Lattice-based exploratory visualisation GUI
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

.defaultLatticistOptions <- function()
    list(
         use.playwith = TRUE,
         defaultPlot = "marginal.plot",
         xyLineType = "smooth",
         add.sub = TRUE,
         sub.func = defaultSubFn,
         MANY = 2000,
         VERYMANY = 8000,
         style.MANY =
           list(pch = 4, alpha.points = 0.6),
         style.VERYMANY =
           list(pch = ".", cex = 3, alpha.points = 0.3),
         style.3panels = list(cex = 0.6),
         style.7panels = list(cex = 0.5),
         disc.levels = 4,
         shingle.overlap = 0.5,
         max.panels = 16,
         catch.errors = TRUE
         )

defaultSubFn <- function(spec, nPoints)
{
    Rvers <- paste("R ", R.version$major, ".",
                   R.version$minor, R.version$status, sep="")
    subt <- if (nPoints > 0)
        paste("N = ", nPoints, ", ", sep="") else ""
    subt <- paste(subt, toString(Sys.Date()), ", ",
                  Rvers, sep="")
    subset <- spec$subset
    if (!is.null(subset)) {
        if (nchar(subset) > 30)
            subt <- call("paste", subset, subt, sep="\n")
        else subt <- call("paste", subset, subt, sep=", ")
    }
    subt
}

## code below copied from lattice

latticist.getOption <- function(name)
{
    .LatticistEnv$options[[name]]
}

latticist.options <- function(...)
{
    ## this would have been really simple if only form allowed were
    ## lattice.options("foo", "bar") and
    ## lattice.options(foo=1, bar=2). But it could also be
    ## lattice.options(foo=1, "bar"), which makes some juggling necessary

    new <- list(...)
    if (is.null(names(new)) && length(new) == 1 && is.list(new[[1]])) new <- new[[1]]
    old <- .LatticistEnv$options

    ## if no args supplied, returns full options list
    if (length(new) == 0) return(old)

    nm <- names(new)
    if (is.null(nm)) return(old[unlist(new)]) ## typically getting options, not setting
    isNamed <- nm != "" ## typically all named when setting, but could have mix
    if (any(!isNamed)) nm[!isNamed] <- unlist(new[!isNamed])

    ## so now everything has non-"" names, but only the isNamed ones should be set
    ## everything should be returned, however

    retVal <- old[nm]
    names(retVal) <- nm
    nm <- nm[isNamed]

    ## this used to be

    ## modified <- updateList(retVal[nm], new[nm])
    ## .LatticeEnv$lattice.options[names(modified)] <- modified

    ## but then calling lattice.options(foo = NULL) had no effect
    ## because foo would be missing from modified.  So, we now do:

    updateList <- function (x, val) {
        if (is.null(x)) x <- list()
        modifyList(x, val)
    }
    .LatticistEnv$options <- updateList(old, new[nm])

    ## return changed entries invisibly
    invisible(retVal)
}
