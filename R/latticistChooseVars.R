## latticist: a Lattice-based exploratory visualisation GUI
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

latticistChooseVars <- function(dat, spec = list())
{
    ## applies to hypervariate plots:
    ## splom, parallel and marginal.plot
    if (is.table(dat)) return(list(ok = TRUE))
    vars <- names(dat)
    checked <- spec$varSubset
    if (!is.null(checked))
        checked <- vars %in% checked
    if (is.null(checked))
        checked <- TRUE
    theW <- ggroup(horizontal = FALSE)
    tmpg <- ggroup(horizontal = TRUE, container = theW)
    varsW <- gcheckboxgroup(vars, checked = checked, container = theW)
    gbutton("All", container = tmpg,
            handler = function(h, ...) svalue(varsW) <- vars )
    gbutton("None", container = tmpg,
            handler = function(h, ...) svalue(varsW) <- NULL )
    glabel(paste("NOTE: too many variables will result in a very slow",
                 "\nplot, especially so for splom (scatter plot matrix)."),
           container = theW)
    result <- list()
    foo <- gbasicdialog(title = "Choose variables to plot",
                 widget = theW,
                 handler = function(h, ...) {
                     varsub <- svalue(varsW)
                     if (identical(varsub, vars))
                         varsub <- NULL
                     result$varSubset <<- varsub
                     dispose(h$obj)
                 })
    result$ok <- isTRUE(foo)
    result
}
