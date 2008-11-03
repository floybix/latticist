## latticist: a Lattice-based exploratory visualisation GUI
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

latticistChooseVars <-
    function(dat, spec = list(), handler)
{
    ## applies to hypervariate plots:
    ## splom, parallel and marginal.plot
    if (is.table(dat)) {
        ## can't handle a var subset with table (yet)
        return(handler(spec$varSubset))
    }
    checked <- spec$varSubset
    vars <- names(dat)
    if (!is.null(checked))
        checked <- vars %in% checked
    if (is.null(checked))
        checked <- TRUE
    ## (would do this with a gbasicdialog() but it is
    ##  not available in gWidgetstcltk)
    dialog <- gwindow(title = "Choose variables to plot")
    theW <- ggroup(horizontal = FALSE, container = dialog)
    tmpg <- ggroup(horizontal = TRUE, container = theW)
    varsW <- gcheckboxgroup(vars, checked = checked, container = theW)
    gbutton("All", container = tmpg,
            handler = function(h, ...) svalue(varsW) <- vars )
    gbutton("None", container = tmpg,
            handler = function(h, ...) svalue(varsW) <- NULL )
    glabel(paste("NOTE: too many variables will result in a very slow",
                 "\nplot, especially so for splom (scatter plot matrix)."),
           container = theW)

    sepW <- gseparator(container = theW, expand = TRUE)
    try(size(sepW) <- c(220, 10), silent = TRUE)
    tmpg <- ggroup(horizontal = TRUE, container = theW)
    addSpace(tmpg, 10)
    canW <- gbutton("Cancel", container = tmpg,
            handler = function(...) {
                dispose(dialog)
            })
    addSpace(tmpg, 10)
    okW <- gbutton("OK",
                   container = tmpg,
            handler = function(...) {
                varsub <- svalue(varsW)
                if (identical(varsub, vars))
                    varsub <- NULL
                dispose(dialog)
                handler(varsub)
            })
    defaultWidget(okW) <- TRUE
    try({
        size(canW) <- c(80, 25)
        size(okW) <- c(80, 25)
    }, silent = TRUE)
    invisible()
}
