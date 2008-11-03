## latticist: a Lattice-based exploratory visualisation GUI
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

latticist_gWidgets <-
    function(dat,
             spec = list(),
             datArg = substitute(dat),
             datName = "dat",
             ...,
             width = 480,
             height = 480,
             pointsize = 12)
{
    title <- paste("Latticist:", datName)

    ## this list will store state
    lattState <- latticistInitOptions(dat, datArg = datArg)
    lattState$spec <- spec
    varexprs <- lattState$varexprs

    ## CUSTOM WIDGETS
    gdroplist <- function(..., width = NA) {
        foo <- gWidgets::gdroplist(...)
        if (!is.na(width))
            size(foo) <- c(width, -1)
        foo
    }
    ggroupThin <- function(..., spacing = 1) {
        foo <- gWidgets::ggroup(..., spacing = spacing)
        ## remove outer border
        if (!inherits(guiToolkit(), "guiWidgetsToolkittcltk")) {
            ## tcltk fails here (gWidgetstcltk 0.0-15)
            svalue(foo) <- 0
        }
        foo
    }

    ## CREATE THE GUI
    win <- gwindow(title = title)
    metagroup <- ggroup(horizontal = FALSE, container = win, expand = FALSE)
    hgroup <- ggroupThin(horizontal = TRUE, container = metagroup, expand = TRUE)
    vgroup <- ggroupThin(horizontal = FALSE, container = hgroup, expand = FALSE)
    ## add the graphics device
    gg <- ggraphics(width = width, height = height, ps = pointsize,
                    container = hgroup, expand = TRUE)
    trellis.device(new = FALSE, retain = TRUE) ## i.e. new device if needed
    ## persistent variables
    targetDev <- dev.cur()
    plot.call <- quote(plot())

    reCompose <- function() {
        dev.set(targetDev)
        if (isTRUE(latticist.getOption("catch.errors"))) {
            tmp <- tryCatch(latticistCompose(dat, lattState$spec,
                                             datArg = datArg),
                            error = force)
            if (inherits(tmp, "error")) {
                gmessage(conditionMessage(tmp),
                         title = "Error", icon = "error")
                stop(tmp)
            }
            plot.call <<- tmp
        } else {
            plot.call <<-
                latticistCompose(dat, lattState$spec,
                                 datArg = datArg)
        }
        callTxt <- deparseOneLine(plot.call, control = NULL)
        ## check whether anything changed
        if (identical(callTxt, svalue(wid.call)))
            return()
        svalue(wid.call) <- callTxt
        eval(call("print", plot.call))
        updateFromSpec()
    }

    updateFromSpec <- function() {
        spec <- lattState$spec
        svalue(wid.xvar) <- toString(spec$xvar)
        svalue(wid.yvar) <- toString(spec$yvar)
        svalue(wid.zvar) <- toString(spec$zvar)
        svalue(wid.groups) <- toString(spec$groups)
        svalue(wid.cond) <- toString(spec$cond)
        svalue(wid.cond2) <- toString(spec$cond2)
        svalue(wid.subset) <- toString(spec$subset)
        svalue(wid.aspect) <- toString(spec$aspect)
        ## construct "scales" value from x/y.relation
        scalesVal <- ""
        if (!is.null(spec$x.relation) &&
            !is.null(spec$y.relation))
        {
            scalesVal <- paste("x ", spec$x.relation, ", ",
                               "y ", spec$y.relation, sep="")
        }
        svalue(wid.scales) <- scalesVal
        svalue(wid.tile) <- isTRUE(spec$doTile)
        svalue(wid.lines) <-
            !identical(spec$doLines, FALSE)
        svalue(wid.separate.strata) <-
            !identical(spec$doSeparateStrata, FALSE)
        ## set which widgets are sensitive
        enabled(wid.flip) <-
            (!is.null(spec$xvar) || !is.null(spec$yvar))
        enabled(wid.zvar) <-
            (!is.null(spec$xvar) && !is.null(spec$yvar))
        enabled(wid.cond2) <-
            (!is.null(spec$cond) || !is.null(spec$cond2))
        enabled(wid.explode) <- (!is.null(spec$groups))
        enabled(wid.superpose) <- (!is.null(spec$cond))
        enabled(wid.tile) <-
            (!is.null(spec$groups) &&
             !is.null(spec$xvar) &&
             !is.null(spec$yvar) &&
             is.null(spec$zvar))
        isVCD <- (is.call.to(plot.call, "mosaic") ||
                  is.call.to(plot.call, "cotabplot") ||
                  is.call.to(plot.call, "pairs"))
        hasPanels <- if (!isVCD)
            (prod(dim(trellis.last.object())) > 1)
        enabled(wid.scales) <- !isVCD && hasPanels
        enabled(wid.separate.strata) <- isVCD
        enabled(wid.identify) <-
            (!is.table(dat) && !isVCD &&
             !is.call.to(plot.call, "marginal.plot") &&
             !is.call.to(plot.call, "parallel"))
    }

    setSpec <- function(h, ...) {
        target <- h$action
        val <- svalue(h$obj)
        if (identical(val, ""))
            val <- NULL
        lattState$spec[[target]] <<- val
        reCompose()
    }

    ## (Title)
    tmpg <- ggroupThin(container = vgroup)
    gimage(system.file("etc", "latticist_title.gif", package="latticist"),
           container = tmpg)
    glabel(paste(" v.", packageDescription("playwith")$Version),
           container = tmpg)

    ## HYPERVARIATE
    hyperg <- gframe("Hypervariate", horizontal = FALSE, expand = FALSE, container = vgroup)
    tmpg <- ggroupThin(container = hyperg)
    goHyper <- function(h, ...) {
        ## ask user to choose variables, or cancel
        latticistChooseVars(dat, spec = lattState$spec,
                            handler = function(value)
                        {
                            lattState$spec$varSubset <<- value
                            lattState$spec$xvar <<- NULL
                            lattState$spec$yvar <<- NULL
                            lattState$spec$zvar <<- NULL
                            lattState$spec$defaultPlot <<- h$action
                            reCompose()
                        })
    }
    gbutton("marginals", container = tmpg,
            handler = goHyper, action = "marginal.plot")
    gbutton("splom (pairs)", container = tmpg,
            handler = goHyper, action = "splom")
    parallelTxt <- "parallel"
    if (is.table(dat)) parallelTxt <- "table"
    gbutton(parallelTxt, container = tmpg,
            handler = goHyper, action = "parallel")

    ## VARIABLES ON AXES
    xyg <- gframe("Variables on axes", horizontal = FALSE, container = vgroup)
    tmpg <- ggroupThin(container = xyg)
    ## "switch" button
    wid.flip <-
        gbutton("switch x/y", container = tmpg,
                handler = function(...) {
                    xvar <- lattState$spec$xvar
                    yvar <- lattState$spec$yvar
                    lattState$spec$xvar <<- yvar
                    lattState$spec$yvar <<- xvar
                    reCompose()
                })
    ## ASPECT
    glabel(" Aspect:", container = tmpg)
    aspectopts <- lattState$aspectopts
    wid.aspect <-
        gdroplist(aspectopts, selected = 0, container = tmpg,
                  editable = TRUE, width = 70,
                  coerce.with = function(x) {
                      if (nchar(x) > 0)
                          parse(text=x)[[1]]
                  }, handler = setSpec, action = "aspect")

    ## TODO: hexbin
    tmpg <- ggroupThin(container = xyg)
    glabel("x=", container = tmpg)
    wid.xvar <-
        gdroplist(varexprs, container = tmpg, editable = TRUE,
                  handler = setSpec, action = "xvar")
    tmpg <- ggroupThin(container = xyg)
    glabel("y=", container = tmpg)
    wid.yvar <-
        gdroplist(varexprs, container = tmpg, editable = TRUE,
                  handler = setSpec, action = "yvar")
    tmpg <- ggroupThin(container = xyg)
    glabel("z=", container = tmpg)
    wid.zvar <-
        gdroplist(varexprs, container = tmpg, editable = TRUE,
                  handler = setSpec, action = "zvar")
    ## TODO: disc? / levels

    ## GROUPS / COLOR
    grpg <- gframe("Groups / Color", horizontal = FALSE, container = vgroup)
    tmpg <- ggroupThin(container = grpg)
    wid.groups <-
        gdroplist(varexprs, container = tmpg, editable = TRUE,
                  handler = setSpec, action = "groups")
    ## "explode" button
    tmpg <- ggroupThin(container = grpg)
    wid.explode <-
        gbutton("explode", container = tmpg,
                handler = function(...) {
                    groups <- lattState$spec$groups
                    if (is.null(lattState$spec$cond)) {
                        lattState$spec$cond <<- groups
                    } else {
                        lattState$spec$cond2 <<- groups
                    }
                    lattState$spec$groups <<- NULL
                    reCompose()
                })
    ## "superpose" button
    wid.superpose <-
        gbutton("superpose", container = tmpg,
                handler = function(...) {
                    conds <- lattState$spec$cond
                    if (!is.null(lattState$spec$cond2)) {
                        conds <- sprintf("paste(%s, %s)", conds,
                                         lattState$spec$cond2)
                    }
                    if (!is.null(lattState$spec$groups)) {
                        conds <- sprintf("paste(%s, %s)", conds,
                                         lattState$spec$groups)
                    }
                    lattState$spec$groups <<- conds
                    lattState$spec$cond <<- NULL
                    lattState$spec$cond2 <<- NULL
                    reCompose()
                })
    ## "Tile" checkbox
    wid.tile <-
        gcheckbox("Tile", container = tmpg,
                  handler = setSpec, action = "doTile")

    ## CONDITIONING
    condg <- gframe("Conditioning", horizontal = FALSE, container = vgroup)
    tmpg <- ggroupThin(container = condg)
    wid.cond <-
        gdroplist(varexprs, container = tmpg, editable = TRUE,
                  handler = setSpec, action = "cond")
    tmpg <- ggroupThin(container = condg)
    wid.cond2 <-
        gdroplist(varexprs, container = tmpg, editable = TRUE,
                  handler = setSpec, action = "cond2")
    ## SCALES
    tmpg <- ggroupThin(container = condg)
    glabel("Scales:", container = tmpg)
    scalesopts <- lattState$scalesopts
    setScales <- function(h, ...) {
        val <- svalue(wid.scales)
        if (is.null(val) || (nchar(val) == 0)) {
            x.relation <- y.relation <- NULL
        } else {
            tmp <- strsplit(val, ", ")[[1]]
            x.relation <- substring(tmp[1], first=3)
            y.relation <- substring(tmp[2], first=3)
        }
        lattState$spec$x.relation <<- x.relation
        lattState$spec$y.relation <<- y.relation
        reCompose()
    }
    wid.scales <-
        gdroplist(scalesopts, selected = 0, container = tmpg,
                  editable = FALSE, width = 70,
                  handler = setScales)
    ## "Separate strata" checkbox
    wid.separate.strata <-
        gcheckbox("Sep. strata", container = tmpg,
                  handler = setSpec, action = "doSeparateStrata")

    ## SUBSET
    subsetg <- gframe("Subset", horizontal = FALSE, container = vgroup)
    tmpg <- ggroupThin(container = subsetg)
    subsetopts <- lattState$subsetopts
    wid.subset <-
        gdroplist(subsetopts, container = tmpg, editable = TRUE,
                  handler = setSpec, action = "subset")

    ## STYLE
    styleg <- gframe("Style", horizontal = FALSE, container = vgroup)
    tmpg <- ggroupThin(container = styleg)
    ## "Lines" checkbox
    wid.lines <-
        gcheckbox("Lines", container = tmpg,
                  checked = !identical(spec$doLines, FALSE),
                  handler = setSpec, action = "doLines")
    wid.style <-
        gbutton("Style Settings...", container = tmpg,
                handler = function(...) {
                    latticeStyleGUI(target.device = targetDev,
                                    width = width, height = height,
                                    pointsize = pointsize)
                })

    ## CALL
    callg <- gframe("Plot call", horizontal = FALSE, container = vgroup)
    wid.call <-
        gtext("", width = 160, height = 80, container = callg,
              font.attr = c(family = "monospace"))
    tmpg <- ggroupThin(container = callg)
    wid.redraw <-
        gbutton("Redraw", container = tmpg,
                handler = function(...) {
                    dev.set(targetDev)
                    plot.call <- parse(text = svalue(wid.call))[[1]]
                    eval(call("print", plot.call))
                })
    wid.identify <-
        gbutton("Identify", container = tmpg,
                handler = function(...) {
                    enabled(vgroup) <- FALSE
                    on.exit(enabled(vgroup) <- TRUE)
                    dev.set(targetDev)
                    isMultiPanel <-
                        (length(trellis.currentLayout()) > 1)
                    trellis.focus(highlight = isMultiPanel)
                    on.exit(trellis.unfocus(), add = TRUE)
                    labels <- rownames(dat)
                    if (is.call.to(plot.call, "splom")) {
                        panel.link.splom()
                    } else if (is.call.to(plot.call, "qqmath")) {
                        panel.identify.qqmath(labels = labels)
                    } else if (is.call.to(plot.call, "cloud")) {
                        panel.identify.cloud(labels = labels)
                    } else {
                        panel.identify(labels = labels)
                    }
                })

    reCompose()

    return(invisible(win))
}

deparseOneLine <-
    function(expr, width.cutoff = 500, ...)
{
    tmp <- deparse(expr, width.cutoff = width.cutoff, ...)
    indents <- attr(regexpr("^ *", tmp), "match.length")
    breaks <- c(diff(indents) <= 0, FALSE)
    tmp <- gsub("^ +", "", tmp)
    tmp <- gsub(" +$", "", tmp)
    breaks[c(tmp[-1]=="{", FALSE)] <- F
    tmp <- paste(tmp, ifelse(breaks, ";", ""), sep="", collapse=" ")
    tmp <- gsub("\\{;", "\\{", tmp)
    tmp <- gsub(";\\}", " \\}", tmp)
    tmp <- gsub(";\\{", " \\{", tmp)
    ## update: need this for long inline vectors:
    tmp <- gsub(";,", ",", tmp)
    tmp <- gsub(",;", ",", tmp)
    tmp
}
