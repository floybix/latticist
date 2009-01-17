## latticist: a Lattice-based exploratory visualisation GUI
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

latticist_playwith <-
    function(dat,
             spec = list(),
             datArg = substitute(dat),
             datName = "dat",
             ...,
             labels = rownames(dat),
             time.mode = FALSE,
             height = playwith.getOption("height") - 1,
             plot.call)
{
    stopifnot(require("playwith"))
    ## I really do not want to do this
    ## (since RGtk2 exports 7000+ symbols and
    ##  is irrelevant to the user);
    ## but it is necessary to pass R CMD check
    require(RGtk2)

    title <- paste("Latticist:", datName)

    plot.call <-
        latticistCompose(dat, spec = spec, datArg = datArg)

    ## this is the constructor (an init.action)
    lattAction <- latticistToolConstructor(dat, datArg = datArg)
    ## this list will store state in playState$latticist
    lattState <- latticistInitOptions(dat, datArg = datArg)
    lattState$spec <- spec
    eval.args <- list(NA, envir = parent.frame())
                                        #if (!is.list(eval.args))
                                        #    eval.args <- as.list(eval.args)
                                        #eval.args$envir <- parent.frame()

    playwith(plot.call = plot.call,
             title = title, ...,
             eval.args = eval.args,
             height = height,
             labels = labels,
             time.mode = time.mode,
             init.actions = list(latticist = lattAction),
             latticist = lattState)
}


latticistToolConstructor <- function(dat, datArg)
{
    ## this is the init.action
    function(playState)
    {
        if (!is.null(playState$widgets$latticist)) {
            ## GUI has already been built, nothing to do.
            return(NA)
        }

        nowUpdating <- FALSE
        ## this function is triggered by user changes to widgets
        reCompose <- function(playState, newPlot = FALSE)
        {
            ## lock, to avoid recursive calls as widgets are updated
            if (isTRUE(nowUpdating)) return(FALSE)
            nowUpdating <<- TRUE
            on.exit(nowUpdating <<- FALSE)

            spec <- playState$latticist$spec
            if (newPlot) {
                ## ignore old aspect and scales for new plot types
                spec$aspect <- NULL
                spec$x.relation <- NULL
                spec$y.relation <- NULL
            }
#            if (isTRUE(spec$doXDisc) && isTRUE(spec$doYDisc) &&
#                require("hexbin")) {
#                ## if both are discretized numerics, use 2D binning
#                spec$doXDisc <- FALSE
#                spec$doYDisc <- FALSE
#                spec$doHexbin <- TRUE
#            }
            ## compose plot call from latticist specification
            oldCall <- playState$call
            playState$call <-
                latticistCompose(dat, spec, datArg = datArg,
                                 enclos = playState$env)
            ## check whether anything changed
            updateMainCall(playState) ## set canonical arguments
            if (identical(deparse(playState$call, control = NULL),
                          deparse(oldCall, control = NULL)))
                return(FALSE)
            ## clear device to avoid redraws when reconfiguring interface
            playDevSet(playState)
            grid::grid.newpage()
            updateFromSpec()
            playNewPlot(playState)
            return(FALSE)
        }

        updateFromSpec <- function() {
            spec <- playState$latticist$spec
            xvar <- spec$xvar
            yvar <- spec$yvar
            zvar <- spec$zvar
            groups <- spec$groups
            cond <- spec$cond
            cond2 <- spec$cond2
            subset <- spec$subset
            doXDisc <- isTRUE(spec$doXDisc)
            doYDisc <- isTRUE(spec$doYDisc)

            ## evaluate variables to determine data types
            if (is.table(dat)) {
                xIsCat <- !is.null(xvar)
                yIsCat <- !is.null(yvar)
                zIsCat <- !is.null(zvar)
                groupsIsCat <- !is.null(groups)
            } else {
                xvarExpr <- if (!is.null(xvar))
                    parse(text = xvar)[[1]]
                yvarExpr <- if (!is.null(yvar))
                    parse(text = yvar)[[1]]
                zvarExpr <- if (!is.null(zvar))
                    parse(text = zvar)[[1]]
                groupsExpr <- if (!is.null(groups))
                    parse(text = groups)[[1]]
                enclos <- playState$env
                xIsCat <- is.categorical(eval(xvarExpr, dat, enclos))
                yIsCat <- is.categorical(eval(yvarExpr, dat, enclos))
                zIsCat <- is.categorical(eval(zvarExpr, dat, enclos))
                groupsIsCat <- is.categorical(eval(groupsExpr, dat, enclos))
            }

            ## for use with GtkComboBoxEntryNewText
            setCBEState <- function(widget, value, opts) {
                if (is.null(value)) {
                    widget["active"] <- 0
                    return()
                }
                index <- match(value, opts)
                if (!is.na(index)) {
                    widget["active"] <- (index - 1)
                } else {
                    #widget$setActiveText(value)
                    widget$getChild()$setText(value)
                }
            }

            ## stored lists of variable expressions
            varexprs <- playState$latticist$varexprs
            subsetopts <- playState$latticist$subsetopts
            aspectopts <- playState$latticist$aspectopts

            setCBEState(xvarW, spec$xvar, varexprs)
            setCBEState(yvarW, spec$yvar, varexprs)
            setCBEState(zvarW, spec$zvar, varexprs)
            setCBEState(groupsW, spec$groups, varexprs)
            setCBEState(condW, spec$cond, varexprs)
            setCBEState(cond2W, spec$cond2, varexprs)
            setCBEState(subsetW, spec$subset, subsetopts)

            xonW["active"] <- TRUE
            yonW["active"] <- TRUE
            xonW["sensitive"] <- !is.null(xvar)
            yonW["sensitive"] <- !is.null(yvar)
            xdiscW["active"] <- doXDisc
            ydiscW["active"] <- doYDisc
            xdiscW["visible"] <- !is.null(xvar) && !xIsCat
            ydiscW["visible"] <- !is.null(yvar) && !yIsCat

            zvarW["sensitive"] <- !is.null(xvar) && !is.null(yvar)
            condW["sensitive"] <- TRUE
            if (is.null(xvar) && is.null(yvar) &&
                (toString(spec$defaultPlot) %in%
                 c("marginal.plot", "")))
            {
                condW["sensitive"] <- FALSE
            }
            cond2W["sensitive"] <- !is.null(cond)

            aspectW["sensitive"] <- !is.null(xvar) || !is.null(yvar)
            if (is.null(spec$aspect)) {
                aspectW["active"] <- 0
            } else {
                aspectStr <- deparse(spec$aspect)
                setCBEState(aspectW, aspectStr, aspectopts)
            }

            nLevels <- spec$nLevels
            if (is.null(spec$nLevels))
                nLevels <- latticist.getOption("disc.levels")
            nLevelsW$setValue(nLevels)
            nLevelsW$setSensitive((!is.null(xvar) && !xIsCat) ||
                                  (!is.null(yvar) && !yIsCat) ||
                                  (!is.null(groups) && !groupsIsCat) ||
                                  (!is.null(cond)))

            linesW["active"] <- !identical(spec$doLines, FALSE)
            linesW["sensitive"] <- !is.null(xvar) || !is.null(yvar)
            tileW["active"] <- isTRUE(spec$doTile)
            tileW["visible"] <- !is.null(groups) && !groupsIsCat
            segmentsW["active"] <- isTRUE(spec$doSegments)
            segmentsW["visible"] <- !is.null(xvar) && !is.null(yvar)
            orLabelW["visible"] <- segmentsW["visible"]
            aserrorW["active"] <- isTRUE(spec$doAsError)
            aserrorW["visible"] <- isTRUE(spec$doSegments)

            isBivarNumeric <-
                (!is.null(xvar) && !is.null(yvar) && is.null(zvar) &&
                 !xIsCat && !yIsCat)

            promptW["visible"] <- (is.null(xvar) && is.null(yvar))
            xyflipW["visible"] <- !is.null(xvar) || !is.null(yvar)
            hexbinW["visible"] <- (isBivarNumeric && !isTRUE(spec$doHexbin) &&
                                   require("hexbin", quietly = TRUE))
            unbinW["visible"] <- isBivarNumeric && isTRUE(spec$doHexbin)
            explodeW["visible"] <- !is.null(groups) && groupsIsCat
            superposeW["visible"] <- !is.null(cond)
            go3DW["visible"] <- !is.null(groups) && !groupsIsCat
            squashW["visible"] <- !is.null(zvar)

            isVCD <- ((!is.null(xvar) && xIsCat) &&
                      (!is.null(yvar) && yIsCat) &&
                      (is.null(zvar) || zIsCat))
            strataW["visible"] <- isVCD
            strataW["active"] <- !identical(spec$doSeparateStrata, FALSE)
            scalesW["visible"] <- !isVCD
            scalesW["sensitive"] <- (!is.null(cond))

            ## scales setting
            scalesopts <- playState$latticist$scalesopts
            if (!is.null(spec$x.relation) &&
                !is.null(spec$y.relation))
            {
                scalesStr <- paste("x ", spec$x.relation, ", ",
                                   "y ", spec$y.relation, sep="")
                setCBEState(scalesW, scalesStr, scalesopts)
            } else {
                scalesW["active"] <- 0
            }
        }

        spec <- playState$latticist$spec
        varexprs <- playState$latticist$varexprs
        subsetopts <- playState$latticist$subsetopts
        aspectopts <- playState$latticist$aspectopts
        scalesopts <- playState$latticist$scalesopts

        ## CREATE THE GUI

        ## set up widgets
        box <- gtkHBox()

        niceButton <- function(label) {
            butt <- gtkEventBox()
            tmp <- gtkLabel(label)
            tmp$setMarkup(paste('<span foreground="blue"><u>',
                                label, '</u></span>', sep=""))
            butt$add(tmp)
            butt
        }

        ## action handlers for gtkComboBoxEntry
        addCBEHandlers <- function(widget, handler, data = NULL)
        {
            gSignalConnect(widget, "changed",
                           function(widget, data) {
                               if (widget["active"] > -1)
                                   handler(widget, data)
                           }, data = data)
            gSignalConnect(widget$getChild(), "activate",
                           handler, data = data)
        }

        ## TITLE, HYPERVAR, SUBSET
        setBox <- gtkVBox()
        titleBox <- gtkHBox()
        ## "help" button
        helpW <- niceButton("help")
        gSignalConnect(helpW, "button-press-event",
                       function(...) print(help("latticist")))
        titleBox$packStart(helpW, padding = 1, expand = FALSE)
        ## (Title)
        imgFile <- system.file("etc", "latticist_title.png",
                               package="latticist")
        titleImg <- gtkImageNewFromFile(imgFile)
        titleBox$packStart(titleImg, expand = TRUE)
        txt <- paste("v.", packageDescription("latticist")$Version)
        titleBox$packStart(gtkLabel(txt), expand = TRUE)
        setBox$packStart(titleBox, expand=FALSE)
        ## HYPERVARIATE buttons
        hyperBox <- gtkHBox()
        marginalsW <- gtkButton("marginals")
        splomW <- gtkButton("splom (pairs)")
        if (is.table(dat)) {
            parallelW <- gtkButton("table")
        } else {
            parallelW <- gtkButton("parallel")
        }
        ## 'tooltip-text' property requires recent GTK+
        try({
            marginalsW["tooltip-text"] <-
                "Show marginal distributions"
            splomW["tooltip-text"] <-
                "Show a scatterplot matrix (all pairs)"
            if (is.table(dat)) {
                parallelW["tooltip-text"] <-
                    "Show a multi-dimensional table"
            } else {
                parallelW["tooltip-text"] <-
                    "Show a parallel coordinates plot"
            }
        }, silent = TRUE)
        goHyper <- function(widget, user.data) {
            ## ask user to choose variables, or cancel
            spec <- playState$latticist$spec
            latticistChooseVars(dat, spec = spec,
                                handler = function(value)
                            {
                                playState$latticist$spec$varSubset <-
                                    value
                                playState$latticist$spec$xvar <- NULL
                                playState$latticist$spec$yvar <- NULL
                                playState$latticist$spec$zvar <- NULL
                                playState$latticist$spec$cond <- NULL
                                playState$latticist$spec$cond2 <- NULL
                                playState$latticist$spec$defaultPlot <-
                                    user.data
                                reCompose(playState, newPlot = TRUE)
                            })
        }
        gSignalConnect(marginalsW, "clicked", goHyper,
                       data = "marginal.plot")
        gSignalConnect(splomW, "clicked", goHyper,
                       data = "splom")
        gSignalConnect(parallelW, "clicked", goHyper,
                       data = "parallel")
        hyperBox$packStart(marginalsW, expand = FALSE, padding = 2)
        hyperBox$packStart(splomW, expand = FALSE, padding = 2)
        hyperBox$packStart(parallelW, expand = FALSE, padding = 2)
        setBox$packStart(hyperBox, expand = FALSE)
        ## SUBSET
        subsetBox <- gtkHBox()
        subsetBox$packStart(gtkLabel(" Subset: "), expand = FALSE)
        ## (prompt)
        promptxt <- paste('<span foreground="#666666">',
                          "Select variables --&gt;",
                          '</span>', sep = "")
        promptW <- gtkLabel("")
        promptW$setMarkup(promptxt)
        subsetBox$packEnd(promptW, expand = FALSE)
        setBox$packStart(subsetBox, expand = FALSE)
        ## subset
        subsetW <- gtkComboBoxEntryNewText()
        subsetW$show()
        subsetW["width-request"] <- -1
        for (item in subsetopts) subsetW$appendText(item)
        ## "changed" emitted on typing and selection
        addCBEHandlers(subsetW,
                       function(...) {
                           val <- subsetW$getActiveText()
                           if (identical(val, ""))
                               val <- NULL
                           playState$latticist$spec$subset <- val
                           reCompose(playState)
                       })
        setBox$packStart(subsetW, expand = FALSE)
        box$packStart(setBox, expand = FALSE, padding=1)

        box$packStart(gtkVSeparator(), expand=FALSE, padding=1)

        ## X Y VARS
        varsBox <- gtkVBox()
        xyBox <- gtkHBox()
        labtxt <- " Variables on axes: "
        xyLabelW <- gtkLabel(labtxt)
        xyLabelW$setMarkup(paste("<b>", labtxt, "</b>", sep=""))
        xyBox$packStart(xyLabelW, expand=FALSE)
        ## "switch" button
        xyflipW <- niceButton("switch")
        gSignalConnect(xyflipW, "button-press-event",
                       function(...) {
                           xvar <- playState$latticist$spec$xvar
                           yvar <- playState$latticist$spec$yvar
                           playState$latticist$spec$xvar <- yvar
                           playState$latticist$spec$yvar <- xvar
                           reCompose(playState, newPlot = TRUE)
                       })
        xyBox$packStart(xyflipW, padding = 2)
        ## "hexbin" button
        hexbinW <- niceButton("hexbin")
        gSignalConnect(hexbinW, "button-press-event",
                       function(...) {
                           playState$latticist$spec$doHexbin <- TRUE
                           reCompose(playState, newPlot = TRUE)
                       })
        xyBox$packStart(hexbinW, padding = 2)
        ## "points" button
        unbinW <- niceButton("points")
        gSignalConnect(unbinW, "button-press-event",
                       function(...) {
                           playState$latticist$spec$doHexbin <- FALSE
                           reCompose(playState, newPlot = TRUE)
                       })
        xyBox$packStart(unbinW, padding = 2)
        ## "reset" button
        resetW <- niceButton("reset")
        gSignalConnect(resetW, "button-press-event",
                       function(...) {
                           playState$latticist$spec <- list()
                           reCompose(playState, newPlot = TRUE)
                       })
        xyBox$packStart(resetW, padding = 2)
        varsBox$packStart(xyBox, expand=FALSE)
        ## Y VAR
        yvarBox <- gtkHBox()
        yonW <- gtkCheckButton("y= ")
        yonW["active"] <- TRUE
        gSignalConnect(yonW, "clicked",
                       function(...) {
                           playState$latticist$spec$yvar <- NULL
                           reCompose(playState, newPlot = TRUE)
                       })
        yvarBox$packStart(yonW, expand=FALSE)
        yvarW <- gtkComboBoxEntryNewText()
        yvarW$show()
        yvarW["width-request"] <- 100
        for (item in varexprs) yvarW$appendText(item)
        ## "changed" emitted on typing and selection
        addCBEHandlers(yvarW,
                       function(...) {
                           val <- yvarW$getActiveText()
                           if (identical(val, ""))
                               val <- NULL
                           playState$latticist$spec$yvar <- val
                           reCompose(playState, newPlot = TRUE)
                       })
        yvarBox$packStart(yvarW)
        ## discretize -- for numerics
        ydiscW <- gtkCheckButton("discr.")
        gSignalConnect(ydiscW, "clicked",
                       function(widget, ...) {
                           playState$latticist$spec$doYDisc <-
                               widget["active"]
                           reCompose(playState, newPlot = TRUE)
                       })
        yvarBox$packStart(ydiscW, expand=FALSE)
        varsBox$packStart(yvarBox, expand=FALSE)

        ## X VAR
        xvarBox <- gtkHBox()
        xonW <- gtkCheckButton("x= ")
        xonW["active"] <- TRUE
        gSignalConnect(xonW, "clicked",
                       function(...) {
                           playState$latticist$spec$xvar <- NULL
                           reCompose(playState, newPlot = TRUE)
                       })
        xvarBox$packStart(xonW, expand=FALSE)
        xvarW <- gtkComboBoxEntryNewText()
        xvarW$show()
        xvarW["width-request"] <- 100
        for (item in varexprs) xvarW$appendText(item)
        ## "changed" emitted on typing and selection
        addCBEHandlers(xvarW,
                       function(...) {
                           val <- xvarW$getActiveText()
                           if (identical(val, ""))
                               val <- NULL
                           playState$latticist$spec$xvar <- val
                           reCompose(playState, newPlot = TRUE)
                       })
        xvarBox$packStart(xvarW)
        ## "discretize" -- for numerics
        xdiscW <- gtkCheckButton("discr.")
        gSignalConnect(xdiscW, "clicked",
                       function(widget, ...) {
                           playState$latticist$spec$doXDisc <-
                               widget["active"]
                           reCompose(playState, newPlot = TRUE)
                       })
        xvarBox$packStart(xdiscW, expand=FALSE)
        varsBox$packStart(xvarBox, expand=FALSE)

        ## XY OPTS
        xyOptsBox <- gtkHBox()
        ## ASPECT
        xyOptsBox$packStart(gtkLabel(" Aspect:"), expand=FALSE)
        aspectW <- gtkComboBoxEntryNewText()
        aspectW$show()
        aspectW["width-request"] <- 50
        for (item in aspectopts) aspectW$appendText(item)
        ## "changed" emitted on typing and selection
        addCBEHandlers(aspectW,
                       function(...) {
                           val <- aspectW$getActiveText()
                           if (identical(val, ""))
                               val <- NULL
                           if (!is.null(val))
                               val <- parse(text = val)[[1]]
                           playState$latticist$spec$aspect <- val
                           reCompose(playState)
                       })
        xyOptsBox$packStart(aspectW)
        xyOptsBox$packStart(gtkLabel(""), padding=1)
        ## LINES
        linesW <- gtkCheckButton("Lines. ")
        gSignalConnect(linesW, "clicked",
                       function(widget, ...) {
                           playState$latticist$spec$doLines <-
                               widget["active"]
                           reCompose(playState)
                       })
        xyOptsBox$packStart(linesW, expand=FALSE)
        xyOptsBox$packStart(gtkLabel(""), padding=1)
        ## LEVELS
        xyOptsBox$packStart(gtkLabel("Levels:"), expand=FALSE)
        nLevelsW <- gtkSpinButton(min = 2, max = 16, step = 1)
        nLevelsW["width-request"] <- 40
        nLevelsW["digits"] <- 0
        gSignalConnect(nLevelsW, "value-changed",
                       function(widget, ...) {
                           playState$latticist$spec$nLevels <-
                               widget["value"]
                           reCompose(playState)
                       })
        xyOptsBox$packStart(nLevelsW, expand=FALSE)
        varsBox$packStart(xyOptsBox, expand=FALSE, padding=1)
        box$packStart(varsBox, padding=1)

        box$packStart(gtkVSeparator(), expand=FALSE, padding=1)

        ## GROUPS / Z VARIABLE
        gzBox <- gtkVBox()
        ## GROUPS
        groupsBox <- gtkHBox()
        groupsBox$packStart(gtkLabel(" Groups / Color: "), expand=FALSE)
        ## "explode" button
        explodeW <- niceButton("explode")
        gSignalConnect(explodeW, "button-press-event",
                       function(...) {
                           groups <- playState$latticist$spec$groups
                           if (is.null(playState$latticist$spec$cond)) {
                               playState$latticist$spec$cond <- groups
                           } else {
                               playState$latticist$spec$cond2 <- groups
                           }
                           playState$latticist$spec$groups <- NULL
                           reCompose(playState, newPlot = TRUE)
                       })
        groupsBox$packStart(explodeW)
        ## "squash" button
        squashW <- niceButton("squash")
        gSignalConnect(squashW, "button-press-event",
                       function(...) {
                           zvar <- playState$latticist$spec$zvar
                           playState$latticist$spec$groups <- zvar
                           playState$latticist$spec$zvar <- NULL
                           reCompose(playState, newPlot = TRUE)
                       })
        groupsBox$packStart(squashW)
        ## "go 3D" button
        go3DW <- niceButton("go 3D")
        gSignalConnect(go3DW, "button-press-event",
                       function(...) {
                           groups <- playState$latticist$spec$groups
                           playState$latticist$spec$zvar <- groups
                           playState$latticist$spec$groups <- NULL
                           reCompose(playState, newPlot = TRUE)
                       })
        groupsBox$packStart(go3DW)
        gzBox$packStart(groupsBox, expand=FALSE, padding=1)
        ## groups
        gBox <- gtkHBox()
        groupsW <- gtkComboBoxEntryNewText()
        groupsW$show()
        groupsW["width-request"] <- 100
        for (item in varexprs) groupsW$appendText(item)
        ## "changed" emitted on typing and selection
        addCBEHandlers(groupsW,
                       function(...) {
                           val <- groupsW$getActiveText()
                           if (identical(val, ""))
                               val <- NULL
                           playState$latticist$spec$groups <- val
                           reCompose(playState)
                       })
        gBox$packStart(groupsW)
        ## tile
        tileW <- gtkCheckButton("tile")
        gSignalConnect(tileW, "clicked",
                       function(widget, ...) {
                           playState$latticist$spec$doTile <-
                               widget["active"]
                           reCompose(playState)
                       })
        gBox$packStart(tileW)
        gzBox$packStart(gBox, expand=FALSE)

        ## Z / SEGMENTS VARIABLE
        zBox <- gtkHBox()
        zBox$packStart(gtkLabel(" Depth (3D)"), expand=FALSE)
        ## segments option
        segmentsW <- gtkCheckButton("Segments (x--z) ")
        gSignalConnect(segmentsW, "clicked",
                       function(widget, ...) {
                           playState$latticist$spec$doSegments <-
                               widget["active"]
                           reCompose(playState, newPlot = TRUE)
                       })
        orLabelW <- gtkLabel(" or")
        zBox$packStart(orLabelW)
        zBox$packStart(segmentsW, expand=FALSE)
        gzBox$packStart(zBox, expand=FALSE, padding=1)
        ## z (3D depth)
        zvarBox <- gtkHBox()
        zvarBox$packStart(gtkLabel(" z= "), expand = FALSE)
        zvarW <- gtkComboBoxEntryNewText()
        zvarW$show()
        zvarW["width-request"] <- 100
        for (item in varexprs) zvarW$appendText(item)
        ## "changed" emitted on typing and selection
        addCBEHandlers(zvarW,
                       function(...) {
                           val <- zvarW$getActiveText()
                           if (identical(val, ""))
                               val <- NULL
                           playState$latticist$spec$zvar <- val
                           reCompose(playState, newPlot = TRUE)
                       })
        zvarBox$packStart(zvarW)
        ## doAsError option
        aserrorW <- gtkCheckButton("(x +/- z)") #"as error")
        gSignalConnect(aserrorW, "clicked",
                       function(widget, ...) {
                           playState$latticist$spec$doAsError <-
                               widget["active"]
                           reCompose(playState)
                       })
        zvarBox$packStart(aserrorW)
        gzBox$packStart(zvarBox, expand=FALSE)
        box$packStart(gzBox, expand=FALSE, padding=1)

        box$packStart(gtkVSeparator(), expand=FALSE, padding=1)

        ## CONDITIONING VARS
        cvarsBox <- gtkVBox()
        cBox <- gtkHBox()
        cBox$packStart(gtkLabel(" Conditioning: "), expand=FALSE)
        ## "superpose" button
        superposeW <- niceButton("superpose")
        gSignalConnect(superposeW, "button-press-event",
                       function(...) {
                           conds <- playState$latticist$spec$cond
                           if (!is.null(playState$latticist$spec$cond2)) {
                               conds <- sprintf("paste(%s, %s)", conds,
                                                playState$latticist$spec$cond2)
                           }
                           if (!is.null(playState$latticist$spec$groups)) {
                               conds <- sprintf("paste(%s, %s)", conds,
                                                playState$latticist$spec$groups)
                           }
                           playState$latticist$spec$groups <- conds
                           playState$latticist$spec$cond <- NULL
                           playState$latticist$spec$cond2 <- NULL
                           reCompose(playState, newPlot = TRUE)
                       })
        cBox$packStart(superposeW)
        cvarsBox$packStart(cBox, expand=FALSE, padding=1)
        ## first conditioning variable
        condBox <- gtkHBox()
        condW <- gtkComboBoxEntryNewText()
        condW$show()
        condW["width-request"] <- 100
        for (item in varexprs) condW$appendText(item)
        ## "changed" emitted on typing and selection
        addCBEHandlers(condW,
                       function(...) {
                           val <- condW$getActiveText()
                           if (identical(val, ""))
                               val <- NULL
                           playState$latticist$spec$cond <- val
                           reCompose(playState, newPlot = TRUE)
                       })
        condBox$packStart(condW)
        cvarsBox$packStart(condBox, expand=FALSE)
        ## second conditioning variable
        cond2Box <- gtkHBox()
        cond2W <- gtkComboBoxEntryNewText()
        cond2W$show()
        cond2W["width-request"] <- 100
        for (item in varexprs) cond2W$appendText(item)
        ## "changed" emitted on typing and selection
        addCBEHandlers(cond2W,
                       function(...) {
                           val <- cond2W$getActiveText()
                           if (identical(val, ""))
                               val <- NULL
                           playState$latticist$spec$cond2 <- val
                           reCompose(playState, newPlot = TRUE)
                       })
        cond2Box$packStart(cond2W)
        cvarsBox$packStart(cond2Box, expand=FALSE)
        ## SCALES
        scalesBox <- gtkHBox()
        ## for Lattice plots
        scalesLabW <- gtkLabel("Scales:")
        scalesBox$packStart(scalesLabW, expand=FALSE)
        scalesW <- gtkComboBoxNewText()
        scalesW$show()
        scalesW["width-request"] <- 80
        for (item in scalesopts) scalesW$appendText(item)
        ## "changed" emitted on typing and selection
        gSignalConnect(scalesW, "changed",
                       function(...) {
                           scalesIdx <- scalesW$getActive() + 1
                           if (scalesIdx <= 1) return()
                           scalesopts <- playState$latticist$scalesopts
                           tmp <- strsplit(scalesopts[scalesIdx], ", ")[[1]]
                           x.relation <- substring(tmp[1], first=3)
                           y.relation <- substring(tmp[2], first=3)
                           playState$latticist$spec$x.relation <- x.relation
                           playState$latticist$spec$y.relation <- y.relation
                           reCompose(playState)
                       })
        ## for vcd plots
        strataW <- gtkCheckButton("separate strata")
        gSignalConnect(strataW, "clicked",
                       function(widget, ...) {
                           playState$latticist$spec$doSeparateStrata <-
                               widget["active"]
                           reCompose(playState, newPlot = TRUE)
                       })
        scalesBox$packStart(strataW)
        scalesBox$packStart(scalesW)
        cvarsBox$packStart(scalesBox, expand=FALSE, padding=1)
        box$packStart(cvarsBox, padding=1)

        updateFromSpec()

        ## add it directly to the window (not a toolbar!)
        ## use blockRedraws() to maintain current device size
        playwith:::blockRedraws({
            playState$widgets$vbox$packEnd(box, expand=FALSE)
        }, playState = playState)

        playState$widgets$latticist <- box
        return(NA)
    }
}

.profLatticist <- function(n = 20000) {
    audit <- read.csv(system.file("csv", "audit.csv", package = "rattle"))
    audit <- lapply(audit, rep, length.out=n)
    gc()
    Rprof(tmp <- tempfile())
    latticist(audit)
    Rprof()
    print(summaryRprof(tmp))
    unlink(tmp)
}

