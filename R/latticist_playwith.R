## latticist: a Lattice-based exploratory visualisation GUI
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

NULLNAMES <- c("", "(none)", "NULL")

latticist_playwith <-
    function(dat,
             spec = list(),
             datArg = substitute(dat),
             ...,
             labels = rownames(dat),
             time.mode = FALSE,
             height = playwith.getOption("height") - 1,
             plot.call)
{
    stopifnot(require("playwith"))

    title <- paste("Latticist:",
                   toString(deparse(datArg), width=30))

    if (missing(plot.call))
        plot.call <-
            latticistCompose(dat, spec = spec, datArg = datArg)

    ## this is the constructor (an init.action)
    lattAction <- latticistToolConstructor(dat, datArg = datArg)
    ## this list will store state in playState$latticist
    lattState <- latticistInitOptions(dat, datArg = datArg)
    lattState$spec <- spec
                                        #if (!is.list(eval.args))
                                        #    eval.args <- as.list(eval.args)
                                        #eval.args$envir <- parent.frame()

    playwith(plot.call = plot.call,
             title = title, ...,
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
        ## this function is triggered by user changes to widgets
        reCompose <- function(playState, newPlot = FALSE)
        {
            spec <- playState$latticist$spec
            if (newPlot) {
                ## ignore old aspect and scales for new plot types
                spec$aspect <- NULL
                spec$x.relation <- NULL
                spec$y.relation <- NULL
            }
            if (isTRUE(spec$doXDisc) && isTRUE(spec$doYDisc) &&
                require("hexbin")) {
                ## if both are discretized numerics, use 2D binning
                spec$doXDisc <- FALSE
                spec$doYDisc <- FALSE
                spec$doHexbin <- TRUE
            }
            ## compose plot call from latticist specification
            oldCall <- playState$call
            playState$call <-
                latticistCompose(dat, spec, datArg = datArg,
                                 enclos = playState$env)
            ## check whether anything changed
            updateMainCall(playState) ## set canonical arguments
            if (identical(deparse(playState$call, control = NULL),
                          deparse(oldCall, control = NULL)))
                return()
            ## need playNewPlot (not playReplot) to reload latticist
            playNewPlot(playState)
        }

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
            groupsIsCat <- !is.null(groups)
        } else {
            xvarExpr <- if (!is.null(xvar))
                parse(text = xvar)[[1]]
            yvarExpr <- if (!is.null(yvar))
                parse(text = yvar)[[1]]
            groupsExpr <- if (!is.null(groups))
                parse(text = groups)[[1]]
            enclos <- playState$env
            xIsCat <- is.categorical(eval(xvarExpr, dat, enclos))
            yIsCat <- is.categorical(eval(yvarExpr, dat, enclos))
            groupsIsCat <- is.categorical(eval(groupsExpr, dat, enclos))
        }

        xvarStr <- toString(spec$xvar)
        yvarStr <- toString(spec$yvar)
        zvarStr <- toString(spec$zvar)
        groupsStr <- toString(spec$groups)
        condStr <- toString(spec$cond)
        cond2Str <- toString(spec$cond2)
        subsetStr <- toString(spec$subset)

        ## update stored lists of variable expressions
        varexprs <- playState$latticist$varexprs
        varexprs <- unique(c(varexprs,
                             xvarStr, yvarStr, zvarStr,
                             condStr, cond2Str, groupsStr))
        ## replace all synonyms of "NULL" with just one
        varexprs <- varexprs[!(varexprs %in% NULLNAMES)]
        varexprs <- c(NULLNAMES[[1]], varexprs)
        playState$latticist$varexprs <- varexprs

        ## update subset options
        subsetopts <- playState$latticist$subsetopts
        subsetopts <-
            unique(c(subsetopts,
                     if (!is.null(subset) && !isTRUE(subset))
                     subsetStr))
        playState$latticist$subsetopts <- subsetopts

        ## aspect setting
        aspectopts <- playState$latticist$aspectopts
        aspectVal <- spec$aspect
        if (!is.null(aspectVal)) {
            aspectVal <- deparse(aspectVal)
            aspectopts <-
                unique(c(aspectopts, aspectVal))
        }

        ## scales setting
        scalesopts <- playState$latticist$scalesopts
        scalesVal <- NULL
        if (!is.null(spec$x.relation) &&
            !is.null(spec$y.relation))
        {
            scalesVal <- paste("x ", spec$x.relation, ", ",
                               "y ", spec$y.relation, sep="")
        }

        nLevels <- INIT.NLEVELS
        if (!is.null(spec$nLevels))
            nLevels <- spec$nLevels


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
        txt <- paste("Latticist",
                     packageDescription("playwith")$Version)
        titleW <- gtkLabel(txt)
        titleW$setMarkup(paste("<big><b><i>", txt, "</i></b></big>"))
        titleBox$packStart(titleW, expand = TRUE)
        setBox$packStart(titleBox, expand=FALSE)
        ## (prompt)
        promptxt <- paste('<span foreground="#666666">',
                          "Select variables --&gt;",
                          '</span>', sep = "")
        promptW <- gtkLabel("")
        promptW$setMarkup(promptxt)
        promptW["visible"] <- (is.null(xvar) && is.null(yvar))
        ## SUBSET
        subsetBox <- gtkHBox()
        subsetBox$packStart(gtkLabel(" Subset: "), expand = FALSE)
        subsetBox$packEnd(promptW, expand = FALSE)
        setBox$packStart(subsetBox, expand=FALSE)
        ## subset
        subsetW <- gtkComboBoxEntryNewText()
        subsetW$show()
        subsetW["width-request"] <- -1
        for (item in subsetopts) subsetW$appendText(item)
        index <- match(subsetStr, subsetopts)
        if (is.na(index)) index <- 1 ## should never happen
        subsetW["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        addCBEHandlers(subsetW,
                       function(...) {
                           val <- subsetW$getActiveText()
                           if (val %in% NULLNAMES)
                               val <- NULL
                           playState$latticist$spec$subset <- val
                           reCompose(playState)
                       })
        setBox$packStart(subsetW, expand = FALSE)
        ## HYPERVAR
        hyperBox0 <- gtkHBox()
        hyperBox0$packStart(gtkLabel(" Hyper-variate plots: "),
                            expand = FALSE)
        ## "choose variables" button
        choosevarsW <- niceButton("choose variables")
        choosevarsW["visible"] <- (is.null(xvar) && is.null(yvar))
        gSignalConnect(choosevarsW, "button-press-event",
                       function(...) {
                           if (isTRUE(doChooseVars()))
                               reCompose(playState)
                       })
        hyperBox0$packStart(choosevarsW)
        setBox$packStart(hyperBox0, expand=FALSE, padding = 1)
        ## hypervar reset buttons
        hyperBox <- gtkHBox()
        marginalsW <- gtkButton("marginals")
        marginalsW["tooltip-text"] <-
            "Show marginal distributions"
        splomW <- gtkButton("splom")
        splomW["tooltip-text"] <-
            "Show a scatterplot matrix (all pairs)"
        parallelW <- gtkButton("parallel")
        parallelW["tooltip-text"] <-
            "Show a parallel coordinates plot"
        goHyper <- function(widget, user.data) {
            ## ask user to choose variables, or cancel
            spec <- playState$latticist$spec
            result <- latticistChooseVars(dat, spec = spec)
            if (!isTRUE(result$ok)) return()
            playState$latticist$spec$varSubset <-
                result$varSubset
            ## reset x/y/z to force hypervariate
            playState$latticist$spec$xvar <- NULL
            playState$latticist$spec$yvar <- NULL
            playState$latticist$spec$zvar <- NULL
            playState$latticist$spec$defaultPlot <- user.data
            reCompose(playState, newPlot = TRUE)
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
        box$packStart(setBox, expand=FALSE, padding=1)

        box$packStart(gtkVSeparator(), expand=FALSE, padding=1)

        ## X Y VARS
        varsBox <- gtkVBox()
        xyBox <- gtkHBox()
        isBivarNumeric <-
            (!is.null(xvar) && !is.null(yvar) && is.null(zvar) &&
             !xIsCat && !yIsCat)
        isBivarCat <-
            (!is.null(xvar) && !is.null(yvar) && is.null(zvar) &&
             xIsCat && yIsCat)
        labtxt <- " Variables / expressions on axes: "
        if (!is.null(xvar) || !is.null(yvar))
            labtxt <- " Variables on axes: "
        xyLabelW <- gtkLabel(labtxt)
        xyLabelW$setMarkup(paste("<b>", labtxt, "</b>", sep=""))
        xyBox$packStart(xyLabelW, expand=FALSE)
        ## "switch" button
        xyflipW <- niceButton("switch")
        xyflipW["visible"] <- !is.null(xvar) || !is.null(yvar)
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
        hexbinW["visible"] <- (isBivarNumeric && !doXDisc && !doYDisc &&
                               require("hexbin", quietly = TRUE))
        gSignalConnect(hexbinW, "button-press-event",
                       function(...) {
                           playState$latticist$spec$doHexbin <- TRUE
                           reCompose(playState, newPlot = TRUE)
                       })
        xyBox$packStart(hexbinW, padding = 2)
        ## "points" button
        unbinW <- niceButton("points")
        unbinW["visible"] <- isBivarNumeric && isTRUE(spec$doHexbin)
        gSignalConnect(unbinW, "button-press-event",
                       function(...) {
                           playState$latticist$spec$doHexbin <- FALSE
                           reCompose(playState, newPlot = TRUE)
                       })
        xyBox$packStart(unbinW, padding = 2)
        varsBox$packStart(xyBox, expand=FALSE)
        ## Y VAR
        yvarBox <- gtkHBox()
        yonW <- gtkCheckButton("y= ")
        yonW["active"] <- TRUE
        yonW["sensitive"] <- !is.null(yvar)
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
        index <- match(yvarStr, varexprs)
        if (is.na(index)) index <- 1
        yvarW["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        addCBEHandlers(yvarW,
                       function(...) {
                           val <- yvarW$getActiveText()
                           if (val %in% NULLNAMES)
                               val <- NULL
                           playState$latticist$spec$yvar <- val
                           reCompose(playState, newPlot = TRUE)
                       })
        yvarBox$packStart(yvarW)
        ## discretize -- for numerics
        ydiscW <- gtkCheckButton("discretize")
        ydiscW["active"] <- doYDisc
        ydiscW["sensitive"] <- !is.null(yvar)
        ydiscW["visible"] <- !yIsCat
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
        xonW["sensitive"] <- !is.null(xvar)
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
        index <- match(xvarStr, varexprs)
        if (is.na(index)) index <- 1
        xvarW["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        addCBEHandlers(xvarW,
                       function(...) {
                           val <- xvarW$getActiveText()
                           if (val %in% NULLNAMES)
                               val <- NULL
                           playState$latticist$spec$xvar <- val
                           reCompose(playState, newPlot = TRUE)
                       })
        xvarBox$packStart(xvarW)
        ## "discretize" -- for numerics
        xdiscW <- gtkCheckButton("discretize")
        xdiscW["active"] <- doXDisc
        xdiscW["sensitive"] <- !is.null(xvar)
        xdiscW["visible"] <- !xIsCat
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
        aspectW["sensitive"] <- !is.null(xvar) || !is.null(yvar)
        aspectW["width-request"] <- 50
        for (item in aspectopts) aspectW$appendText(item)
        if (!is.null(aspectVal)) {
            index <- match(aspectVal, aspectopts)
            if (is.na(index)) index <- 0
            aspectW["active"] <- (index - 1)
        }
        ## "changed" emitted on typing and selection
        addCBEHandlers(aspectW,
                       function(...) {
                           val <- aspectW$getActiveText()
                           if (val %in% NULLNAMES)
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
        linesW["active"] <- !identical(spec$doLines, FALSE)
        linesW["sensitive"] <- !is.null(xvar) || !is.null(yvar)
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
        nLevelsW <- gtkSpinButton(min=1, max=16, step=1)
        nLevelsW["width-request"] <- 40
        nLevelsW["digits"] <- 0
        nLevelsW$setValue(nLevels)
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
        explodeW["visible"] <- !is.null(groups) && groupsIsCat
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
        ## "go 3D" button
        go3DW <- niceButton("go 3D")
        go3DW["visible"] <- !is.null(groups) && !groupsIsCat
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
        index <- match(groupsStr, varexprs)
        if (is.na(index)) index <- 1
        groupsW["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        addCBEHandlers(groupsW,
                       function(...) {
                           val <- groupsW$getActiveText()
                           if (val %in% NULLNAMES)
                               val <- NULL
                           playState$latticist$spec$groups <- val
                           reCompose(playState)
                       })
        gBox$packStart(groupsW)
        ## tile
        tileW <- gtkCheckButton("tile")
        tileW["active"] <- isTRUE(spec$doTile)
        tileW["visible"] <- !is.null(groups) && !groupsIsCat
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
        segmentsW["active"] <- isTRUE(spec$doSegments)
        segmentsW["visible"] <- !is.null(xvar) && !is.null(yvar)
        gSignalConnect(segmentsW, "clicked",
                       function(widget, ...) {
                           playState$latticist$spec$doSegments <-
                               widget["active"]
                           reCompose(playState, newPlot = TRUE)
                       })
        orLabelW <- gtkLabel(" or")
        orLabelW["visible"] <- segmentsW["visible"]
        zBox$packStart(orLabelW)
        zBox$packStart(segmentsW, expand=FALSE)
        ## "squash" button
        squashW <- niceButton("squash")
        squashW["visible"] <- !is.null(zvar)
        gSignalConnect(squashW, "button-press-event",
                       function(...) {
                           zvar <- playState$latticist$spec$zvar
                           playState$latticist$spec$groups <- zvar
                           playState$latticist$spec$zvar <- NULL
                           reCompose(playState, newPlot = TRUE)
                       })
        zBox$packStart(squashW)
        gzBox$packStart(zBox, expand=FALSE, padding=1)
        ## z (3D depth)
        zvarBox <- gtkHBox()
        zvarBox$packStart(gtkLabel(" z= "), expand = FALSE)
        zvarW <- gtkComboBoxEntryNewText()
        zvarW$show()
        zvarW["sensitive"] <- !is.null(xvar) && !is.null(yvar)
        zvarW["width-request"] <- 100
        for (item in varexprs) zvarW$appendText(item)
        index <- match(zvarStr, varexprs)
        if (is.na(index)) index <- 1
        zvarW["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        addCBEHandlers(zvarW,
                       function(...) {
                           val <- zvarW$getActiveText()
                           if (val %in% NULLNAMES)
                               val <- NULL
                           playState$latticist$spec$zvar <- val
                           reCompose(playState, newPlot = TRUE)
                       })
        zvarBox$packStart(zvarW)
        ## asError option
        aserrorW <- gtkCheckButton("as error") # (x+/-z)")
        aserrorW["active"] <- isTRUE(spec$doAsError)
        aserrorW["visible"] <- isTRUE(spec$doSegments)
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
        superposeW["visible"] <- !is.null(cond)
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
        condW["sensitive"] <- (playState$callName != "marginal.plot")
        condW["width-request"] <- 100
        for (item in varexprs) condW$appendText(item)
        index <- match(condStr, varexprs)
        if (is.na(index)) index <- 1
        condW["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        addCBEHandlers(condW,
                       function(...) {
                           val <- condW$getActiveText()
                           if (val %in% NULLNAMES)
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
        cond2W["sensitive"] <- !is.null(cond)
        cond2W["width-request"] <- 100
        for (item in varexprs) cond2W$appendText(item)
        index <- match(cond2Str, varexprs)
        if (is.na(index)) index <- 1
        cond2W["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        addCBEHandlers(cond2W,
                       function(...) {
                           val <- cond2W$getActiveText()
                           if (val %in% NULLNAMES)
                               val <- NULL
                           playState$latticist$spec$cond2 <- val
                           reCompose(playState, newPlot = TRUE)
                       })
        cond2Box$packStart(cond2W)
        cvarsBox$packStart(cond2Box, expand=FALSE)
        ## SCALES
        scalesBox <- gtkHBox()
        if (playState$callName %in% c("mosaic", "cotabplot")) {
            ## vcd plot
            strataW <- gtkCheckButton("separate strata")
            strataW["active"] <- !identical(spec$doSeparateStrata, FALSE)
            gSignalConnect(strataW, "clicked",
                       function(widget, ...) {
                           playState$latticist$spec$doSeparateStrata <-
                               widget["active"]
                           reCompose(playState, newPlot = TRUE)
                       })
            scalesBox$packStart(strataW)

        } else {
            ## Lattice plot
            scalesBox$packStart(gtkLabel("Scales:"), expand=FALSE)
            scalesW <- gtkComboBoxNewText()
            scalesW$show()
            scalesW["sensitive"] <- (prod(dim(playState$trellis)) > 1)
            scalesW["width-request"] <- 80
            for (item in scalesopts) scalesW$appendText(item)
            if (!is.null(scalesVal)) {
                index <- match(scalesVal, scalesopts)
                if (is.na(index)) index <- 0
                scalesW["active"] <- (index - 1)
            }
            ## "changed" emitted on typing and selection
            gSignalConnect(scalesW, "changed",
                           function(...) {
                               scalesIdx <- scalesW$getActive() + 1
                               scalesopts <- playState$latticist$scalesopts
                               tmp <- strsplit(scalesopts[scalesIdx], ", ")[[1]]
                               x.relation <- substring(tmp[1], first=3)
                               y.relation <- substring(tmp[2], first=3)
                               playState$latticist$spec$x.relation <- x.relation
                               playState$latticist$spec$y.relation <- y.relation
                               reCompose(playState)
                           })
            scalesBox$packStart(scalesW)
        }
        cvarsBox$packStart(scalesBox, expand=FALSE, padding=1)
        box$packStart(cvarsBox, padding=1)

        ## add it directly to the window (not a toolbar!)
        ## use blockRedraws() to maintain current device size

        if (!is.null(playState$widgets$latticist)) {
            hideWidgetNoRedraw(playState$widgets$latticist,
                               horiz = TRUE,
                               playState = playState)
            playState$widgets$latticist$destroy()
        }
        blockRedraws({
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

