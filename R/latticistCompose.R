## latticist: a Lattice-based exploratory visualisation GUI
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

latticistCompose <-
    function(dat, spec = list(),
             datArg = substitute(dat),
             enclos = parent.frame())
{
    force(datArg)
    doCompose <-
        function(xvar = NULL, yvar = NULL, zvar = NULL,
                 groups = NULL, cond = NULL, cond2 = NULL,
                 subset = NULL, varSubset = NULL,
                 aspect = NULL, aspect3D = NULL,
                 x.relation = NULL, y.relation = NULL,
                 doXDisc = FALSE, doYDisc = FALSE, nLevels = NULL,
                 doLines = TRUE, doSeparateStrata = TRUE,
                 doHexbin = FALSE, doTile = FALSE,
                 doSegments = FALSE, doAsError = FALSE,
                 defaultPlot = latticist.getOption("defaultPlot"))
        {
            ## keep original variable expressions for titles etc
            xvarStr <- xvar
            yvarStr <- yvar
            zvarStr <- zvar
            groupsStr <- groups
            condStr <- cond
            cond2Str <- cond2
            subsetStr <- subset

            ## parse into language objects
            xvar <- if (!is.null(xvar)) parse(text = xvar)[[1]]
            yvar <- if (!is.null(yvar)) parse(text = yvar)[[1]]
            zvar <- if (!is.null(zvar)) parse(text = zvar)[[1]]
            groups <- if (!is.null(groups)) parse(text = groups)[[1]]
            cond <- if (!is.null(cond)) parse(text = cond)[[1]]
            cond2 <- if (!is.null(cond2)) parse(text = cond2)[[1]]
            subset <- if (!is.null(subset)) parse(text = subset)[[1]]

            if (is.null(subset)) subset <- TRUE
            if (is.null(nLevels))
                nLevels <- latticist.getOption("disc.levels")

            deparse1 <- function(expr)
                paste(deparse(expr, width = 500, control = NULL,
                              backtick = TRUE),
                      collapse = " ")

            isUnordered <- function(x, val) {
                ## need this because is.ordered(cut()) == FALSE!
                if (is.call.to(x, "cut")) return(FALSE)
                if (is.call.to(x, "cut2")) return(FALSE)
                ## assumes is.categorical(val)
                !is.ordered(val) && !is.shingle(val)
            }

            ## work out data types
            xIsCat <- yIsCat <- zIsCat <-
                groupsIsCat <- condIsCat <- cond2IsCat <- NA
            xVal <- yVal <- zVal <-
                groupsVal <- condVal <- cond2Val <- NULL
            groupsIsNum <- FALSE
            nPoints <- 0
            anyNumerics <- NA

            if (is.table(dat)) {
                xIsCat <- !is.null(xvar)
                yIsCat <- !is.null(yvar)
                zIsCat <- !is.null(zvar)
                groupsIsCat <- !is.null(groups)
                condIsCat <- !is.null(cond)
                cond2IsCat <- !is.null(cond2)

                dfdat <- as.data.frame(dat) ## TODO: need this?

                xVal <- eval(xvar, dfdat, enclos)
                yVal <- eval(yvar, dfdat, enclos)
                zVal <- eval(zvar, dfdat, enclos)
                groupsVal <- eval(groups, dfdat, enclos)
                condVal <- eval(cond, dfdat, enclos)
                cond2Val <- eval(cond2, dfdat, enclos)
                subsetVal <- eval(subset, dfdat, enclos)

                if (isTRUE(subsetVal)) {
                    nPoints <- sum(dat, na.rm = TRUE)
                } else {
                    ## handle integer/logical/recycling
                    nPoints <- sum(dfdat$Freq[subsetVal])
                }
                rm(dfdat)

                anyNumerics <- FALSE

            } else {
                ## dat is a data.frame
                ## evaluate to check types
                xVal <- eval(xvar, dat, enclos)
                yVal <- eval(yvar, dat, enclos)
                zVal <- eval(zvar, dat, enclos)
                groupsVal <- eval(groups, dat, enclos)
                condVal <- eval(cond, dat, enclos)
                cond2Val <- eval(cond2, dat, enclos)
                subsetVal <- eval(subset, dat, enclos)
                xIsCat <- is.categorical(xVal)
                yIsCat <- is.categorical(yVal)
                zIsCat <- is.categorical(zVal)
                groupsIsCat <- is.categorical(groupsVal)
                groupsIsNum <- is.numeric(groupsVal)
                condIsCat <- is.categorical(condVal)
                cond2IsCat <- is.categorical(cond2Val)
                ## calculate number of data points
                if (is.null(xVal) && is.null(yVal)) {
                    ## hypervariate: just report size of subset
                    if (isTRUE(subsetVal)) {
                        nPoints <- NROW(dat)
                    } else {
                        ## handle integer/logical/recycling
                        tmp <- rep(TRUE, NROW(dat))
                        nPoints <- sum(tmp[subsetVal])
                    }
                } else if (!is.null(xVal) && !is.null(yVal))
                    nPoints <- sum(is.finite(xVal[subsetVal]) &
                                   is.finite(yVal[subsetVal]))
                else if (!is.null(xVal))
                    nPoints <- sum(is.finite(xVal[subsetVal]))
                else if (!is.null(yVal))
                    nPoints <- sum(is.finite(yVal[subsetVal]))

                ## discretize
                doXDisc <- doXDisc && (!is.null(xvar) && !xIsCat)
                doYDisc <- doYDisc && (!is.null(yvar) && !yIsCat)
                doCondDisc <- (!is.null(cond) && !condIsCat)
                doCond2Disc <- (!is.null(cond2) && !cond2IsCat)

                ## if there are any numerical variables on plot
                ## we can use shingles (otherwise plotting a "table" method)
                anyNumerics <- ((!is.null(xvar) && !xIsCat && !doXDisc) ||
                                (!is.null(yvar) && !yIsCat && !doYDisc))
                if (anyNumerics) {
                    ## use shingles where appropriate
                    overlap <- latticist.getOption("shingle.overlap")
                    if (doXDisc) xvar <- call("equal.count", xvar, nLevels) ## or cut?
                    if (doYDisc) yvar <- call("equal.count", yvar, nLevels) ## or cut?
                    ## conditioning variables
                    if (doCondDisc) cond <- call("equal.count", cond, nLevels)
                    if (doCond2Disc) cond2 <- call("equal.count", cond2, nLevels)
                    if (overlap != 0.5) {
                        if (doXDisc) xvar$overlap <- overlap
                        if (doYDisc) yvar$overlap <- overlap
                        if (doCondDisc) cond$overlap <- overlap
                        if (doCond2Disc) cond2$overlap <- overlap
                    }
                } else {
                    ## table method, need factors not shingles
                    if (doXDisc) {
                        if (is.null(yvar) || doYDisc)
                            xvar <- call("cut", xvar, nLevels)
                        else xvar <- call("cutEq", xvar, nLevels)
                    }
                    if (doYDisc) {
                        if (is.null(xvar) || doXDisc)
                            yvar <- call("cut", yvar, nLevels)
                        else yvar <- call("cutEq", yvar, nLevels)
                    }
                    if (doCondDisc) cond <- call("cutEq", cond, nLevels)
                    if (doCond2Disc) cond2 <- call("cutEq", cond2, nLevels)
                }
                ## re-evaluate data if changed
                if (doXDisc) xVal <- eval(xvar, dat, enclos)
                if (doYDisc) yVal <- eval(yvar, dat, enclos)
                if (doCondDisc) condVal <- eval(cond, dat, enclos)
                if (doCond2Disc) cond2Val <- eval(cond2, dat, enclos)
                xIsCat <- is.categorical(xVal)
                yIsCat <- is.categorical(yVal)
            }
            groupsWasCat <- groupsIsCat

            ## if only one conditioning term, call it cond
            if (is.null(cond) && !is.null(cond2)) {
                cond <- cond2
                condVal <- cond2Val
                cond2 <- cond2Val <- NULL
            }

            ## reorder conditioning (TODO: index.cond)
            if (!is.null(condVal)) {
                                        #                    if (!is.null(yVal) || !is.null(xVal)) {
                                        #                        cond <- call("reorder", cond,
                                        #                                   if (!is.null(yVal)) yvar else xvar)
                                        #                    }
                                        #                    condVal <- tryEval(cond, dat)
            }

            ## combined conditioning term (may be NULL)
            conds <- cond
            if (!is.null(cond2)) conds <- call("+", cond, cond2)
            nCondLevels <- 1
            MAXPANELS <- latticist.getOption("max.panels")
            tooManyPanels <- FALSE
            if (!is.null(conds)) {
                nCondLevels <- nlevels(condVal[subsetVal])
                if (!is.null(cond2Val))
                    nCondLevels <- nCondLevels * nlevels(cond2Val[subsetVal])
                if (nCondLevels > MAXPANELS)
                    tooManyPanels <- TRUE
            }

            ## work out (average) number of points in each panel
            nPointsPerPanel <- round(nPoints / nCondLevels)
            ## TODO: do this better?
            ## hypervariate plots are more complex
            if (is.null(xvar) && is.null(yvar) &&
                !is.table(dat))
            {
                nVars <- NCOL(dat)
                if (!is.null(varSubset))
                    nVars <- length(varSubset)
                if (defaultPlot == "marginal.plot") {
                    nPointsPerPanel <-
                        nPointsPerPanel * nVars

                } else if (defaultPlot == "splom") {
                    nPointsPerPanel <-
                        nPointsPerPanel * (nVars * (nVars - 1)) / 2

                } else if (defaultPlot == "parallel") {
                    nPointsPerPanel <-
                        nPointsPerPanel * nVars
                }
            }

            ## create template plot call
            call <- call("xyplot", 0 ~ 0)
            call$data <- datArg
            call$groups <- if (groupsIsCat) groups
            call$subset <-
                if (!isTRUE(subset)) subset else NULL
            ## build up 'scales' list and assign at the end
            scales <- list()

            ## put shingle levels on axis
            if (anyNumerics) {
                if (doXDisc)
                    scales$x$limits <-
                        as.character(levels(xVal))
                if (doYDisc)
                    scales$y$limits <-
                        as.character(levels(yVal))
            }
            ## put shingle levels in strip
#            if (anyNumerics && doCondDisc &&
#                (is.null(cond2) || doCond2Disc))
#            {
#                call$strip <-
#                    quote(strip.custom(strip.levels=TRUE, strip.names=FALSE))
#            }

            ## construct plot title
            ## (not for hypervariate plots, not for mosaic plots)
            if ((!is.null(xvar) || !is.null(yvar)) &&
                ((xIsCat && yIsCat) == FALSE))
            {
                title <- paste(c(if (!is.null(zvar)) zvarStr,
                                 if (!is.null(yvar)) yvarStr,
                                 if (!is.null(xvar)) xvarStr),
                               collapse=" vs ")
                if (!is.null(zvar))
                    title <- paste(zvarStr, "vs", xvarStr,
                                   "and", yvarStr)
                if (is.null(xvar) || is.null(yvar))
                    title <- paste("Distribution of", title)
                byStr <- paste(c(if (!is.null(cond)) condStr,
                                 if (!is.null(cond2)) cond2Str,
                                 if (!is.null(groups)) groupsStr),
                               collapse=" and ")
                if (nchar(byStr) > 0)
                    title <- paste(title, byStr, sep=" by ")
                call$main <- title
            }

            ## axis labels (not for categoricals)
            if (!is.null(xvar) && !xIsCat)
                call$xlab <- xvarStr
            if (!is.null(yvar) && !yIsCat)
                call$ylab <- yvarStr

            MANY <- latticist.getOption("MANY")
            VERYMANY <- latticist.getOption("VERYMANY")

            ## choose plot type and formula
            if (is.null(xvar) && is.null(yvar)) {
                ## HYPERVARIATE

                dat.expr <- datArg
                if (!is.null(varSubset))
                    dat.expr <- call("[", datArg, varSubset)
                dat.form <- call("~", dat.expr)
                if (!is.null(conds))
                    dat.form <- call("~", call("|", dat.expr, conds))

                if (defaultPlot == "marginal.plot") {
                    call[[1]] <- quote(marginal.plot)
                    call[[2]] <- dat.expr
                    ## discretise groups if necessary
                    if (groupsIsNum) {
                        groups <- call("cutEq", groups, nLevels)
                        groupsVal <- eval(groups, dat, enclos)
                        groupsIsCat <- TRUE
                        call$groups <- groups
                    }
                    ## TODO: set plot.points according to nPointsPerPanel

                    if (nPointsPerPanel >= VERYMANY) {
                        call$plot.points <- FALSE
                    } else if (nPointsPerPanel >= MANY) {
                        call$plot.points <- "jitter"
                        call$pch <- "+" ## like jittered rug
                    }
                    ## TODO: set scales$cex / scales$rot according to levels

                    call$reorder <- FALSE
                    if (doLines) {
                        if (!is.null(groups))
                            call$type <- c("p", "l")
                        else
                            call$type <- c("p", "h")
                    } else {
                        call$type <- "p"
                    }

                } else if (defaultPlot == "splom") {
                    if (is.table(dat)) {
                        call[[1]] <- quote(pairs)
                        call[[2]] <- dat.expr
                        call$diag_panel <-
                            quote(pairs_diagonal_text(distribute = "margin"))
                        call$lower_panel_args <-
                            list(shade = TRUE)
                        call$labeling_args <-
                            list(abbreviate = 5)
                        call$labeling_args <-
                            list(rep = FALSE)
                        call$groups <- NULL

                    } else {
                        ## data.frame
                        call[[1]] <- quote(splom)
                        call[[2]] <- dat.form
                        ## support color covariate
                        if (groupsIsNum) {
                            call$groups <-
                                call("n.level.colors", groups)
                            call$panel <-
                                function(..., col, pch, groups, subscripts)
                                {
                                    col <- groups[subscripts]
                                    try(panel.xyplot(..., col = col, pch = 15,
                                        subscripts = subscripts), silent = TRUE)
                                }
                            call$legend <-
                                call("simpleColorKey",
                                     call("with", datArg, groups))
                        } else {
                            ## TODO: remove braces (need to fix deparseOneLine to handle it)
                            call$panel <-
                                function(...) { try(panel.xyplot(...), silent = TRUE) }
                        }
                        if (doLines) {
                            xyType <- c("p", latticist.getOption("xyLineType"))
                            call$type <- xyType
                        }
                        call$lower.panel <- quote(expression) #function(...) { NULL }
                        call$varname.cex <- 0.7
                        call$pscales <- 0
                        call["xlab"] <- list(NULL)
                    }

                } else if (defaultPlot == "parallel") {
                    if (is.table(dat)) {
                        ## "parallel" actually means a table view here
                        call[[1]] <- quote(barchart)
                        call[[2]] <- datArg
                        call$data <- NULL
                        call$groups <- FALSE
                        #if (is.null(x.relation))
                        #    x.relation <- "free"
                        ## which of the dimensions to use for groups
                        i.groups <- NA
                        if (!is.null(groups)) {
                            i.groups <- match(deparse1(groups),
                                              names(dimnames(dat)))
                        }
                        ## which of the dimensions to put on the axis?
                        ## choose the one with most levels (space efficient)
                        ## or next-most levels if that is the grouping var
                        i.axis <- which.max(dim(dat))
                        if (identical(i.axis, i.groups)) {
                            tmp <- dim(dat)
                            tmp[i.groups] <- 0
                            i.axis <- which.max(tmp)
                        }
                        ii <- c(i.axis, if (!is.na(i.groups)) i.groups)
                        condPerm <- seq_along(dim(dat))[-ii]
                        perm <- c(i.axis, condPerm,
                                  if (!is.na(i.groups)) i.groups)
                        call[[2]] <- call("aperm", datArg, perm)
                        if (!is.null(groups)) {
                            call$groups <- TRUE
                            call$stack <- TRUE
                        }
                        ## work out whether to put varnames in strips
                        ## (if too many panels, strips get overfull)
                        condDims <- dim(dat)[condPerm]
                        if (prod(head(condDims, 2)) <= 9) {
                            call$strip <-
                                quote(strip.custom(strip.names = TRUE))
                        }
                        call$ylab <- names(dimnames(dat))[i.axis]
                        call["xlab"] <- list(NULL)
                        call$as.table <- TRUE
                        call$subscripts <- TRUE
                    } else {
                        ## data.frame -- parallel plot
                        call[[1]] <- quote(parallel)
                        call[[2]] <- dat.form
                        if (is.null(groups)) {
                            call$col <- quote(trellis.par.get("plot.line")$col)
                        }
                        if (groupsIsNum) {
                            call$groups <-
                                call("n.level.colors", groups)
                            call$panel <-
                                function(..., col, groups, subscripts)
                                {
                                    col <- groups[subscripts]
                                    panel.parallel(..., col = col,
                                                   subscripts = subscripts)
                                }
                            call$legend <-
                                call("simpleColorKey",
                                     call("with", datArg, groups))
                            call$main <-
                                paste("Parallel plot grouped by",
                                      deparse1(groups))
                        }
                    }
                }

            } else if (is.null(xvar) || is.null(yvar)) {
                ## UNIVARIATE

                ## discretise groups if necessary
                if (groupsIsNum) {
                    groups <- call("cutEq", groups, nLevels)
                    groupsVal <- eval(groups, dat, enclos)
                    groupsIsCat <- TRUE
                    call$groups <- groups
                }

                if (xIsCat || yIsCat) {
                    ## UNIVARIATE CATEGORICAL
                    if (!is.null(yvar)) {
                        ## data on y axis, use dotplot
                        call[[1]] <- quote(dotplot)
                        call$data <- NULL
                        call$subset <- NULL
                        condOK <- cond
                        if (identical(yvar, cond)) {
                            condOK <- call("I", cond)
                        }
                        form <- paste(c(deparse1(yvar),
                                        if (!is.null(cond)) deparse1(condOK),
                                        if (!is.null(cond2)) deparse1(cond2),
                                        if (!is.null(groups)) deparse1(groups)),
                                      collapse=" + ")
                        form <- paste("~", form)
                        if (is.table(dat))
                            form <- paste("Freq", form)
                        tabcall <-
                            call("xtabs", as.formula(form), datArg)
                        tabcall$subset <- if (!isTRUE(subset)) subset
                        call[[2]] <- tabcall
                        ## and set logical `groups` argument
                        call$groups <- !is.null(groups)

                        if (doLines) {
                            if (!is.null(groups))
                                call$type <- c("p", "l")
                            else
                                call$type <- c("p", "h")
                            call$origin <- 0
                        }

                    } else {
                        ## data on x axis, use barchart
                        ## (just for variety? & dotplot.table has no horizontal=FALSE)
                        ## BUT if x is a discretized numeric, use histogram
                        if (doXDisc && is.null(groups)) {
                            xvar <- xvar[[2]] ## undo disc function
                            call[[1]] <- quote(histogram)
                            if (!is.null(conds))
                                call[[2]] <- call("~", call("|", xvar, conds))
                            else
                                call[[2]] <- call("~", xvar)
                        } else {
                            call[[1]] <- quote(barchart)
                            call$data <- NULL
                            call$subset <- NULL
                            condOK <- cond
                            if (identical(xvar, cond)) {
                                condOK <- call("I", cond)
                            }
                            form <- paste(c(deparse1(xvar),
                                            if (!is.null(cond)) deparse1(condOK),
                                            if (!is.null(cond2)) deparse1(cond2),
                                            if (!is.null(groups)) deparse1(groups)),
                                          collapse=" + ")
                            form <- paste("~", form)
                            if (is.table(dat))
                                form <- paste("Freq", form)
                            tabcall <-
                                call("xtabs", as.formula(form), datArg)
                            tabcall$subset <- if (!isTRUE(subset)) subset
                            call[[2]] <- tabcall
                            ## and set logical `groups` argument
                            call$groups <- !is.null(groups)
                            ## TODO: make stack an option?
                            call$stack <- TRUE
                            call$horizontal <- FALSE
                        }
                    }

                } else {
                    ## UNIVARIATE NUMERIC
                    if (!is.null(xvar)) {
                        ## data on x axis, use densityplot
                        call[[1]] <- quote(densityplot)
                        if (!is.null(conds))
                            call[[2]] <- call("~", call("|", xvar, conds))
                        else
                            call[[2]] <- call("~", xvar)
                        ## settings depend on number of points, groups
                        if (nPointsPerPanel >= VERYMANY) {
                            call$plot.points <- FALSE
                        } else if (nPointsPerPanel >= MANY) {
                            call$plot.points <- "jitter"
                            call$pch <- "+" ## like jittered rug
                        }
                        call$ref <- TRUE
                    } else {
                        ## data on y axis, use qqmath
                        call[[1]] <- quote(qqmath)
                        if (!is.null(conds))
                            call[[2]] <- call("~", call("|", yvar, conds))
                        else
                            call[[2]] <- call("~", yvar)
                        ## settings depend on number of points, groups
                        type <- "p"
                        if (nPointsPerPanel >= VERYMANY) {
                            call$f.value <- quote(ppoints(100))
                            if (doLines) type <- "l"
                        } else {
                            if (doLines) type <- "o"
                        }
                        ## a grid is always useful with qqmath
                        call$type <- c("g", type)
                        ## decide when to use normal vs uniform etc
                        tailtest <- function(x) {
                            qs <- quantile(x, c(0.01, 0.05, 0.95, 0.99), na.rm=TRUE)
                            diff(qs[c(1,4)]) / diff(qs[2:3])
                        }
                        ## expected with uniform is 1.1
                        ## expected with normal is 1.4
                        ## expected with lognormal is 2.0
                        tst <- tailtest(yVal)
                        if (tst >= 1.9) {
                            ## more skewed than normal
                            ## TODO: this is probably just confusing...
                            call$distribution <- quote(qlnorm)
                            call$xlab <- "Log-normal quantiles"
                        } else if (tst <= 1.2) {
                            ## uniform distribution
                            call$distribution <- quote(qunif)
                            call$xlab <- expression("Proportion" <= y)
                        } else {
                            call$distribution <- quote(qnorm)
                            call$xlab <- "Probability (normal distribution)"
                            ## label probabilites on axis
                            ## TODO: axis.components function to do this
                            probs <- c(0.001, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.999)
                            scales$x$at <- bquote(qnorm(.(probs)))
                            scales$x$labels <- probs
                        }
                        call$prepanel <- quote(prepanel.qqmathline)
                    }
                }

            } else if (xIsCat && yIsCat && (is.null(zvar) || zIsCat)) {
                ## BIVARIATE CATEGORICAL OR TRIVARIATE CATEGORICAL

                ## MOSAIC PLOT
                if (doSeparateStrata && !is.null(conds)) {
                    call[[1]] <- quote(cotabplot)
                    call$main <- NULL ## would apply to each panel!
                    call$sub <- NULL
                } else {
                    call[[1]] <- quote(mosaic)
                }
                ## discretise groups if necessary
                if (groupsIsNum) {
                    groups <- call("cut", groups, nLevels)
                    groupsVal <- eval(groups, dat, enclos)
                    groupsIsCat <- TRUE
                    call$groups <- groups
                }
                ## `form` is for mosaic itself, with conditioning syntax
                ## `subform` is for xtabs (excludes any panel-type conditioning)
                form <- paste(c(deparse1(yvar),
                                deparse1(xvar),
                                if (!is.null(zvar)) deparse1(zvar),
                                if (!is.null(groups)) deparse1(groups)),
                              collapse=" + ")
                subform <- form
                if (!is.null(conds)) {
                    form <- paste(form, "|", deparse1(conds))
                    if (doSeparateStrata == FALSE) {
                        subform <- paste(deparse1(conds), "+", subform)
                    }
                }
                subform <- paste("~", subform)
                form <- paste("~", form)
                call[[2]] <- as.formula(form)
                if (!is.null(groups)) {
                    call$groups <- NULL
                    call$highlighting <- deparse1(groups)
                }
                if (is.null(groups)) {
                    call$shade <- TRUE
                }
                labeling_args <- list()
                ## construct the table that will be plotted (for calcs)
                if (is.table(dat)) subform <- paste("Freq", subform)
                theTab <- xtabs(as.formula(subform), data = dat)
                overlapInfo <- function(vals, labs) {
                    if (length(labs) <= 1) return(NULL)
                    props <- as.vector(vals) / sum(vals)
                    midp <- cumsum(props) - props/2
                    ## labels may be abbreviated to 6 chars
                    chars <- nchar(labs)
                    charsAbrv6 <- pmin(6, chars)
                    charsAbrv4 <- pmin(4, chars)
                    overlapFn <- function(chars, charSpace) {
                        startpos <- (midp * charSpace) - chars/2
                        endpos <- (midp * charSpace) + chars/2
                        return(any(endpos[-length(chars)] > startpos[-1]))
                    }
                    ## assume each side of the mosaic can fit ~40 chars
                    ## if there is no overlap, no problem.
                    if (!overlapFn(chars, charSpace = 40))
                        return(NULL)
                    ## there is an overlap; let's try abbreviating the text
                    if (!overlapFn(charsAbrv6, charSpace = 40))
                        return("abrv6")
                    ## that didn't work, see if we can scale to fit ~48
                    if (!overlapFn(chars, charSpace = 48))
                        return("scale")
                    ## or both
                    if (!overlapFn(charsAbrv6, charSpace = 48))
                        return(c("scale", "abrv6"))
                    ## try abbreviating to 4 chars
                    if (!overlapFn(charsAbrv4, charSpace = 48))
                        return(c("scale", "abrv4"))
                    ## only hope is to rotate the text
                    return(c("rotate", "abrv6", "scale"))
                }
                ## TODO: account for gaps in conditioned plots?
                overlapList <- list()
                overlapList$left <-
                    overlapInfo(vals = margin.table(theTab, 1),
                                labs = dimnames(theTab)[[1]])
                overlapList$top <-
                    overlapInfo(vals = margin.table(theTab, 1:2)[1,],
                                labs = dimnames(theTab)[[2]])
                if (length(dim(theTab)) >= 3) {
                    overlapList$right <-
                        overlapInfo(vals = margin.table(theTab, 3:1)[,dim(theTab)[2],],
                                    labs = rep(dimnames(theTab)[[3]], dim(theTab)[1]))
                }
                if (length(dim(theTab)) >= 4) {
                    overlapList$bottom <-
                        overlapInfo(vals = margin.table(theTab, c(4,2,1))[,,dim(theTab)[1] ],
                                    labs = rep(dimnames(theTab)[[4]], dim(theTab)[2]))
                }
                ## construct list of labelling args
                labargCall <- call("list")
                if ("scale" %in% unlist(overlapList)) {
                    scaleToSize <- 10
                    if (is.call.to(call, "cotabplot"))
                        scaleToSize <- 9
                    labargCall$gp_labels <-
                        call("gpar", fontsize = scaleToSize)
                }
                marginsCall <- call("c", 0)
                marginsCall$left <- 3
                marginsCall$top <- 3
                if (length(dim(theTab)) >= 3)
                    marginsCall$right <- 3
                if (length(dim(theTab)) >= 4)
                    marginsCall <- call("c")
                if (any(c("abrv6", "abrv4") %in% unlist(overlapList))) {
                    abrvCall <- call("c")
                    for (space in c("left", "top", "right", "bottom")) {
                        if ("abrv6" %in% overlapList[[space]])
                            abrvCall[space] <- list(6)
                        if ("abrv4" %in% overlapList[[space]])
                            abrvCall[space] <- list(4)
                    }
                    labargCall$abbreviate <- abrvCall
                }
                if ("rotate" %in% unlist(overlapList)) {
                    rotCall <- call("c")
                    justCall <- call("c")
                    offsetCall <- call("c")
                    if ("rotate" %in% overlapList$left) {
                        rotCall$left <- 0
                                        #justCall$left <- "right"
                        offsetCall$left <- 1.8
                        marginsCall$left <- 4.8
                    }
                    if ("rotate" %in% overlapList$top) {
                        rotCall$top <- 30
                                        #justCall$top <- "left"
                        offsetCall$top <- 1
                        marginsCall$top <- 4
                    }
                    if ("rotate" %in% overlapList$right) {
                        rotCall$right <- 0
                                        #justCall$right <- "left"
                        offsetCall$right <- 1.8
                        marginsCall$right <- 4.8
                    }
                    if ("rotate" %in% overlapList$bottom) {
                        rotCall$bottom <- 30
                        offsetCall$bottom <- 1
                        marginsCall$bottom <- 4
                    }
                    labargCall$rot_labels <- rotCall
                    labargCall$just_labels <- justCall
                    labargCall$offset_varnames <- offsetCall
                }
                if (is.call.to(call, "cotabplot")) {
                    marginsCall <- 0.5
                    if (isTRUE(call$shade))
                        call$legend <- FALSE
                }
                call$labeling_args <- labargCall
                call$margins <- marginsCall
                call$keep_aspect_ratio <- identical(aspect, "iso")

            } else if (is.null(zvar)) {
                ## BIVARIATE, WITH AT LEAST ONE NUMERIC

                if (yIsCat || xIsCat) {
                    ## BIVARIATE CATEGORICAL AND NUMERIC

                    ## TODO: if only one value for each level use dotplot

                    if (is.logical(xVal))
                        call$horizontal <- FALSE

                    ## reorder factor levels if more than 2
                    if (is.categorical(yVal) && isUnordered(yvar, yVal)) {
                        if (nlevels(yVal) > 2) {
                            yvar <- call("reorder", yvar, xvar, na.rm=T)
                            yVal <- eval(yvar, dat, enclos)
                        }
                    }
                    if (is.categorical(xVal) && isUnordered(xvar, xVal)) {
                        if (nlevels(xVal) > 2) {
                            xvar <- call("reorder", xvar, yvar, na.rm=T)
                            xVal <- eval(xvar, dat, enclos)
                        }
                    }
                    ## formula
                    if (!is.null(conds))
                        call[[2]] <- call("~", yvar, call("|", xvar, conds))
                    else
                        call[[2]] <- call("~", yvar, xvar)

                    if (!is.null(groups)) {
                        call[[1]] <- quote(stripplot)
                        call$jitter.data <- TRUE
                        if (doLines) {
                            call$type <- c("p", "a")
                            ## TODO: check whether there are any missing values
                                        #call$fun <- quote(median)
                            call$fun <- quote(function(x) { median(x, na.rm=TRUE) })
                        }
                        ## support color covariate
                        if (groupsIsNum) {
                            call$groups <-
                                call("n.level.colors", groups)
                            call$panel <-
                                function(..., col, fill, pch, groups, subscripts)
                                {
                                    fill <- groups[subscripts]
                                    panel.stripplot(..., col = "#00000044", fill = fill, #
                                                    pch = 21, subscripts = subscripts)
                                }
                            call$legend <-
                                call("simpleColorKey",
                                     call("with", datArg, groups))
                        }

                    } else {
                        call[[1]] <- quote(bwplot)
                                        #call$origin <- 0
                        call$varwidth <- FALSE
                    }

                } else {
                    ## BIVARIATE NUMERIC
                    call[[1]] <- quote(xyplot)
                    if (!is.null(conds))
                        call[[2]] <- call("~", yvar, call("|", xvar, conds))
                    else
                        call[[2]] <- call("~", yvar, xvar)

                    if (doHexbin && require("hexbin")) {
                        call[[1]] <- quote(hexbinplot)
                        type <- if (doLines) "r"
                        if (!is.null(conds)) type <- c("g", type)
                        call$type <- type
                        if (is.null(aspect))
                            aspect <- 1
                        ## ignore groups setting
                        call$groups <- NULL

                    } else {

                        if (groupsIsNum) {
                            if (doTile) {
                                call[[1]] <- quote(tileplot)
                                if (require("tripack", quietly = TRUE))
                                    call$use.tripack <- TRUE
                            } else {
                                call[[1]] <- quote(levelplot)
                                call$panel <- quote(panel.levelplot.points)
                                call$prepanel <- quote(prepanel.default.xyplot)
                            }
                            form <- call("~", groups, call("*", xvar, yvar))
                            if (!is.null(conds))
                                form[[3]] <- call("|", form[[3]], conds)
                            call[[2]] <- form
                        }

                        if (doLines) {
                            ## work out whether x data (in each panel) is spaced out
                            ## enough to join by lines (rather than smoothing, below)
                            guessPanelType <- function(panelx) {
                                ans <- list(lines=TRUE, jitter=FALSE)
                                if (!any(is.finite(panelx)))
                                    return(ans)
                                rge <- range(panelx, na.rm=TRUE)
                                diffs <- diff(sort(panelx))
                                diffr <- range(diffs)
                                ans$lines <- FALSE
                                if (min(diffr) == 0) {
                                    ans$jitter <- TRUE
                                    posdiff <- min(diffs[diffs > 0])
                                    ## join averages by lines if few discrete values
                                    if (posdiff > diff(rge) / 30) {
                                        ans$lines <- TRUE
                                    }
                                } else {
                                    if ((max(diffr) - min(diffr) < getOption("ts.eps")) &&
                                        (min(diffr) > 2 * getOption("ts.eps"))) {
                                        ## regular differences -- likely time series
                                        ans$lines <- TRUE
                                    } else {
                                        if (min(diffr) > diff(rge) / 100) {
                                            ans$lines <- TRUE
                                        }
                                    }
                                }
                                ans
                            }
                            if (is.null(conds) && is.null(groups)) {
                                panelType <- guessPanelType(xVal[subsetVal])
                            } else {
                                ## split into packets by conditioning and/or groups
                                condList <- list()
                                if (!is.null(cond)) {
                                    condList$cond <- condVal
                                    if (!is.factor(condVal) && !is.shingle(condVal))
                                        condList$cond <- as.factorOrShingle(condVal)
                                }
                                if (!is.null(cond2)) {
                                    condList$cond2 <- cond2Val
                                    if (!is.factor(cond2Val) && !is.shingle(cond2Val))
                                        condList$cond2 <- as.factorOrShingle(cond2Val)
                                }
                                if (!is.null(groups)) {
                                    condList$groups <- groupsVal
                                    if (!is.factor(groupsVal) && !is.shingle(groupsVal))
                                        condList$groups <- as.factorOrShingle(groupsVal)
                                }
                                if (!isTRUE(subset))
                                    condList <- lapply(condList,
                                                       function(v) v[subsetVal, drop=TRUE])
                                nlevsList <- lapply(condList, nlevels)
                                packetDefs <- do.call(expand.grid, lapply(nlevsList, seq_len))
                                ## make a list for each packet (combination of cond levels)
                                packetDefs <- as.data.frame(t(packetDefs))
                                xSubVal <- if (isTRUE(subset)) xVal else xVal[subsetVal]
                                panelType <- sapply(packetDefs, function(packLev) {
                                    id <- lattice:::compute.packet(condList, levels=packLev)
                                    unlist(guessPanelType(xSubVal[id]))
                                })
                                panelType <- as.data.frame(t(panelType))
                            }

                            if (any(panelType$jitter)) {
                                call$jitter.x <- TRUE
                                if (all(panelType$lines))
                                    call$type <- c("p", "a")
                            } else {
                                if (all(panelType$lines))
                                    call$type <- "o"
                            }

                            if (any(panelType$lines == FALSE)) {
                                ## use loess smoother
                                call$type <- c("p", latticist.getOption("xyLineType"))
                                call$prepanel <- quote(try.prepanel.loess)
                                ## do not worry about errors in loess.smooth
                                call$plot.args <- quote(list(panel.error = "warning"))
                            }
                        }
                    }
                }
            } else {
                ## TRIVARIATE

                if (doSegments) {
                    ## SEGMENTS
                    ## use segplot
                    call[[1]] <- quote(segplot)
                    if (xIsCat && !yIsCat) {
                        ## switch x and y, so categorical is on y axis
                        oldx <- xvar
                        xvar <- yvar
                        yvar <- oldx
                        oldx <- xVal
                        xVal <- yVal
                        yVal <- oldx
                        xIsCat <- FALSE
                        yIsCat <- TRUE
                    }
                    form <- call("~", yvar, call("+", xvar, zvar))
                    call$xlab <- paste(deparse1(xvar), "--",
                                       deparse1(zvar))
                    if (doAsError) {
                        ## symmetric additive error form
                        ## I(x-z) + I(x+z)
                        form[[3]] <- call("+", call("I", call("-", xvar, zvar)),
                                          call("I", call("+", xvar, zvar)))
                        call$centers <- xvar
                        call$draw.bands <- FALSE
                        call$xlab <- paste(deparse1(xvar), "+/-",
                                           deparse1(zvar))
                    }
                    if (!is.null(conds))
                        form[[3]] <- call("|", form[[3]], conds)
                    call[[2]] <- form
                    ## colors coded by "level"
                    call$level <- groups
                    call$groups <- NULL
                    mainStr <- paste(deparse1(xvar), "&", deparse1(zvar),
                                     "vs", deparse1(yvar))
                    if (!is.null(groups))
                        mainStr <- paste(mainStr, "by", deparse1(groups))
                    call$main <- mainStr

                } else {
                    ## TRIVARIATE 3D
                    ## use cloud
                    call[[1]] <- quote(cloud)

                    ## 3D NUMERIC (3D SCATTER)
                    form <- call("~", zvar, call("*", xvar, yvar))
                    if (!is.null(conds))
                        form[[3]] <- call("|", form[[3]], conds)
                    call[[2]] <- form
                    if (doLines)
                        call$type <- c("p", "h")
                    ## support color covariate
                    if (groupsIsNum) {
                        call$groups <-
                            call("n.level.colors", groups)
                        call$panel <-
                            function(..., col, pch, groups, subscripts)
                            {
                                col <- groups[subscripts]
                                panel.cloud(..., col = col, pch = 16,
                                            subscripts = subscripts)
                            }
                        call$legend <-
                            call("simpleColorKey",
                                 call("with", datArg, groups))
                    }
                }
            }

            ## generic stuff...

            isVCD <- (is.call.to(call, "mosaic") ||
                      is.call.to(call, "cotabplot") ||
                      is.call.to(call, "pairs"))

            ## aspect and scales

            if (is.call.to(call, "cloud")) {
                ## for 3D plots, aspect widget applies to "panel.aspect".
                ## set panel.aspect to "fill" by default if only one panel
                if (is.null(aspect) && is.null(conds))
                    aspect <- "fill"
                if (identical(eval(aspect), "fill"))
                    aspect <- round(dev.size()[2] / dev.size()[1], 2)
                if (is.numeric(aspect))
                    call$panel.aspect <- aspect
            } else {
                call$aspect <- aspect ## may be NULL
            }

            if (!is.null(x.relation) || !is.null(y.relation)) {
                ## either of these may be NULL
                scales$x$relation <- x.relation
                scales$y$relation <- y.relation
            }

            anyNumerics <- ((!is.null(xvar) && !xIsCat) ||
                            (!is.null(yvar) && !yIsCat) ||
                            (!is.null(zvar) && !zIsCat) ||
                            is.call.to(call, "splom") ||
                            is.call.to(call, "parallel"))
            ## style settings for points
            if (anyNumerics) {
                style.3panels <- latticist.getOption("style.3panels")
                style.7panels <- latticist.getOption("style.7panels")
                style.MANY <- latticist.getOption("style.MANY")
                style.VMANY <- latticist.getOption("style.VERYMANY")
                ## modify styles for line-type plots
                linestyle.MANY <- style.MANY
                linestyle.VMANY <- style.VMANY
                linestyle.MANY$alpha.line <- style.MANY$alpha.points
                linestyle.VMANY$alpha.line <- style.VMANY$alpha.points
                linestyle.MANY$alpha.points <- NULL
                linestyle.VMANY$alpha.points <- NULL
                ## construct style for current plot
                theme <- list()
                if (nCondLevels >= 3)
                    theme <- modifyList(theme, style.3panels)
                if ((nCondLevels >= 7) ||
                    is.call.to(call, "splom"))
                    theme <- modifyList(theme, style.7panels)
                ## if "MANY" points, use a different plot style
                ## (but skip this if using f.value argument to qqmath)
                callName <- toString(call[[1]])
                if ((nPointsPerPanel >= MANY) && is.null(call$f.value))
                {
                    if (callName %in% c("parallel", "segplot")) {
                        theme <- modifyList(theme, linestyle.MANY)
                    } else if (callName %in%
                               c("xyplot", "stripplot", "bwplot", "qqmath",
                                 "levelplot", "cloud", "splom")) {
                        theme <- modifyList(theme, style.MANY)
                    }
                }
                if ((nPointsPerPanel >= VERYMANY) && is.null(call$f.value))
                {
                    if (callName %in% c("parallel", "segplot")) {
                        theme <- modifyList(theme, linestyle.VMANY)
                    } else if (callName %in%
                               c("xyplot", "stripplot", "bwplot", "qqmath",
                                 "levelplot", "cloud", "splom")) {
                        theme <- modifyList(theme, style.VMANY)
                    }
                }
                call$par.settings <- quote(simpleTheme())
                if (length(theme) > 0)
                    call$par.settings <-
                        as.call(c(list(quote(simpleTheme)), theme))
            }
            ## add a grid if there are multiple panels
            if (anyNumerics && !is.null(conds)) {
                type <- call$type
                if (is.null(type)) type <- "p" ## assumed
                type <- unique(c("g", type))
                call$type <- type
            }

            ## set up key
            if (!isVCD && !is.null(groups) && groupsIsCat) {
                auto.key <- list()
                ## work out key type
                typeVal <- call$type
                if (all(c("p", "l") %in% typeVal)) {
                    typeVal <- c(setdiff(typeVal, c("p", "l")), "o")
                }
                if (is.call.to(call, "marginal.plot"))
                    typeVal <- c("p", "l")
                if (is.call.to(call, "parallel"))
                    typeVal <- "l"
                ## all type values other than "p" and "g" imply lines
                if (any(typeVal %in% c("p", "g") == FALSE)) {
                    auto.key$lines <- TRUE
                    if (any(c("o", "b") %in% typeVal))
                        auto.key$type <- "o"
                    if (("p" %in% typeVal) == FALSE)
                        auto.key$points <- FALSE
                }
                ## get group levels that will appear in key
                levs <- levelsOK(groupsVal)
                ## if groups are discretised, or hypervar, need key title
                if (groupsWasCat || (is.null(xvar) && is.null(yvar))) {
                    auto.key$title <- groupsStr
                    auto.key$cex.title <- 1
                }
                n.items <- length(levs)
                if (n.items > 1 && n.items <= 4)
                    auto.key$columns <- n.items
                if (n.items > 4)
                    auto.key$space <- "right"
                if (n.items == 4)
                    auto.key$between.columns <- 1
                if (n.items >= 3)
                    auto.key$cex <- 0.7
                ## TODO: check lengths of text, abbreviate?
                call$auto.key <- auto.key
            }

            ## fix up long x axis labels
            if (xIsCat && !isVCD && !is.call.to(call, "histogram"))
            {
                if (nlevels(xVal) >= 4) {
                    if (max(sapply(levelsOK(xVal), nchar)) >= 12) {
                        scales$x$rot <- 30
                        scales$x$cex <- 0.7
                                        #scales$x$abbreviate <- TRUE
                    } else
                    if ((nlevels(xVal) >= 8) ||
                        (mean(sapply(levelsOK(xVal), nchar)) >= 8)) {
                        scales$x$rot <- 60
                    }
                }
            }

            ## sub-title
            sub.func <- latticist.getOption("sub.func")
            if (isTRUE(latticist.getOption("add.sub")) &&
                !is.null(sub.func))
            {
                subt <- sub.func
                if (is.function(sub.func))
                    subt <- sub.func(spec = spec, nPoints = nPoints)
                if (isVCD) {
                    if (!is.call.to(call, "cotabplot")) {
                        call$sub <- subt
                        call$sub_gp <-
                            call("gpar", cex = 0.7)
                    }
                } else {
                    call$sub <- list(subt, x = 0.99, just="right",
                                     cex = 0.7, font = 1)
                }
            }

            if (!isVCD) {
                if (!is.null(conds))
                    call$subscripts <- TRUE

                ## convert nested list 'scales' to quoted argument
                if (length(scales) > 0) {
                    tmp <- try( parse(text = deparse(scales,
                                      control = NULL))[[1]] )
                    if (!inherits(tmp, "try-error"))
                        call$scales <- tmp
                }

                ## layout
                if (tooManyPanels)
                    call$layout <- c(0, min(MAXPANELS,
                                            ceiling(nCondLevels / 2)))

                ## useOuterStrips with bivar conditioning
                ## (does not work with 'layout')
                if (!is.null(cond) && !is.null(cond2) && !tooManyPanels) {
                    call <- call("useOuterStrips", call)
                }
            }

            call
        }
    do.call("doCompose", spec)
}

is.call.to <- function(x, name)
    is.call(x) && identical(x[[1]], as.symbol(name))

is.categorical <- function(x)
    is.factor(x) || is.shingle(x) || is.character(x) || is.logical(x)

levelsOK <- function(x) {
    if (is.logical(x)) return(c(TRUE, FALSE))
    levels(x)
}

reorderByFreq <- function(x) {
    reorder(x, x, function(z) -length(z))
}

cutEq <- function(x, n, type=2, dig.lab=4, ...)
{
    stopifnot(length(n) == 1)
    br <- quantile(x, seq(0, 1, length=n+1), type=type,
                   na.rm=TRUE, names=FALSE)
    br[length(br)] <- max(x, na.rm=TRUE)
    br <- unique(br)
    cut(x, br, dig.lab=dig.lab, right=FALSE,
        include.lowest=TRUE, ordered_result=TRUE)
}

try.prepanel.loess <- function(...) {
    result <- try(prepanel.loess(...), silent=TRUE)
    if (inherits(result, "try-error"))
        return(list())
    result
}

n.level.colors <-
    function(x, n.col = 50,
             at = do.breaks(range(x, finite = TRUE), n.col),
             ...)
{
    level.colors(x, at = at, ...)
}

simpleColorKey <-
    function(x, n.col = 50,
             at = do.breaks(range(x, finite = TRUE), n.col),
             ..., space = "right")
{
    foo <- list(space =
                list(fun = "draw.colorkey",
                     args = list(key = list(at = at, ...),
                                 draw = FALSE)))
    names(foo) <- space
    foo
}

