## latticist: a Lattice-based exploratory visualisation GUI
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

symbolName <- function(nm)
{
    unlist(lapply(nm, function(z)
                  deparse(as.symbol(z), backtick = TRUE)),
                  use.names = FALSE)
}

latticistInitOptions <- function(dat, datArg)
{
    stuff <- list()
    datNm <- toString(deparse(datArg, backtick = TRUE))

    if (is.table(dat)) {
        ## dat is a table
        stuff$varexprs <-
            c("", symbolName(names(dimnames(dat))))

        ## subsets
        ## preload factor levels (only most frequent two of each)
        dimn <- dimnames(dat)
        toplev <- lapply(names(dimn), function(nm) {
            tmp <- head(dimn[[nm]], 2)
            paste(symbolName(nm), "==", sapply(tmp, deparse))
        })
        stuff$subsetopts <-
            c("", unlist(toplev))

    } else {
        ## dat is a data.frame

        ## which variables are categorical (vs numeric)
        iscat <- sapply(dat, is.categorical)

        ## variables and expressions
        ## group into categorical vs numeric
        stuff$varexprs <-
            c("",
              symbolName(names(dat)[iscat]),
              if (any(iscat) && any(!iscat))
              "------------------",
              symbolName(names(dat)[!iscat]),
              "-------------------",
              sprintf("1:nrow(%s)", datNm))

        ## subsets
        ## preload factor levels (only first two of each)
        toplev <- lapply(names(dat)[iscat], function(nm) {
            nmOK <- symbolName(nm)
            if (is.factor(dat[[nm]])) {
                tmp <- head(levels(dat[[nm]]), 2)
                paste(nmOK, "==", sapply(tmp, deparse))
            } else if (is.logical(dat[[nm]])) {
                paste(nmOK, "==", c("TRUE", "FALSE"))
            } else {
                tmp <- names(sort(table(dat[[nm]]), decreasing=TRUE))
                tmp <- head(tmp, 2)
                paste(nmOK, "==", sapply(tmp, deparse))
            }
        })
        subsetopts <- c("", unlist(toplev))
        if (nrow(dat) > 1000) {
            ## a regular sample down by one order of magnitude
            subN <- 10 ^ (round(log10(nrow(dat))) - 1)
            subsetopts <-
                c(subsetopts,
                  "## regular subsample:",
                  sprintf("seq(1, nrow(%s), length = %i)",
                          datNm, subN))
        }
        subsetopts <-
                c(subsetopts,
                  "------------------")
        ## is.finite() of variables with missing values
        missings <- lapply(names(dat), function(nm) {
            nmOK <- symbolName(nm)
            if (any(is.na(dat[[nm]])))
                paste("!is.na(", nmOK, ")", sep="")
            else NULL
        })
        missings <- unlist(missings)
        if (length(missings) > 0) {
            subsetopts <- c(subsetopts,
                            sprintf("complete.cases(%s)", datNm),
                            missings)
        }
        stuff$subsetopts <- subsetopts
    }

    ## aspect
    stuff$aspectopts <-
        c("",
          '"fill"', '"iso"', '"xy"',
          '0.5', '1', '2')
    ## scales
    stuff$scalesopts <-
        c("",
          "x same, y same",
          "x same, y free",
          "x free, y same",
          "x free, y free",
          "------------------",
          "x sliced, y sliced",
          "x sliced, y same",
          "x sliced, y free",
          "x same, y sliced",
          "x free, y sliced")

    stuff
}
