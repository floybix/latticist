## latticist: a Lattice-based exploratory visualisation GUI
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

## LICENSE
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version. See the file gpl-license.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

latticist <-
    function(dat,
             spec = list(),
             reorder.levels = !is.table(dat),
             ...,
             use.playwith = latticist.getOption("use.playwith"))
{
    datArg <- substitute(dat)
    datName <- toString(deparse(datArg), width = 30)

    use.playwith <- (use.playwith && require("playwith"))

    isOK <- is.data.frame(dat) || is.table(dat)
    makeLocalCopy <- (isTRUE(reorder.levels) || !isOK)

    if (makeLocalCopy) {

        if (!isOK)
            dat <- as.data.frame(dat)

        if (is.table(dat)) {
            if (reorder.levels)
                dat <- reorderTableByFreq(dat)

        } else {
            ## dat is a data.frame

            ## convert numerics with discrete values in {-1, 0, 1} to factors
            isnum <- sapply(dat, is.numeric)
            for (nm in names(dat)[isnum]) {
                dd <- dat[[nm]]
                ## test first 50 values first (quick check)
                vals <- unique(head(dd, n=50))
                if (all(vals[is.finite(vals)] %in% -1:1)) {
                    if (all(range(dd, na.rm=TRUE) %in% -1:1) &&
                        all(range(abs(diff(dd)), na.rm=TRUE) %in% 0:2))
                    {
                        dat[[nm]] <- factor(dd)
                    }
                }
            }

            if (reorder.levels) {
                iscat <- sapply(dat, is.categorical)
                for (nm in names(dat)[iscat]) {
                    val <- dat[[nm]]
                    if (is.character(val))
                        dat[[nm]] <- factor(val)
                    if (!is.ordered(val) &&
                        !is.shingle(val) &&
                        nlevels(val) > 1)
                    {
                        dat[[nm]] <- reorderByFreq(val)
                    }
                }
            }

        }

        ## refer to the local copy of dat
        if (is.symbol(datArg) && use.playwith) {
            datArgNm <- paste(as.character(datArg),
                              ".mod", sep = "")
            datArg <- as.symbol(datArgNm)
            assign(datArgNm, dat)
        } else {
            datArg <- quote(dat)
        }
    }

    if (use.playwith) {
        latticist_playwith(dat, spec = spec, datArg = datArg,
                           datName = datName, ...)

    } else {
        ## use gWidgets
        latticist_gWidgets(dat, spec = spec, datArg = datArg,
                           datName = datName, ...)
    }
}


reorderTableByFreq <- function(x)
{
    stopifnot(is.table(x))
    df <- as.data.frame(x)
    i <- which(names(df) == "Freq")
    df[-i] <- lapply(df[-i], reorder, - df$Freq)
    xtabs(Freq ~ ., df)
}

