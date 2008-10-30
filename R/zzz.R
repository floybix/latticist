## latticist: a Lattice-based exploratory visualisation GUI
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

## local environment to store user options
.LatticistEnv <- new.env()
.LatticistEnv$options <- list()

.onLoad <- function(libname, pkgname)
{
    latticist.options(.defaultLatticistOptions())
}
