\name{latticistCompose}
\alias{latticistCompose}
\alias{try.prepanel.loess}
\alias{n.level.colors}
\alias{simpleColorKey}
\title{ Latticist API }
\description{
  Convert a simple specification list into a Lattice plot call,
  or the inverse operation.
}
\usage{
latticistCompose(dat, spec = list(),
                 datArg = substitute(dat),
                 enclos = parent.frame())
}
\arguments{
  \item{dat}{
    a data frame (with numeric and/or categorical variables).
  }
  \item{spec}{
    a list specifying the latticist plot. See Details.
  }
  \item{datArg}{
    the symbol to use for \code{dat} in the generated call.
  }
  \item{enclos}{
    an \code{environment} to use as an enclosure around \code{dat} for
    evaluating expressions.
  }

}
\details{

  Elements of \code{spec} can include:
  \describe{
    \item{\code{xvar, yvar, zvar}}{
      variables (or expressions) for the x, y and z axes.
      If all are missing, a hypervariate plot is produced according to
      \code{defaultPlot} (below).
    }
    \item{\code{groups}}{
      a grouping variable or color-coding covariate (can be categorical
      or numeric).
    }
    \item{\code{cond, cond2}}{
      conditioning variables.
    }
    \item{\code{subset}}{
      subset expression.
    }
    \item{\code{varSubset}}{
      the subset of variables (from the data frame, \code{dat}) to
      include in hypervariate plots (see defaultPlot).
    }
    \item{\code{defaultPlot}}{
      one of "marginal.plot" (the default), "splom" or
      "parallel". Specifies the type of plot to produce when xvar and yvar
      are missing.
    }
    \item{etc}{ TODO... }
  }

  Note that xvar, yvar, zvar, groups, cond, cond2, subset must be
  character strings (or NULL), and will be \code{parse}d.
}
\value{
  \code{latticistCompose} returns a \code{\link{call}}.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\references{
  For an excellent introduction to and coverage of Lattice:

Sarkar, Deepayan (2008) "Lattice: Multivariate Data Visualization with
R", Springer. \url{http://lmdvr.r-forge.r-project.org/}
}
\seealso{
  \code{\link{latticist}},
  \link[lattice]{Lattice}
}
\examples{
latticistCompose(CO2)

latticistCompose(CO2, spec = list(xvar = "uptake"))

latticistCompose(CO2, spec = list(yvar = "uptake"))

latticistCompose(CO2, spec = list(yvar = "uptake", doYDisc = TRUE))
}
\keyword{ hplot }
