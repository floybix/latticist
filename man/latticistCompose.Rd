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
      \code{defaultPlot} (see below).
      Note that x or y may be discretized by setting \code{doXDisc} or
      \code{doYDisc} (see below).
    }
    \item{\code{groups}}{
      a grouping variable or color-coding covariate. This can refer to
      either a categorical or numeric variable. Many plot types support
      continuous color covariates, in which case a colorkey will be
      drawn. For categorical groups a standard key will be drawn.
    }
    \item{\code{cond, cond2}}{
      conditioning variables. These will be turned into shingles if required,
      with \code{nLevels} distinct levels (see below).
    }
    \item{\code{subset}}{
      subset expression.
    }
    \item{\code{varSubset}}{
      a character vector, the subset of variables (from the data frame)
      to include in hypervariate plots.
    }
    \item{\code{defaultPlot}}{
      one of "marginal.plot", "splom" or "parallel". Specifies the type
      of plot to produce if \code{xvar} and \code{yvar} are
      missing. Note that \code{groups} is supported by all these plots,
      and \code{cond} is supported by "splom" and "parallel".

      NOTE: when the data is a \code{table}, "parallel" does
      not produce a \code{parallel} plot, but rather a stacked
      \code{barchart} of the table.
      Also when the data is a \code{table}, "splom" produces a
      \code{pairs} layout of \code{mosaic} plots.
    }
    \item{\code{aspect}}{
      panel aspect ratio, as a numeric value (= y / x), or
      one of the values "fill", "iso", "xy".
    }
    \item{\code{doLines}}{
      whether to add lines to relevant plot types (i.e. those involving
      numeric variables).
      For \code{dotplot} and \code{cloud} this refers to droplines (type
      \code{"h"}) (though if groups are defined in a \code{dotplot}, type
      \code{"l"} is used).
      In a grouped \code{stripplot}, group medians are joined (type
      \code{"a"}, \code{fun = median}).
      For \code{qqmath} the points are joined in order (type \code{"l"}).
      For \code{xyplot} and similar, the line type depends on the nature
      of the data; if the data x values form a regular sequence or are
      few in number they are simply joined (type \code{"l"});
      if duplicate x values are detected, with a reasonable number of
      unique values, their averages are joined (type \code{"a"});
      otherwise a smoothing line is added according to
      \code{latticist.getOption("xyLineType")}. This defaults to
      \code{"smooth"} (loess fit), but could reasonably be set to
      \code{"r"} (regression line) or \code{"a"} (joined averages).
    }
    \item{\code{doHexbin}}{
      whether to use \code{hexbinplot} rather than \code{xyplot} for
      bivariate numeric plots. These can be faster and more effective
      for large datasets. Note that groups are not supported.
    }
    \item{\code{doSegments}, \code{doAsError}}{
      if \code{doSegments} is TRUE when all of \code{xvar}, \code{yvar}
      and \code{zvar} are defined, a \code{segplot} is produced where
      the x values are joined to z values by horizontal
      segments. Alternatively, if \code{doAsError} is TRUE, segments are
      drawn from \code{(x - z)} to \code{(x + z)}, and each x point is
      marked, such that z acts as an error or range about x.
    }
    \item{\code{doTile}}{
      when drawing a bivariate numeric plot with a color-covariate
      (i.e. when \code{xvar}, \code{yvar} and \code{groups} are all
      numeric), this option will draw a \code{tileplot}, which draws a
      polygon enclosing each point. This may be appropriate when x and y
      are on the same scale.
    }
    \item{\code{doXDisc}, \code{doYDisc}}{
      set to discretize \code{xvar} and/or \code{yvar}, if they are
      numeric. Either \code{cut} or \code{equal.count} is used,
      depending on the plot type, with \code{nLevels} distinct levels.
    }
    \item{\code{nLevels}}{
      number of levels for discretizing \code{cond} and \code{cond2},
      optionally \code{xvar} or \code{yvar}, and in some cases
      \code{groups}. For shingles, the amount of overlap is taken from
      \code{latticist.getOption("shingle.overlap")}.
    }
    \item{\code{x.relation}, \code{y.relation}}{
      defines the scales in conditioned lattice plots. Can have
      values in "free", "same" or "sliced".
    }
    \item{\code{doSeparateStrata}}{
      when a \code{mosaic} plot is produced and \code{cond} or
      \code{cond2} are defined, this defines whether to separate the
      strata defined by conditioning variables into different panels
      (the default; uses \code{cotabplot}), or to include the
      conditioning variables in the one mosaic plot
      (\code{doSeparateStata = FALSE}).
    }
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

latticistCompose(CO2, spec = list(defaultPlot = "parallel"))

latticistCompose(CO2, spec = list(xvar = "uptake"))

latticistCompose(CO2, spec = list(yvar = "uptake"))

latticistCompose(CO2, spec = list(yvar = "uptake", doYDisc = TRUE))


}
\keyword{ hplot }
