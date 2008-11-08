\name{latticist.options}
\alias{latticist.options}
\alias{latticist.getOption}
\title{User default settings for latticist}
\description{
  A basic user settings facility, like \code{\link{options}} and
  \code{\link[lattice]{lattice.options}}.
}
\usage{
latticist.options(...)
latticist.getOption(name)
}
\arguments{
  \item{name}{ character giving the name of a setting. }
  \item{...}{ new options can be defined, or existing ones modified, using
          one or more arguments of the form 'name = value' or by
          passing a list of such tagged values.  Existing values can be
          retrieved by supplying the names (as character strings) of
          the components as unnamed arguments.
    }
}
\details{
  The available options and their current values can be seen with
  \code{str(latticist.options())}.

  The options are:

  \describe{
    \item{\code{use.playwith}}{
      default for the argument to \code{\link{latticist}}.
    }
    \item{\code{defaultPlot}}{
      see \code{\link{latticistCompose}}. Note, this value can be
      over-ridden by specifying \code{defaultPlot} in the \code{spec}
      argument.
    }
    \item{\code{xyLineType}}{
      line type for scatterplots; one (or more) of
      "smooth", "r" or "a".
    }
    \item{\code{add.sub}, \code{sub.func}}{
      set \code{add.sub} to FALSE to omit the subtitle. Set
      \code{sub.func} to a character value for the subtitle, or a
      function to generate the subtitle. It is passed arguments
      \code{spec} and \code{nPoints}.
    }
    \item{\code{MANY}, \code{VERYMANY}}{
      threshold number of data points (per panel, on average), at which
      to change the plot type to something more appropriate for large
      datasets.

      For \code{qqmath}, in the \code{VERYMANY} case, the \code{f.value}
      argument is used to approximate the distribution with 100 points.
      For \code{densityplot}, in the \code{VERYMANY} case, data points
      are not drawn.
    }
    \item{\code{style.MANY}, \code{style.3panels}, etc}{
      these lists are combined to form a list of arguments to
      \code{\link[lattice]{simpleTheme}}.

      First, if the number of panels is 3 or more, the values in
      \code{style.3panels} are used. If the number of panels is 7 or
      more, the values in \code{style.7panels} are also used.

      Next, if the number of data points (per panel) exceeds
      \code{MANY}, the values in \code{style.MANY} are used.
      If they also exceed \code{VERYMANY}, the values in
      \code{style.VERYMANY} are used \emph{in addition} to those already
      set.
    }
    \item{\code{max.panels}}{
      sets the maximum number of panels to show on one page.
    }
    \item{\code{disc.levels}, \code{shingle.overlap}}{
      default number of levels to use when making shingles or
      factors. This can be over-ridden by specifying \code{nLevels} in
      the \code{spec} argument.

      \code{shingle.overlap} sets the amount of overlap for shingles;
      passed to \code{\link[lattice]{equal.count}}.
    }
    \item{\code{catch.errors}}{
      in the default case (TRUE), most errors are caught and displayed
      to the user in a dialog box. Set to FALSE to avoid the
      \code{tryCatch} block and thus allow debugging.
    }
  }

}
\seealso{ \code{\link{latticist}} }
\examples{
str(latticist.options())
}
\keyword{ programming }

