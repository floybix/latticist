\name{latticist}
\alias{latticist}
\title{ A Lattice GUI }
\description{
  Interactively explore a data set using \link[lattice]{Lattice} displays.
}
\usage{
latticist(dat,
          spec = list(),
          reorder.levels = !is.table(dat),
          ...,
          use.playwith = latticist.getOption("use.playwith"))
}
\arguments{
  \item{dat}{ a \code{data frame} (with numeric and/or categorical variables),
    or a \code{table}. }
  \item{spec}{ a list specifying the initial latticist display.
    See \code{\link{latticistCompose}}. }
  \item{reorder.levels}{ if \code{TRUE}, change the ordering of levels of factor
    variables so they are in order of frequency. Note that this does not
    change the data, only its internal representation: see
    \code{\link{reorder}}. This is recommended for effective graphic
    displays. Factor variables of class \code{ordered} are not reordered.
    Therefore you should ensure that any variables whose levels have an inherent
    order are of class \code{ordered}: see \code{\link{as.ordered}}.
    Note: the result of \code{\link{cut}} is by default NOT
    \code{ordered}!
    In addition, numeric variables with discrete values in {0, 1, -1}
    are converted to factors.
  }
  \item{use.playwith}{
    whether to launch as a toolbar for \code{playwith} (requires the
    \pkg{playwith} package), or a generic \pkg{gWidgets} interface.
  }
  \item{\dots}{
    further arguments specific to the interface type. For the \code{playwith}
    interface, these are passed to \code{\link[playwith]{playwith}}. For
    the \code{gWidgets} interface, these are:
    \describe{
      \item{\code{width = 480, height = 480}}{
	size of the graphics device in pixels.
      }
      \item{\code{pointsize = 12}}{
	text size on the graphics device.
      }
    }
  }
}
\details{
  Latticist is a graphical user interface for exploratory visualisation.
  It is primarily an interface to the Lattice graphics system  (from the
  \pkg{lattice} package), but also produces displays from the \pkg{vcd}
  package for categorical data.

  Given a multivariate dataset (either a \code{data frame} or a
  \code{table}), Latticist attempts to produce useful displays based on
  the properties of the data. The user chooses variables or expressions
  for the plot axes, for grouping, conditioning and subsetting. Some
  hypervariate displays are also available.

  A minimal graphical user interface is available, built on the
  \pkg{gWidgets} package. This requires one of the "toolkit
  implementations" to installed:
  \pkg{gWidgetstcltk}, \pkg{gWidgetsRGtk2} or \pkg{gWidgetsrJava}.

  The fastest way to start, without any external system requirements, is
  to \code{install.packages("gWidgetstcltk")}. However, that does not
  support an embedded graphics device, so the plots will appear in a
  separate window.

  Alternatively, Latticist can be run as a toolbar extension to
  \code{\link[playwith]{playwith}}.
  This brings many extra features, such as dynamic zooming, identifying
  data points, linked brushing, etc. Note that the \pkg{playwith}
  package requires \pkg{RGtk2} and, therefore, the \emph{GTK+} libraries.

  To enable all types of graphic displays, install the \pkg{deldir}
  package, as well as \pkg{hexbin}, a BioConductor package, which can be
  installed as follows:

  \code{source("http://bioconductor.org/biocLite.R")}

  \code{biocLite("hexbin")}
}
\value{
  The \pkg{playwith} method invisibly returns the \code{playState}
  object representing the plot window. The \pkg{gWidgets} method
  invisibly returns the \code{gwindow} object.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\references{
  For an excellent introduction to and coverage of Lattice:

Sarkar, Deepayan (2008) "Lattice: Multivariate Data Visualization with
R", Springer. \url{http://lmdvr.r-forge.r-project.org/}
}
\seealso{
  \code{\link{latticistCompose}},
  \link[lattice]{Lattice},
  \code{lattice.demo} in the \pkg{TeachingDemos} package. }
\examples{
if (interactive()) {
options(device.ask.default = FALSE)
askNext <- function()
    stopifnot(gconfirm("Press OK for next example..."))

## data.frame example

## CO2 dataset: see ?CO2
## From an experiment on the cold tolerance of a grass species.
latticist(CO2)

## The following sequence would normally be interactive:
askNext()
latticist(CO2, spec = list(yvar = "uptake"))
askNext()
latticist(CO2, spec = list(yvar = "uptake", xvar = "conc"))
askNext()
latticist(CO2, spec = list(yvar = "uptake", xvar = "conc",
                            groups = "Treatment"))
askNext()
latticist(CO2, spec = list(yvar = "uptake", xvar = "conc",
                            groups = "Treatment", cond = "Type"))
askNext()
latticist(CO2, spec = list(yvar = "uptake", xvar = "conc",
                            cond = "Plant"))
askNext()

## table example

## Titanic dataset: Survival of passengers on the Titanic
## summarised by economic status (class), sex, age and survival.
latticist(Titanic)

}
}
\keyword{ hplot }
\keyword{ iplot }
\keyword{ dynamic }
