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
      \item{\code{width = 450, height = 450}}{
	size of the graphics device in pixels.
	Note, these are ignored if not using an embedded device.
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
  Note that \pkg{gWidgetsrJava} is currently broken (as of version 0.0-13).

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
  object representing the plot window. One can close it with
  \code{playDevOff()} or \code{dev.off()}.

  The \pkg{gWidgets} method invisibly returns the \code{gwindow}
  object. One can close it with \code{dispose()}.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\references{
  For an excellent introduction to and coverage of Lattice:

  Sarkar, Deepayan (2008) \dQuote{Lattice: Multivariate Data
    Visualization with R}, Springer.  ISBN: 978-0-387-75968-5
  \url{http://lmdvr.r-forge.r-project.org/}

  The mosaic displays and extensions from \pkg{vcd} are well described in:

  David Meyer, Achim Zeileis, and Kurt Hornik (2006).
  \dQuote{The Strucplot Framework: Visualizing Multi-Way Contingency
    Tables with vcd}. \emph{Journal of Statistical Software}, 17(3),
  1-48. \url{http://www.jstatsoft.org/v17/i03/}
}
\seealso{
  \code{\link{latticistCompose}},
  \code{\link{latticist.options}},
  \link[lattice]{Lattice},
  \code{lattice.demo} in the \pkg{TeachingDemos} package. }
\examples{
if (interactive()) {
options(device.ask.default = FALSE)

\dontrun{
## data frame example:
latticist(iris)

## table example:
latticist(Titanic, spec = list(groups = "Survived"))
}

## The GUI comes in three flavours:

if (require("gWidgetstcltk") &&
    isTRUE(gconfirm("Show tcl/tk-based GUI?")))
{
   options(guiToolkit = "tcltk")
   latticist(CO2, use.playwith = FALSE)
}

if (require("gWidgetsRGtk2") &&
    isTRUE(gconfirm("Show RGtk2-based GUI?")))
{
   options(guiToolkit = "RGtk2")
   latticist(CO2, use.playwith = FALSE)
}

if (require("playwith") &&
    isTRUE(gconfirm("Show playwith-based GUI?")))
{
   latticist(CO2, use.playwith = TRUE)
}

}
}
\keyword{ hplot }
\keyword{ iplot }
\keyword{ dynamic }
