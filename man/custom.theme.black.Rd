\name{custom.theme.black}
\alias{custom.theme.black}
\title{ Alternative Lattice themes }
\description{
  Alternative Lattice themes.
}
\usage{
custom.theme.black(symbol = brewer.pal(n = 8, name = "Set2"),
                   fill = brewer.pal(n = 8, name = "Set2"),
                   region = rev(brewer.pal(n = 9, name = "YlOrRd")),
                   reference = "#444444", bg = "black", fg = "white",
                   etc = TRUE)
}
\arguments{
  \item{symbol}{ colors for points and lines. }
  \item{fill}{ colors for polygons. }
  \item{region}{ color ramp for continuous regions. }
  \item{reference}{ color of reference lines. }
  \item{bg}{ background color. }
  \item{fg}{ foreground color. }
  \item{etc}{ \code{TRUE} to set extra graphical parameters designed for
               black backgrounds. }
}
\details{
  This is a wrapper around \code{\link[latticeExtra]{custom.theme}}
  with different defaults. It can be used as a Lattice themes.

  The \code{etc} argument sets:
  \itemize{
    \item grey strips;
    \item solid plot symbols;
    \item translucent plot symbols (alpha = 0.5);
    \item thick lines;
    \item no borders on polygons.
  }

  These settings can be modified by editing the resulting list. An easy
  way to do that is via \code{\link[lattice]{simpleTheme}} (see
  examples).
}
\value{
  a list of settings suitable for passing to \code{trellis.par.set}.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link[latticeExtra]{custom.theme}},
  \code{\link[lattice]{simpleTheme}} }
\examples{
opar <- trellis.par.get()

trellis.par.set(custom.theme.black())
latticeStyleDemo()

## make changes to the theme
myTheme <- modifyList(custom.theme.black(),
                      simpleTheme(alpha.points = 0.1))
myTheme$add.line$lty <- 3
## now apply myTheme, or just:
trellis.par.set(simpleTheme(alpha.points = 0.1))
trellis.par.set(add.line = list(lty = 3))
latticeStyleDemo()

trellis.par.set(opar)
}
\keyword{ color }
