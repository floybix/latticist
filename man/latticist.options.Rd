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
  These functions are direct copies of the lattice equivalents:
  see \code{\link[lattice]{lattice.options}}.

  The available options can be seen with \code{str(latticist.options())}.
}
\seealso{ \code{\link{latticist}} }
\examples{
str(latticist.options())
}
\keyword{ programming }

