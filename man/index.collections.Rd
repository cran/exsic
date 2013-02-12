\name{index.collections}
\alias{index.collections}
\title{Creates the index of collectors and their specimes.}
\usage{
  index.collections(atable = NULL, format = format.SBMG)
}
\arguments{
  \item{atable}{a table with exsiccatae records}

  \item{format}{a format.exsic table (check with
  is.format)}
}
\value{
  a string in markdown format
}
\description{
  Should only be used within the template file.
}
\author{
  Reinhard Simon
}
\seealso{
  Other main: \code{\link{exsic}},
  \code{\link{index.citations}},
  \code{\link{index.countries}},
  \code{\link{index.species}},
  \code{\link{index.species.short}},
  \code{\link{prepare.table}}, \code{\link{read.exsic}},
  \code{\link{section.exsic}}, \code{\link{write.exsic}}
}

