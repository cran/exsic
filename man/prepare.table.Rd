\name{prepare.table}
\alias{prepare.table}
\title{Prepare exsic table}
\usage{
  prepare.table(atbl, sortfilter = NULL)
}
\arguments{
  \item{atbl}{a data.frame table}

  \item{sortfilter}{a data frame with two columns (one for
  species and one for genus; maybe NULL)}
}
\value{
  a table in the format expected by the exsic function
}
\description{
  Complements missing minimal columns and missing content.
  If applicable (not null) applies a list of countries and
  species in the given order to sort the table. Countries
  and species not present in a given sortfilter data.frame
  will be filtered out from the underlying table.
}
\author{
  Reinhard Simon
}
\seealso{
  Other main: \code{\link{exsic}},
  \code{\link{index.citations}},
  \code{\link{index.collections}},
  \code{\link{index.countries}},
  \code{\link{index.species}},
  \code{\link{index.species.short}},
  \code{\link{read.exsic}}, \code{\link{section.exsic}},
  \code{\link{write.exsic}}
}

