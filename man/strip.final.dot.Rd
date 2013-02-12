\name{strip.final.dot}
\alias{strip.final.dot}
\title{Strips a text of any final dots.}
\usage{
  strip.final.dot(txt = "")
}
\arguments{
  \item{txt}{a text string}
}
\value{
  a text string without final dot
}
\description{
  This is a small helper function to prepare the final
  exsic table.
}
\details{
  Intended for eliminating any duplicated dots in the final
  document due to final dots usually in the 'locnotes'
  (location notes) column.
}
\author{
  Reinhard Simon
}
\seealso{
  Other helper: \code{\link{coll.cite}},
  \code{\link{is.exsic}}, \code{\link{is.format}},
  \code{\link{is.sortfilter}}
}

