% Generated by roxygen2 (4.0.2): do not edit by hand
\name{coll.cite}
\alias{coll.cite}
\title{Constructs a final citation of collector names.}
\usage{
coll.cite(collector, addcoll = "", initials = c("none", "before"),
  dots = TRUE)
}
\arguments{
\item{collector}{a name}

\item{addcoll}{a name or list of names; may be empty}

\item{initials}{use 'none' or 'before' to indicate if initials should be used for the citation}

\item{dots}{boolean; should dots be used to separate the initials?}
}
\value{
a text
}
\description{
Uses two of the BRAHMS fields (collector, addcoll). Expects names in both fields to conform to the
pattern: "Lastname, A" where A are the list of initials without dots; if no comma present the function
assummes all words to be part of the lastname like in: van de Bergh.
}
\details{
The 'collector' field must have only one name; the 'addcoll' field may have several names separated by ; or
it may be empty. Like in the case of 'collector' a missing comma indicates that all words till the next ;
are part of the last name.

Two collector names will be separated by '&'; more than two names will result in a citation with just
the (principal) collectors name followed by 'et al.'.
}
\note{
The function does not yet handle any other special cases like including an 'obligatory' second
author as permitted in some specimen citations when the author of the treatment is also a secondary
collector.

In any case, using this function or editing the final citation manually, the final citation should
be in a column called 'collcite'.
}
\author{
Reinhard Simon
}
\seealso{
Other helper: \code{\link{is.exsic}};
  \code{\link{is.format}}; \code{\link{is.sortfilter}};
  \code{\link{strip.final.dot}}
}

