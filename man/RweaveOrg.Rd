\name{weaverOrg}
\alias{RtangleOrg}
\alias{RweaveOrg}
\alias{RweaveOrgFinish}
\alias{RweaveOrgOptions}
\alias{RweaveOrgRuncode}
\alias{RweaveOrgSetup}
\alias{RweaveOrgWritedoc}
\alias{weaverOrg}
\alias{weaverOrgSetup}
\title{A driver to parse org noweb files with Sweave tool}
\usage{
RweaveOrgSetup(file, syntax, output = NULL, quiet = FALSE, debug = FALSE,
extension = "org", backend = "org-mode",
openSchunk = "#+BEGIN_example", closeSchunk = "\n#+END_example\n",
openSinput = "", closeSinput = "", openSoutput = "\n", closeSoutput = "",
indent = "", openInclude = "#+INCLUDE: \"", closeInclude = ".org\"",
openFig = "[[file:", closeFig = "]]", ...)

weaverOrgSetup(file, syntax, output = NULL, quiet = FALSE, debug = FALSE,
extension = "org", backend = "org-mode",
openSchunk = "#+BEGIN_example", closeSchunk = "\n#+END_example\n",
openSinput = "", closeSinput = "", openSoutput = "\n", closeSoutput = "",
indent = "", openInclude = "#+INCLUDE: \"", closeInclude = ".org\"",
openFig = "[[file:", closeFig = "]]", use.cache = TRUE, ...)
}
\arguments{
  \item{trace}{trace}

  \item{dependson}{dependson}

  \item{file}{file}

  \item{syntax}{syntax}

  \item{output}{output}

  \item{quiet}{quite}

  \item{debug}{debug}

  \item{stylepath}{stylepath}

  \item{...}{...}

  \item{use.cache}{use.cache}
}
\value{
None value is returned. From a .Rnw noweb file, the corresponding
.org is produced (as eventuals files for graphs).
}
\description{
A driver to parse org noweb files with Sweave tool

A driver to parse org noweb files with Sweave tool

A driver to parse org noweb files with Sweave tool
This driver parses org files containing R code and replace pieces of code
with their output.

RweaveOrgSetup

weaverOrgSetup

weaverOrg
}
\note{
In order to work properly, noweb codes have to be located at the
beginning of a line (no indentation).

Compare with RweaveLatex driver, RweaveOrg provides one new option :
\code{format} to choose the format of figure that will be inserted in the
final document.

In addition, \code{cache} option from \code{weaver}
package is also available with \code{weaverOrg} driver.

A wrapper for \code{Sweave} can be used, named \code{Org}.
}
\examples{
\dontrun{
Org("file.Rnw")
}
}
\author{
David Hajage

David Hajage

David Hajage \email{dhajage@gmail.com}
}
\seealso{
\code{\link[utils]{Sweave}}, \code{\link{Org}}
}
\keyword{file}
\keyword{internal}
\keyword{IO}

