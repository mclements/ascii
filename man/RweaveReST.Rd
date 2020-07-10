\name{weaverReST}
\alias{RtangleReST}
\alias{RweaveReST}
\alias{RweaveReSTFinish}
\alias{RweaveReSTOptions}
\alias{RweaveReSTRuncode}
\alias{RweaveReSTSetup}
\alias{RweaveReSTWritedoc}
\alias{weaverReST}
\alias{weaverReSTSetup}
\title{A driver to parse rest noweb files with Sweave tool}
\usage{
RweaveReSTSetup(file, syntax, output = NULL, quiet = FALSE,
debug = FALSE, extension = "rst", backend = "docutils, sphinx, ...",
openSchunk = ".. code-block:: r\n", closeSchunk = "\n\n",
openSinput = "", closeSinput = "", openSoutput = "\n",
closeSoutput = "", indent = "  ", openInclude = ".. include::",
closeInclude = ".rst", openFig = ".. image:: ", closeFig = "", ...)

weaverReSTSetup(file, syntax, output = NULL, quiet = FALSE,
debug = FALSE, extension = "rst", backend = "docutils, sphinx, ...",
openSchunk = ".. code-block:: r\n", closeSchunk = "\n\n",
openSinput = "", closeSinput = "", openSoutput = "\n",
closeSoutput = "", indent = "  ", openInclude = ".. include::",
closeInclude = ".rst", openFig = ".. image:: ", closeFig = "", use.cache = TRUE, ...)
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
.rst is produced (as eventuals files for graphs).
}
\description{
A driver to parse rest noweb files with Sweave tool

A driver to parse rest noweb files with Sweave tool

A driver to parse sphinx noweb files with Sweave tool
This driver parses sphinx files containing R code and replace pieces of
code with their output.

RweaveReSTSetup

weaverReSTSetup

weaverReST
}
\note{
In order to work properly, noweb codes have to be located at the
beginning of a line (no indentation).

Compare with RweaveLatex driver, RweaveReST provides one new option :
\code{format} to choose the format of figure that will be inserted in the
final document.

In addition, \code{cache} option from \code{weaver}
package is also available with \code{weaverReST} driver.

A wrapper for \code{Sweave} can be used, named \code{ReST}.
}
\examples{
\dontrun{
ReST("file.Rnw")
}
}
\author{
David Hajage

David Hajage

David Hajage \email{dhajage@gmail.com}
}
\seealso{
\code{\link[utils]{Sweave}}, \code{\link[ascii]{ReST}}
}
\keyword{file}
\keyword{internal}
\keyword{IO}

