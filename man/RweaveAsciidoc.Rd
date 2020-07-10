\name{weaverAsciidoc}
\alias{RtangleAsciidoc}
\alias{RweaveAsciidoc}
\alias{RweaveAsciidocFinish}
\alias{RweaveAsciidocOptions}
\alias{RweaveAsciidocRuncode}
\alias{RweaveAsciidocSetup}
\alias{RweaveAsciidocWritedoc}
\alias{weaverAsciidoc}
\alias{weaverAsciiSetup}
\title{A driver to parse asciidoc noweb files with Sweave tool}
\usage{
weaverAsciiSetup(file, syntax, output = NULL, quiet = FALSE, debug = FALSE,
extension = "txt", backend = "asciidoc", openSchunk = "----",
closeSchunk = "\n----\n", openSinput = "", closeSinput = "",
openSoutput = "\n", closeSoutput = "", indent = "", openInclude = "include::",
closeInclude = ".txt[]", openFig = "image::", closeFig = "[]", use.cache = TRUE, ...)
}
\arguments{
  \item{use.cache}{use.cache}
}
\value{
None value is returned. From a .Rnw noweb file, the corresponding
.txt is produced (as eventuals files for graphs).
}
\description{
A driver to parse asciidoc noweb files with Sweave tool

A driver to parse asciidoc noweb files with Sweave tool
This driver parses asciidoc files containing R code and replace pieces of
code with their output.

weaverAsciidocSetup

weaverAsciidoc
}
\note{
In order to work properly, noweb codes have to be located at the
beginning of a line (no indentation).

Compare with RweaveLatex driver, RweaveAsciidoc provides one new option :
\code{format} to choose the format of figure that will be inserted in the
final document.

In addition, \code{cache} option from \code{weaver}
package is also available with \code{weaverAsciidoc} driver.

A wrapper for \code{Sweave} can be used, named \code{Asciidoc}.
}
\examples{
\dontrun{
Asciidoc("file.Rnw")
}
}
\author{
David Hajage

David Hajage \email{dhajage@gmail.com}
}
\seealso{
\code{\link[utils]{Sweave}}, \code{\link[ascii]{Asciidoc}}
}
\keyword{file}
\keyword{internal}
\keyword{IO}

