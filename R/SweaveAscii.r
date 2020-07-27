##' Sweave wrappers
##'
##' @param file Name of Sweave source file.
##' @param driver Sweave driver
##' @param syntax Sweave syntax
##' @param encoding Encoding
##' @param ... Further arguments passed to the driver's setup function.
##' @author David Hajage \email{dhajage@@gmail.com}
##' @seealso \code{\link{Sweave}}
##' @rdname sweave-wrapper
##' @export
##' @import utils
##' @keywords IO file
Asciidoc <- function(file, driver=RweaveAsciidoc, syntax=SweaveSyntaxNoweb,
                     encoding="", ...)
    Sweave(file, driver, syntax, encoding, ...)

##' @rdname sweave-wrapper
##' @export
##' @import utils
##' @keywords IO file
T2t <- function(file, driver=RweaveT2t, syntax=SweaveSyntaxNoweb,
                encoding="", ...)
    Sweave(file, driver, syntax, encoding, ...)

##' @rdname sweave-wrapper
##' @export
##' @import utils
##' @keywords IO file
ReST <- function(file, driver=RweaveReST, syntax=SweaveSyntaxNoweb,
                 encoding="", ...)
    Sweave(file, driver, syntax, encoding, ...)

##' @rdname sweave-wrapper
##' @export
##' @import utils
##' @examples
##' \dontrun{
##' testfile <- system.file("examples", "Org-test-1.nw", package = "ascii")
##'
##' ## enforce par(ask = FALSE)
##' options(device.ask.default = FALSE)
##'
##' ## create an org file - in the current working directory, getwd():
##' Org(testfile)
##' Org(testfile, driver=weaverOrg)
##'
##' ## This can be edited in and exported from Org Mode
##' }
##' @keywords IO file
Org <- function(file, driver=RweaveOrg, syntax=SweaveSyntaxNoweb,
                encoding="", ...)
    Sweave(file, driver, syntax, encoding, ...)

##' @rdname sweave-wrapper
##' @export
##' @import utils
##' @keywords IO file
Textile <- function(file, driver=RweaveTextile, syntax=SweaveSyntaxNoweb,
                    encoding="", ...)
    Sweave(file, driver, syntax, encoding, ...)

##' @rdname sweave-wrapper
##' @export
##' @import utils
##' @keywords IO file
Pandoc <- function(file, driver=RweavePandoc, syntax=SweaveSyntaxNoweb,
                   encoding="", ...)
    Sweave(file, driver, syntax, encoding, ...)
