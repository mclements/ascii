## hack to deal with unexported objects

.SweaveValidFilenameRegexp <- "^[[:alnum:]/#+_-]+$"

survmean <- function (x, scale = 1, rmean) 
{
    if (!is.null(x$start.time)) 
        start.time <- x$start.time
    else start.time <- min(0, x$time)
    pfun <- function(nused, time, surv, n.risk, n.event, lower, 
        upper, start.time, end.time) {
        minmin <- function(y, x) {
            tolerance <- .Machine$double.eps^0.5
            keep <- (!is.na(y) & y < (0.5 + tolerance))
            if (!any(keep)) 
                NA
            else {
                x <- x[keep]
                y <- y[keep]
                if (abs(y[1] - 0.5) < tolerance && any(y < y[1])) 
                  (x[1] + x[min(which(y < y[1]))])/2
                else x[1]
            }
        }
        if (!is.na(end.time)) {
            hh <- ifelse((n.risk - n.event) == 0, 0, n.event/(n.risk * 
                (n.risk - n.event)))
            keep <- which(time <= end.time)
            if (length(keep) == 0) {
                temptime <- end.time
                tempsurv <- 1
                hh <- 0
            }
            else {
                temptime <- c(time[keep], end.time)
                tempsurv <- c(surv[keep], surv[max(keep)])
                hh <- c(hh[keep], 0)
            }
            n <- length(temptime)
            delta <- diff(c(start.time, temptime))
            rectangles <- delta * c(1, tempsurv[-n])
            varmean <- sum(cumsum(rev(rectangles[-1]))^2 * rev(hh)[-1])
            mean <- sum(rectangles) + start.time
        }
        else {
            mean <- 0
            varmean <- 0
        }
        med <- minmin(surv, time)
        if (!is.null(upper)) {
            upper <- minmin(upper, time)
            lower <- minmin(lower, time)
            c(nused, max(n.risk), n.risk[1], sum(n.event), sum(mean), 
                sqrt(varmean), med, lower, upper)
        }
        else c(nused, max(n.risk), n.risk[1], sum(n.event), sum(mean), 
            sqrt(varmean), med, 0, 0)
    }
    stime <- x$time/scale
    if (is.numeric(rmean)) 
        rmean <- rmean/scale
    surv <- x$surv
    plab <- c("records", "n.max", "n.start", "events", "*rmean", 
        "*se(rmean)", "median", paste(x$conf.int, c("LCL", "UCL"), 
            sep = ""))
    ncols <- 9
    if (is.matrix(surv) && !is.matrix(x$n.event)) 
        x$n.event <- matrix(rep(x$n.event, ncol(surv)), ncol = ncol(surv))
    if (is.null(x$strata)) {
        if (rmean == "none") 
            end.time <- NA
        else if (is.numeric(rmean)) 
            end.time <- rmean
        else end.time <- max(stime)
        if (is.matrix(surv)) {
            out <- matrix(0, ncol(surv), ncols)
            for (i in 1:ncol(surv)) {
                if (is.null(x$conf.int)) 
                  out[i, ] <- pfun(x$n, stime, surv[, i], x$n.risk, 
                    x$n.event[, i], NULL, NULL, start.time, end.time)
                else out[i, ] <- pfun(x$n, stime, surv[, i], 
                  x$n.risk, x$n.event[, i], x$lower[, i], x$upper[, 
                    i], start.time, end.time)
            }
            dimnames(out) <- list(dimnames(surv)[[2]], plab)
        }
        else {
            out <- matrix(pfun(x$n, stime, surv, x$n.risk, x$n.event, 
                x$lower, x$upper, start.time, end.time), nrow = 1)
            dimnames(out) <- list(NULL, plab)
        }
    }
    else {
        nstrat <- length(x$strata)
        stemp <- rep(1:nstrat, x$strata)
        last.time <- (rev(stime))[match(1:nstrat, rev(stemp))]
        if (rmean == "none") 
            end.time <- rep(NA, nstrat)
        else if (is.numeric(rmean)) 
            end.time <- rep(rmean, nstrat)
        else if (rmean == "common") 
            end.time <- rep(median(last.time), nstrat)
        else end.time <- last.time
        if (is.matrix(surv)) {
            ns <- ncol(surv)
            out <- matrix(0, nstrat * ns, ncols)
            if (is.null(dimnames(surv)[[2]])) 
                dimnames(out) <- list(rep(names(x$strata), ns), 
                  plab)
            else {
                cname <- outer(names(x$strata), dimnames(surv)[[2]], 
                  paste, sep = ", ")
                dimnames(out) <- list(c(cname), plab)
            }
            k <- 0
            for (j in 1:ns) {
                for (i in 1:nstrat) {
                  who <- (stemp == i)
                  k <- k + 1
                  if (is.null(x$lower)) 
                    out[k, ] <- pfun(x$n[i], stime[who], surv[who, 
                      j], x$n.risk[who], x$n.event[who, j], NULL, 
                      NULL, start.time, end.time[i])
                  else out[k, ] <- pfun(x$n[i], stime[who], surv[who, 
                    j], x$n.risk[who], x$n.event[who, j], x$lower[who, 
                    j], x$upper[who, j], start.time, end.time[i])
                }
            }
        }
        else {
            out <- matrix(0, nstrat, ncols)
            dimnames(out) <- list(names(x$strata), plab)
            for (i in 1:nstrat) {
                who <- (stemp == i)
                if (is.null(x$lower)) 
                  out[i, ] <- pfun(x$n[i], stime[who], surv[who], 
                    x$n.risk[who], x$n.event[who], NULL, NULL, 
                    start.time, end.time[i])
                else out[i, ] <- pfun(x$n[i], stime[who], surv[who], 
                  x$n.risk[who], x$n.event[who], x$lower[who], 
                  x$upper[who], start.time, end.time[i])
            }
        }
    }
    if (is.null(x$lower)) 
        out <- out[, 1:7, drop = F]
    if (rmean == "none") 
        out <- out[, -(5:6), drop = F]
    list(matrix = out[, , drop = T], end.time = end.time)
}
environment(survmean) <- environment(survival::coxph)

RtangleFinish <- function (object, error = FALSE) 
{
    if (!is.null(object$output) && object$output >= 3) 
        close(object$output)
    if (length(object$chunkout)) 
        for (con in object$chunkout) close(con)
}
RtangleRuncode <- 
function (object, chunk, options) 
{
    if (!(options$engine %in% c("R", "S"))) 
        return(object)
    chunkprefix <- RweaveChunkPrefix(options)
    if (options$split) {
        if (!grepl(.SweaveValidFilenameRegexp, chunkprefix)) 
            warning("file stem ", sQuote(chunkprefix), " is not portable", 
                call. = FALSE, domain = NA)
        outfile <- paste(chunkprefix, options$engine, sep = ".")
        if (!object$quiet) 
            cat(options$chunknr, ":", outfile, "\n")
        chunkout <- object$chunkout[chunkprefix][[1L]]
        if (is.null(chunkout)) {
            chunkout <- file(outfile, "w")
            if (!is.null(options$label)) 
                object$chunkout[[chunkprefix]] <- chunkout
        }
    }
    else chunkout <- object$output
    showOut <- options$eval || !object$drop.evalFALSE
    if (showOut) {
        annotate <- object$annotate
        if (is.logical(annotate) && annotate) {
            cat("###################################################\n", 
                "### code chunk number ", options$chunknr, ": ", 
                if (!is.null(ol <- options$label)) 
                  ol
                else .RtangleCodeLabel(chunk), if (!options$eval) 
                  " (eval = FALSE)", "\n", "###################################################\n", 
                file = chunkout, sep = "")
        }
        else if (is.function(annotate)) 
            annotate(options, chunk = chunk, output = chunkout)
    }
    hooks <- SweaveHooks(options, run = FALSE)
    for (k in hooks) cat("getOption(\"SweaveHooks\")[[\"", k, 
        "\"]]()\n", file = chunkout, sep = "")
    if (showOut) {
        if (!options$show.line.nos) 
            chunk <- grep("^#line ", chunk, value = TRUE, invert = TRUE)
        if (!options$eval) 
            chunk <- paste("##", chunk)
        cat(chunk, "\n", file = chunkout, sep = "\n")
    }
    if (is.null(options$label) && options$split) 
        close(chunkout)
    object
}
SweaveParseOptions <-
function (text, defaults = list(), check = NULL) 
{
    x <- sub("^[[:space:]]*(.*)", "\\1", text)
    x <- sub("(.*[^[:space:]])[[:space:]]*$", "\\1", x)
    x <- unlist(strsplit(x, "[[:space:]]*,[[:space:]]*"))
    x <- strsplit(x, "[[:space:]]*=[[:space:]]*")
    if (length(x)) {
        if (length(x[[1L]]) == 1L) 
            x[[1L]] <- c("label", x[[1L]])
    }
    else return(defaults)
    if (any(lengths(x) != 2L)) 
        stop(gettextf("parse error or empty option in\n%s", text), 
            domain = NA)
    options <- defaults
    for (k in seq_along(x)) options[[x[[k]][1L]]] <- x[[k]][2L]
    if (!is.null(options[["label"]]) && !is.null(options[["engine"]])) 
        options[["label"]] <- sub(paste0("\\.", options[["engine"]], 
            "$"), "", options[["label"]])
    if (!is.null(check)) 
        check(options)
    else options
}
environment(RtangleFinish) <- environment(RtangleRuncode) <-
    environment(SweaveParseOptions) <- environment(utils::packageDescription)
