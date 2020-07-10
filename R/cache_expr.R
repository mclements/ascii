weaver_opts <- new.env(parent=emptyenv())
weaver_opts$DEBUG <- TRUE
weaver_opts$LOG <- "weaver_debug_log.txt"
CACHE_DIR <- "r_env_cache"
CACHE_EXT <- ".RData"

log_debug <- function(msg) {
    if (!weaver_opts$DEBUG)
      return(FALSE)
    log <- file(weaver_opts$LOG, open="a")
    sink(file=log, append=FALSE)
##     now <- format(Sys.time(), "%d-%m-%Y-%H:%M:%S")
##     cat(now, "\n")
    print(msg)
    sink(NULL)
    close(log)
}

expr_printer <- function(expr_text, max.char=30) {
    ## expr_text is the result of deparse(substitute(expr))
    expr_text <- paste(expr_text, collapse="")
    fmt <- paste("      %", max.char, "s", sep="")
    expr_text <- sprintf(fmt, substr(expr_text, 1, max.char))
    cat(expr_text, file=stderr())
}


findDeps <- function(sexpr) {
    ## Return a character vector of previously defined symbols
    ## upon which this expression depends.
    ##
    ## We try to err on the side of extra dependencies.
    ## We include locals in order to pick up things like
    ## 'b <- b[x]'.
    fake <- function() {NULL} # Need a closure for findGlobals
    body(fake) <- sexpr
    used <- c(codetools::findGlobals(fake, merge=TRUE),
              codetools::findLocals(body(fake)))
    unique(used)
}


get_expr_hash <- function(expr, method=c("md5", "sha1")) {
    ## Return hash value of 'expr' and string containg the
    ## text version of expr that was hashed.
    method <- match.arg(method)
    text <- paste(deparse(expr), collapse=" ")
    hash <- digest::digest(text, serialize=FALSE, algo=method)
    list(hash=hash, text=text)
}


eval_and_cache <- function(sexpr, deps, cacheEnv, cachefile, quiet) {
    if (!quiet)
      cat("  COMPUTING... ", file=stderr())
    log_debug("computing...")
    ## We want to pick up inherited stuff during the eval.  So no
    ## parent=emptyenv().
    eval(sexpr, envir=cacheEnv)
    DEPS <- deps
    SESSION <- sessionInfo()
    save(cacheEnv, DEPS, SESSION, file=cachefile)
    if (!quiet)
      cat("done.\n", file=stderr())
}


load_from_cache_env <- function(fromEnv, toEnv, hash, sym2hash, updated) {
    ## The 'updated' arg is a logical flag.  TRUE indicates that
    ## syms in fromEnv were retrieved from cache but had to be
    ## recomputed because of a dependency mismatch.  This is allows
    ## us to detect second order dependency mismatch where the
    ## expression doesn't change, but we've recomputed.
    syms <- ls(fromEnv)
    for (sym in syms) {
        assign(sym, fromEnv[[sym]], envir=toEnv)
        assign(sym, list(hash=hash, updated=updated), envir=sym2hash)
    }
}


deps_changed <- function(depSyms, sym2hash, oldDeps) {
    changed <- FALSE
    for (sym in depSyms) {
        if (sym2hash[[sym]]$updated) {
            changed <- TRUE
            log_debug("updated flag TRUE, forcing recompute")
            break
        }
        if (sym2hash[[sym]]$hash != oldDeps[sym]) {
            changed <- TRUE
            log_debug("sym <=> hash mismatch, forcing recompute")
            break
        }
    }
    changed
}


get_cache_dir <- function(dir) {
    file.path(dir, getRversion())
}


get_chunk_id <- function(chunk.name, chunk.num) {
    ## Return the chunk ID: For a chunk named "foo" that
    ## is chunk number 5, the ID is: 'foo_5'.  If the chunk
    ## has no name, we use UNNAMED_CHUNK_x and if for some reason
    ## there is no chunk number, we use _UNKNOWN_NUMBER and, in this
    ## case, give a warning message.
    if(is.null(chunk.num) || is.na(chunk.num) || !is.numeric(chunk.num)) {
        chunk.num <- "UNKNOWN_NUMBER"
        warning("invalid chunk number: ", chunk.num, " using ", chunk.num)
    }
    chunk.num <- as.character(chunk.num)
    if (length(chunk.name) == 0 || is.na(chunk.name) || chunk.name == "")
      chunk.name <- "UNNAMED_CHUNK"
    paste(chunk.name, chunk.num, sep="_")
}

## one idea is to put the cache_expr defn in an environment
## persistent_funcs and then access it via get_chache_expr() I suppose
## we could rename the resetStorage => initCacheExpr and have this
## function take a cache.dir arg.

make_cache_expr <- function()
  local({
    sym2hash <- new.env(parent=emptyenv())
    hashDeps <- new.env(parent=emptyenv())

    function(expr, chunk.name, chunk.num, quiet=TRUE, dir=CACHE_DIR) {
        ## Get an expression object that we can pass around
        ## without worrying about evaluation.  Have to take cdr
        ## (i.e., [[1]]) because otherwise we get
        ## expression(foo) instead of foo.
        sexpr <- parse(text=deparse(substitute(expr)))[[1]]

        h <- get_expr_hash(sexpr)
        hash <- h$hash

        log_debug(format(Sys.time(), "%d-%m-%Y-%H:%M:%S"))
        log_debug("EXPRESSION:")
        log_debug(h$text)
        if (!quiet)
          expr_printer(h$text)

        used <- findDeps(sexpr)
        known <- ls(sym2hash)
        usedIdx <- match(used, known, 0)
        used <- known[usedIdx]
        deps <- sapply(used, function(v) sym2hash[[v]]$hash)
        hashDeps[[hash]] <- deps
        log_debug("DEPENDENCIES:")
        log_debug(deps)

        callingEnv <- parent.frame()

        dir <- file.path(get_cache_dir(dir),
                         get_chunk_id(chunk.name, chunk.num))
        cachefile <- file.path(dir, paste(hash, CACHE_EXT, sep=""))
        recompute <- TRUE
        foundCache <- FALSE
        if(file.exists(cachefile)) {
            foundCache <- TRUE
            log_debug(paste("FOUND", cachefile))
            DEPS <- NULL # prevents 'no visible binding' warning
            found <- load(cachefile)
            stopifnot(all(c("cacheEnv", "DEPS", "SESSION") %in% found))
            oldDeps <- DEPS
            depSyms <- names(oldDeps)
            recompute <- deps_changed(depSyms, sym2hash, oldDeps)
            ## FIXME: add session info check here
        }
        if (recompute) {
            dir.create(dir, showWarnings=FALSE, recursive=TRUE)
            cacheEnv <- new.env(parent=.GlobalEnv)
            eval_and_cache(sexpr, deps, cacheEnv, cachefile, quiet)
        } else {
            if (!quiet)
              cat("  CACHE USED\n", file=stderr())
            log_debug("CACHE USED")
        }
        updated <- foundCache && recompute
        load_from_cache_env(cacheEnv, callingEnv, hash, sym2hash, updated)
        log_debug("===========================================")
        invisible(NULL)
    }
})

cache_expr <- make_cache_expr()
