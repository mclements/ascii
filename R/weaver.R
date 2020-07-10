## Adapted from utils package code by Friedrich Leisch

weaver <- function()
{
    list(setup = weaverLatexSetup,
         runcode = weaverRuncode,
         writedoc = RweaveLatexWritedoc,
         finish = weaverLatexFinish,
         checkopts = RweaveLatexOptions)
}

weaverLatexSetup <-
    function(file, syntax,
             output=NULL, quiet=FALSE, debug=FALSE, echo=TRUE,
             eval=TRUE, keep.source=FALSE, split=FALSE, stylepath=TRUE,
             pdf=TRUE, eps=TRUE, use.cache=TRUE)
{
    if (!quiet)
      cat("Working dir:", getwd(), "\n")
    log_debug(paste("Working dir:", getwd()))
    res <- RweaveLatexSetup(file, syntax, output=output, quiet=quiet,
                            debug=debug, echo=echo, eval=eval,
                            keep.source=keep.source,
                            split=split, stylepath=stylepath, pdf=pdf,
                            eps=eps)
    res$options[["use.cache"]] <- use.cache
    res$options[["cache"]] <- FALSE
    ## to be on the safe side: see if defaults pass the check
    res$options <- RweaveLatexOptions(res$options)
    res
}


resetStorage <- function(fun) {
    storage <- environment(fun)
    storage[["hashDeps"]] <- new.env(parent=emptyenv())
    storage[["sym2hash"]] <- new.env(parent=emptyenv())
}


weaverRemoveOrphans <- function(object, options) {
    if(!options$use.cache || !options$cache)
      return(NULL)
    chunk <- options$label
    cachedir <- file.path(get_cache_dir(CACHE_DIR),
                          get_chunk_id(chunk, options$chunknr))
    curhashes <- sort(ls(environment(cache_expr)$hashDeps))
    expPat1 <- paste(".*\\", CACHE_EXT, "$", sep="")
    expPat2 <- paste("\\", CACHE_EXT, sep="")
    hashfiles <- list.files(cachedir, pattern=expPat1)
    hashfiles <- sort(sub(expPat2, "", hashfiles))
    orphans <- hashfiles[!hashfiles %in% curhashes]
    if (length(orphans)) {
        if (!object$quiet)
          cat("     Removing orphaned cache files:\n")
        for (orph in orphans) {
            if (!object$quiet)
              cat(paste("       ", orph, ".RData", sep=""), "\n")
            orph <- paste(cachedir, "/", orph, CACHE_EXT, sep="")
            tryCatch(file.remove(orph), error=function(e) NULL)
        }
    }
}


weaverLatexFinish <- function(object, error=FALSE) {
    resetStorage(cache_expr)
    RweaveLatexFinish(object, error)
}


weaverRuncode <- makeRweaveLatexCodeRunner(evalFunc=weaverEvalWithOpt)

## FIXME: we used to call weaverRemoveOrphans inside the copy/pasted
## Runcode.  Now that we don't have this we either need to ask for a
## hook to be added or see if the work can be done in the finish step.
##
## Also, with currnet, there is no nice way to decide whether we should
## be quiet or not in the eval process.  One option is to pass object to
## evalFunc as well.

weaverEvalWithOpt <- function (expr, options, quiet=FALSE){
    if(options$eval){
        label <- options$label
        chunkNum <- options$chunknr
        if(options$use.cache && options$cache) {
            expr <- substitute(cache_expr(e, chunk.name=n, chunk.num=i,
                                          quiet=q),
                               list(e=expr, n=label, i=chunkNum, q=quiet))
        }
        res <- try(withVisible(eval(expr, .GlobalEnv)), silent=TRUE)
        if(inherits(res, "try-error")) return(res)
        if(options$print | (options$term & res$visible))
            print(res$value)
    }
    return(res)
}
