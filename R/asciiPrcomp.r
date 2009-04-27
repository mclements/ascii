ascii.prcomp <- function (x, include.rownames = TRUE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = "", ...){
    y <- data.frame(x$rotation,check.names=FALSE)
    obj <- asciiDataFrame$new(y, include.rownames,
         include.colnames, format,
         digits, decimal.mark, na.print,
         caption, caption.level = caption.level, width, frame, grid,
         valign, header, footer, align,
         col.width, style)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}

ascii.summary.prcomp <- function (x, include.rownames = TRUE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = "", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = "", ...){
    x <- data.frame(x$importance,check.names=FALSE)
    obj <- asciiDataFrame$new(x, include.rownames,
         include.colnames, format,
         digits, decimal.mark, na.print,
         caption, caption.level, width, frame, grid,
         valign, header, footer, align,
         col.width, style)
    class(obj) <- c("ascii", "proto", "environment")
    return(obj)
}
