ascii.table <- function (x, include.rownames = TRUE, include.colnames = TRUE, format = "f", digits = 2, decimal.mark = ".", na.print = "", caption = "", caption.level = ".", width = 0, frame = "", grid = "", valign = "", header = TRUE, footer = FALSE, align = "", col.width = 1, style = "", cgroup = NULL, n.cgroup = NULL, calign = "", cvalign = "", cstyle = "", rgroup = NULL, n.rgroup = NULL, ralign = "", rvalign = "", rstyle = "", ...){

  if (length(dim(x)) == 1 | is.null(dim(x))) {
    y <- as.data.frame(t(unclass(x)))
    names(y) <- names(x)
  }
  if (length(dim(x)) == 2) {
    y <- unclass(x)
    if (length(unique(rownames(y))) < nrow(y)) rownames(y) <- 1:nrow(y)
    y <- as.data.frame(y)
  }
  if (length(dim(x)) > 2)  y <- as.data.frame(x)

  obj <- ascii(x = y, include.rownames = include.rownames,
      include.colnames = include.colnames, format = format,
      digits = digits, decimal.mark = decimal.mark, na.print = na.print,
      caption = caption, caption.level = caption.level, width = width, frame = frame, grid = grid,
      valign = valign, header = header, footer = footer, align = align,
      col.width = col.width, style = style, cgroup = cgroup, n.cgroup = n.cgroup, calign = calign,
         cvalign = cvalign, cstyle = cstyle, rgroup = rgroup, n.rgroup = n.rgroup, ralign = ralign,
         rvalign = rvalign, rstyle = rstyle)
  class(obj) <- c("ascii", "proto", "environment")
  return(obj)
}