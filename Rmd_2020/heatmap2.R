heatmap2 <- function (x, Rowv = NULL, Colv = if (symm) "Rowv" else NULL, 
          distfun = dist, hclustfun = hclust, 
          reorderfun = function(d, w) reorder(d, w), add.expr, symm = FALSE, 
          revC = identical(Colv, "Rowv"), scale = c("row", "column", "none"), 
          na.rm = TRUE, margins = c(5, 5), ColSideColors, RowSideColors, cexRow = 0.2 + 
            1/log10(nr), cexCol = 0.2 + 1/log10(nc), labRow = NULL, 
          labCol = NULL, main = NULL, xlab = NULL, ylab = NULL, keep.dendro = FALSE, 
          verbose = getOption("verbose"), ...) 
{
  di <- dim(x)
  nr <- di[1L]
  nc <- di[2L]
  if (nr <= 1 || nc <= 1) 
    stop("'x' must have at least 2 rows and 2 columns")
  if (!is.numeric(margins) || length(margins) != 2L) 
    stop("'margins' must be a numeric vector of length 2")
  doRdend <- !identical(Rowv, NA)
  doCdend <- !identical(Colv, NA)
  if (!doRdend && identical(Colv, "Rowv")) 
    doCdend <- FALSE
  if (is.null(Rowv)) 
    Rowv <- rowMeans(x, na.rm = na.rm)
  if (is.null(Colv)) 
    Colv <- colMeans(x, na.rm = na.rm)
  rowInd <- 1L:nr
  colInd <- 1L:nc
  x <- x[rowInd, colInd]
  if (!symm || scale != "none") 
    x <- t(x)
  iy <- 1L:nr
  labCol <- colnames(x)
  labRow <- rownames(x)
  image(1L:nc, 1L:nr, x, xlim = 0.5 + c(0, nc), ylim = 0.5 + 
          c(0, nr), axes = FALSE, xlab = "", ylab = "", ...)
  axis(1, 1L:nc, labels = labCol, las = 2, line = -0.5, tick = 0, 
       cex.axis = cexCol)
  if (!is.null(xlab)) 
    mtext(xlab, side = 1, line = margins[1L] - 1.25)
  axis(4, iy, labels = labRow, las = 2, line = -0.5, tick = 0, 
       cex.axis = cexRow)
  if (!is.null(ylab)) 
    mtext(ylab, side = 4, line = margins[2L] - 1.25)
  if (!missing(add.expr)) 
    eval.parent(substitute(add.expr))
  par(mar = c(margins[1L], 0, 0, 0))
  
}