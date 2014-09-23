## latticist: a Lattice-based exploratory visualisation GUI
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

latticeStyleToBasePar <- function() {
    opar <- par(no.readonly = TRUE)
    trellispar <- trellis.par.get()
    ## palette() has no alpha setting; need to apply it to col
    setAlpha <- function(col, alpha) {
        crgb <- col2rgb(col, alpha = TRUE)
        crgb[4] <- alpha * 255
        rgb(crgb[1], crgb[2], crgb[3], crgb[4], maxColorValue = 255)
    }
    with(trellispar, {
        col <- plot.symbol$col
        alpha <- plot.symbol$alpha
        col <- setAlpha(col, alpha)
        cols <- c(col, superpose.symbol$col[-1])
        palette(cols)
        par(bg = background$col,
            fg = axis.line$col)
        par(pch = plot.symbol$pch,
            lty = plot.line$lty,
            lwd = plot.line$lwd,
            cex = plot.symbol$cex,
            ps = fontsize$text,
            col = axis.line$col,
            col.axis = axis.line$col,
            cex.axis = axis.text$cex,
            col.main = par.main.text$col,
            cex.main = par.main.text$cex,
            col.sub = par.sub.text$col,
            cex.sub = par.sub.text$cex,
            col.lab = par.xlab.text$col,
            cex.lab = par.xlab.text$cex,
            lheight = add.text$lineheight)
        if (!is.null(grid.pars$fontfamily))
            par(family = grid.pars$fontfamily)
    })
    invisible(opar)
}

