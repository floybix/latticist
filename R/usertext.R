## latticist: a Lattice-based exploratory visualisation GUI
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

panel.usertext <-
    function(x, y = NULL, labels = seq_along(x), col = user.text$col,
             alpha = user.text$alpha, cex = user.text$cex, srt = 0, lineheight = user.text$lineheight,
             font = user.text$font, fontfamily = user.text$fontfamily, fontface = user.text$fontface,
             adj = c(0.5, 0.5), pos = NULL, offset = 0.5, ...)
{
    user.text <- current.user.text()
    panel.text(x, y, labels, col = col, alpha = alpha, cex = cex, srt = srt,
               lineheight = lineheight, font = font, fontfamily = fontfamily,
               fontface = fontface, adj = adj, pos = pos, offset = offset, ...)
}

current.user.text <- function() {
    user.text <- trellis.par.get("user.text")
    if (is.null(eval(user.text))) {
        user.text <- trellis.par.get("add.text")
    }
    user.text
}

