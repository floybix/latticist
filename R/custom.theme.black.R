## latticist: a Lattice-based exploratory visualisation GUI
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

custom.theme.black <-
    function(symbol = brewer.pal(n = 8, name = "Set2"),
             fill = brewer.pal(n = 8, name = "Set2"),
             region = rev(brewer.pal(n = 9, name = "YlOrRd")),
             reference = "#444444", bg = "black", fg = "white",
             etc = TRUE)
{
    foo <- custom.theme(symbol = symbol, fill = fill, region = region,
                        reference = reference, bg = bg, fg = fg)
    etcList <- list(add.text = list(col = "#eeeeee"),
                    plot.symbol = list(pch = 16, alpha = 0.5),
                    superpose.symbol = list(pch = 16, alpha = 0.5),
                    plot.line = list(lwd = 2),
                    superpose.line = list(lwd = 2),
                    reference.line = list(lwd = 2),
                    add.line = list(lwd = 2),
                    plot.polygon = list(border = "transparent"),
                    superpose.polygon = list(border = "transparent"),
                    strip.background = list(col = grey(3:8/8)),
                    strip.shingle = list(col = grey(2:7/8)))
    if (etc)
        foo <- modifyList(foo, etcList)
    ## need to reset any existing "user.text" entry (usually black)
    ## this seems to be the only way to do it (needs to be eval'd later!)
    foo$user.text <- expression(NULL)
    foo
}

